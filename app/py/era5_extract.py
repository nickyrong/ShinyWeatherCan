"""
era5_extract.py  --  ERA5-Land ARCO point extraction for the ShinyWeatherCan app.

Deployed with the app; called from R via reticulate (see app/functions_era5.R), and
usable as a CLI for testing / as a subprocess fallback.

Opens the geo-chunked ERA5-Land ARCO Zarr stores (ECMWF), reads a single station
point, and aggregates hourly -> daily on the station's LOCAL STANDARD TIME day.

Verified facts baked in (see app/CLAUDE.md):
  - store longitude is -180..180 (auto-detected); pass the ECCC decimal lon directly.
  - t2m in Kelvin (-> degC); tp in metres, ALREADY de-accumulated hourly (-> sum*1000 mm).
  - aggregate on the station's LST day: ERA5 is UTC; ECCC uses fixed LST (no DST), so the
    caller passes lst_offset_hours derived from the station's own ECCC LOCAL vs UTC columns.

Deps: xarray, zarr<3, fsspec, aiohttp (+ pandas).
Auth: the CDS Personal Access Token is supplied by the caller (the app passes the user's
      key via daily_at_point(key=...)); it is never stored on the server. find_key() is a
      LOCAL-DEV fallback only, checking env CDSAPI_KEY -> ~/.cdsapirc.
"""
import os

import numpy as np
import pandas as pd
import xarray as xr

STORES = {
    "t2m": "https://arco.datastores.ecmwf.int/cadl-arco-geo-007/arco/reanalysis_era5_land/sfc-2m-temperature/geoChunked.zarr",
    "tp":  "https://arco.datastores.ecmwf.int/cadl-arco-geo-009/arco/reanalysis_era5_land/sfc-pressure-precipitation/geoChunked.zarr",
}

def find_key():
    """LOCAL-DEV fallback only: env CDSAPI_KEY -> ~/.cdsapirc. In the deployed app the key is
    always passed in from R (daily_at_point(key=...)), so this is not reached on the server."""
    key = os.getenv("CDSAPI_KEY", "").strip()
    if key:
        return key
    rc = os.path.expanduser("~/.cdsapirc")
    if os.path.exists(rc):
        for line in open(rc):
            if line.strip().startswith("key:"):
                return line.split(":", 1)[1].strip()
    raise RuntimeError(
        "No CDS key supplied (checked the CDSAPI_KEY env var and ~/.cdsapirc). "
        "In the app, enter your CDS Personal Access Token when prompted."
    )


def _open(url, key):
    return xr.open_zarr(
        url,
        consolidated=True,
        storage_options={"headers": {"Authorization": f"Bearer {key}"}},
    )


def _haversine_km(lat0, lon0, lats, lons):
    """Great-circle distance (km) from (lat0, lon0) to arrays lats/lons."""
    r = 6371.0
    p0 = np.radians(lat0)
    p1 = np.radians(np.asarray(lats, dtype=float))
    dphi = p1 - p0
    dl = np.radians(np.asarray(lons, dtype=float) - lon0)
    a = np.sin(dphi / 2) ** 2 + np.cos(p0) * np.cos(p1) * np.sin(dl / 2) ** 2
    return 2 * r * np.arcsin(np.sqrt(a))


def _pick_nearest(lat, lon, lats, lons, valid2d):
    """Nearest (lat,lon) grid-cell centre with valid2d True. valid2d is [len(lats), len(lons)].
    Returns (glat, glon, dist_km) or None if no valid cell."""
    ii, jj = np.where(np.asarray(valid2d))
    if ii.size == 0:
        return None
    clat = np.asarray(lats, float)[ii]
    clon = np.asarray(lons, float)[jj]
    d = _haversine_km(lat, lon, clat, clon)
    k = int(np.argmin(d))
    return float(clat[k]), float(clon[k]), float(d[k])


def _resolve_land_cell(ds, var, lat, lonq, station_lon, max_deg=2.0):
    """ERA5-Land is land-only (ocean cells are NaN). Return the (lat, lon, dist_km) of the grid
    cell to actually sample: the nearest cell if it has data, else the nearest VALID (land) cell
    within max_deg. Raises if the station is over water with no land cell in range."""
    da_near = ds[var].sel(latitude=lat, longitude=lonq, method="nearest")
    glat, glon = float(da_near.latitude), float(da_near.longitude)
    nt = int(ds.sizes["time"])
    tsel = sorted({0, nt // 2, nt - 1})   # cheap land test: a few timesteps of one cell
    if bool(da_near.isel(time=tsel).notnull().any()):
        return glat, glon, float(_haversine_km(lat, lonq, glat, glon))
    # nearest cell is ocean -> search outward for the nearest land cell
    for half in (0.5, 1.0, max_deg):
        sub = (ds[var].sel(latitude=slice(glat - half, glat + half),
                           longitude=slice(glon - half, glon + half))
               .isel(time=tsel).load())
        valid = sub.notnull().any(dim="time").transpose("latitude", "longitude")
        hit = _pick_nearest(lat, lonq, valid["latitude"].values, valid["longitude"].values,
                            valid.values)
        if hit is not None:
            return hit
    raise RuntimeError(
        "ERA5-Land (a LAND-only reanalysis) has no valid grid cell within "
        f"{max_deg} deg of the station (lat={lat}, lon={station_lon}): it appears to be over "
        "water, so ERA5-Land infilling is not available for this station.")


def _point_hourly(url, var, lat, lon, key):
    ds = _open(url, key)
    # detect longitude convention (store is -180..180, but be robust)
    q = (lon % 360.0) if float(ds.longitude.max()) > 180.0 else (((lon + 180.0) % 360.0) - 180.0)
    glat, glon, dist_km = _resolve_land_cell(ds, var, lat, q, lon)
    da = ds[var].sel(latitude=glat, longitude=glon, method="nearest")
    s = da.to_series()
    s.name = var
    return s, (glat, glon, dist_km)  # series indexed by the store's time coord (UTC)


def daily_at_point(lat, lon, lst_offset_hours, key=None):
    """Return a daily DataFrame for the nearest ERA5-Land grid point.

    lst_offset_hours: the station's Local Standard Time offset from UTC (e.g. -8 for
        BC/PST). Derive it from the station's ECCC data (LOCAL_DATE - UTC_DATE), NOT a
        tz library. Hours are shifted by this offset so ERA5 days align with ECCC days.

    Columns: date (YYYY-MM-DD str), era5_t2m_min, era5_t2m_mean, era5_t2m_max (degC),
             era5_tp_mm (mm), era5_n_hours, and the constant grid-cell columns era5_grid_lat,
             era5_grid_lon, era5_grid_km (distance from the station to the sampled cell; large
             values flag a coastal/island station snapped to the nearest ERA5-Land cell).
    """
    if key is None:
        key = find_key()
    t2m, grid = _point_hourly(STORES["t2m"], "t2m", lat, lon, key)  # grid = (glat, glon, dist_km)
    tp, _ = _point_hourly(STORES["tp"], "tp", lat, lon, key)
    df = pd.concat([t2m, tp], axis=1).reset_index()
    tcol = df.columns[0]
    df["local"] = pd.to_datetime(df[tcol]) + pd.Timedelta(hours=int(lst_offset_hours))
    df["date"] = df["local"].dt.strftime("%Y-%m-%d")
    df["t2m_C"] = df["t2m"] - 273.15
    df["tp_mm"] = df["tp"] * 1000.0  # tp is de-accumulated hourly -> daily sum below
    daily = df.groupby("date").agg(
        era5_n_hours=("t2m_C", "size"),
        era5_t2m_min=("t2m_C", "min"),
        era5_t2m_mean=("t2m_C", "mean"),
        era5_t2m_max=("t2m_C", "max"),
        era5_tp_mm=("tp_mm", "sum"),
    ).reset_index()
    daily["era5_grid_lat"] = round(grid[0], 3)
    daily["era5_grid_lon"] = round(grid[1], 3)
    daily["era5_grid_km"] = round(grid[2], 1)
    return daily


def _cli():
    import argparse
    ap = argparse.ArgumentParser(description="ERA5-Land ARCO daily extraction at a point")
    ap.add_argument("--lat", type=float, required=True)
    ap.add_argument("--lon", type=float, required=True)
    ap.add_argument("--offset", type=float, required=True, help="station LST offset from UTC, e.g. -8")
    ap.add_argument("--out", default=None, help="CSV path; if omitted prints head/tail")
    a = ap.parse_args()
    daily = daily_at_point(a.lat, a.lon, a.offset)
    if a.out:
        daily.to_csv(a.out, index=False)
        print(f"wrote {a.out}  ({len(daily):,} days, {daily.date.min()}..{daily.date.max()})")
    else:
        print(daily.head().to_string())
        print("...")
        print(daily.tail().to_string())


if __name__ == "__main__":
    _cli()
