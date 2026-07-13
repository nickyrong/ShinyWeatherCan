# ShinyWeatherCan — Canadian Climate Data Portal

## Authors

* [Nick Rong](https://github.com/nickyrong)
* [Nate Smith](https://github.com/WraySmith)
<br/>

## Overview

ShinyWeatherCan is an interactive R/Shiny app for exploring, downloading, and analyzing
historical climate-station data from
[Environment and Climate Change Canada (ECCC)](https://climate.weather.gc.ca/historical_data/search_historic_data_e.html).

Live app: **https://nickrongkp.shinyapps.io/WeatherCan/**

Data are pulled on-the-fly directly from the **ECCC GeoMet-OGC-API**
(`https://api.weather.gc.ca/collections/climate-{daily,hourly,monthly}`). Station metadata
comes from the ECCC station inventory (bundled as `app/ec_station_inventory_en.csv`, refreshed
from ECCC as needed).

> **Please be mindful of the shinyapps.io free tier** — close the browser tab when you are done.
> Data are downloaded on demand and larger requests (especially hourly/daily over long periods)
> can take a while, so please be patient.

## Features

- **Interactive station map** — explore 8,000+ ECCC stations by province/territory; markers are
  color-coded by record length.
- **Multi-station workflow** — compare up to three stations (Primary / Secondary / Tertiary), or
  enter a Climate ID directly to skip the map.
- **Flexible intervals** — hourly, daily, or monthly records.
- **Raw data review** — interactive time-series plots plus monthly/yearly summary statistics with
  optional trend lines; export to CSV or Excel.
- **Advanced Usage — ERA5-Land infilling** — fill gaps in a station's **daily** record with
  **bias-corrected ERA5-Land reanalysis** (see below).

## Advanced Usage: ERA5-Land infilling

The **Advanced Usage (ERA5 Infill)** tab fills gaps in the *Primary* station's daily series with
ERA5-Land reanalysis, bias-corrected to the station's own observations via **Quantile Delta
Mapping (QDM)** (per calendar month), after an automated WMO/GHCN-style QA/QC of the observations.
It reports calibration/validation CDF reviews, an infilled time series, summary statistics, and the
percentage of each series that came from ERA5, all exportable to Excel.

**You must supply your own free CDS API key.**

- Register for a free account and copy your *Personal Access Token* from the
  [CDS "How to use the API" page](https://cds.climate.copernicus.eu/how-to-api), then accept **all**
  dataset licences on your [CDS profile → Licences](https://cds.climate.copernicus.eu/profile?tab=licences).
- Enter your key in the app when prompted. **Your key is used only for that run and is _not_ stored
  on the server.** (Full step-by-step instructions, including screenshots, are on the app's
  *Advanced Usage → Instructions* sub-tab.)
- Infilled values are **model-based estimates, not measurements** — use them at your own professional
  judgment and verify independently before relying on them.

## For maintainers: deployment (shinyapps.io)

The base app runs on shinyapps.io with no special setup. The **ERA5 infilling feature additionally
needs a Python backend** (`xarray`, `zarr<3`, `fsspec`, `aiohttp`, `pandas`) which is provisioned at
runtime by [`reticulate`](https://rstudio.github.io/reticulate/) via `py_require()` (uv-managed
ephemeral environment — see `app/functions_era5.R`). The rest of the app works whether or not that
backend is available; ERA5 infilling fails gracefully with a message if it isn't.

Deploy from the repo root with rsconnect's Python bundling disabled so it defers to reticulate's
self-managed environment (see [rsconnect#1143](https://github.com/rstudio/rsconnect/issues/1143) and
this [Posit Community thread](https://forum.posit.co/t/deploying-shiny-app-with-python-reticulate-to-shinyapps-io/200055)):

```r
options(rsconnect.python.enabled = FALSE)
rsconnect::deployApp("app")
```

Do **not** set `RETICULATE_PYTHON` to a local virtualenv or bundle Python paths in
`.Rprofile`/`.Renviron`, and do **not** commit any CDS key (`.gitignore` covers `CDSAPI_KEY` and
`.cdsapirc`). R dependencies are pinned in `app/renv.lock`.

## License

Copyright © 2025 [Nick Rong](https://github.com/nickyrong) & [Nate Smith](https://github.com/WraySmith)

Released under the [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.

## Acknowledgements

Data are sourced from Environment and Climate Change Canada; ERA5-Land reanalysis is provided by the
Copernicus Climate Change Service (C3S) / ECMWF. Foundational ECCC data-retrieval methodology draws on
work by Professor Dan Moore (UBC Geography). Development was supported by Knight Piésold Ltd.

## Contributing

Pull requests and stars are always welcome. For bugs and feature requests,
[please open an issue](https://github.com/nickyrong/ShinyWeatherCan/issues).

Please note that this project is released with a
[Contributor Code of Conduct](https://github.com/nickyrong/ShinyWeatherCan/blob/main/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
