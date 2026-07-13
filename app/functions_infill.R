# functions_infill.R -- assemble the ECCC + bias-corrected ERA5 infilled series per station.
#
# Composes the pipeline: QAQC (functions_qaqc.R) -> ERA5-Land daily (functions_era5.R) ->
# QDM bias-correction + infill (functions_bias_correct.R). Source those three first.

# Sample `size` items from `x` with a fixed seed (reproducible split across re-runs), WITHOUT
# disturbing the global RNG stream -- so downstream qdm()'s runif() draws are unaffected.
.sample_stable <- function(x, size, seed = 1L) {
  has_old <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (has_old) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (has_old) assign(".Random.seed", old, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  sample(x, size)
}

#' Default mapping: ECCC daily column -> ERA5 daily column + ratio flag (precip = TRUE).
infill_var_map <- function() list(
  MIN_TEMPERATURE     = list(era5 = "era5_t2m_min",  ratio = FALSE),
  MAX_TEMPERATURE     = list(era5 = "era5_t2m_max",  ratio = FALSE),
  MEAN_TEMPERATURE    = list(era5 = "era5_t2m_mean", ratio = FALSE),
  TOTAL_PRECIPITATION = list(era5 = "era5_tp_mm",    ratio = TRUE)
)

#' Infill an ECCC daily record with bias-corrected ERA5-Land.
#'
#' @param eccc_daily data.frame with `date_col` + ECCC daily variables.
#' @param lat,lon station decimal degrees.
#' @param lst_offset station Local Standard Time offset (hours from UTC); derive from the
#'   station's ECCC hourly LOCAL_DATE - UTC_DATE via eccc_lst_offset(), NOT a tz library.
#' @param var_map named list mapping ECCC col -> list(era5 = <col>, ratio = <bool>).
#' @param config qaqc_config().
#' @param era5 optional pre-fetched ERA5 daily df (columns from get_era5_daily); if NULL it
#'   is fetched (needs the reticulate/python backend + CDS key).
#' @return list(data = infilled df [date + <var> + <var>_source per variable],
#'   qaqc = flagged obs df, qaqc_summary = per-variable flag counts).
infill_station <- function(eccc_daily, lat, lon, lst_offset,
                           var_map = infill_var_map(), config = qaqc_config(),
                           era5 = NULL, date_col = "LOCAL_DATE", clip_to_obs = FALSE,
                           cds_key = NULL, valid_seed = 1L) {
  stopifnot(date_col %in% names(eccc_daily))

  # 1) QAQC the observations and null the flagged values
  qc    <- qaqc_daily(eccc_daily, date_col = date_col, config = config)
  clean <- qaqc_clean(qc)
  obs_date <- as.Date(clean[[date_col]])

  # 2) ERA5-Land daily on the station's LST day
  if (is.null(era5)) era5 <- get_era5_daily(lat, lon, lst_offset, cds_key = cds_key)
  era5$date <- as.Date(era5$date)
  # the sampled ERA5-Land grid cell + its distance from the station (constant columns from the
  # extractor); a large distance flags a coastal/island station snapped to the nearest land cell
  era5_grid <- if (all(c("era5_grid_lat", "era5_grid_lon", "era5_grid_km") %in% names(era5)))
    c(lat = era5$era5_grid_lat[1], lon = era5$era5_grid_lon[1], km = era5$era5_grid_km[1]) else NULL

  # calibration / validation split of the station's overlap with ERA5. Validation = a set of
  # RANDOM complete months (>= 90% of days observed) held out from fitting -- NOT the first
  # years -- so a non-stationary early period cannot confound the validation. The number of
  # months held out is tiered by overlap length; calibration = all remaining overlap. >= 7 yr.
  vcols   <- intersect(names(var_map), names(clean))
  has_obs <- Reduce(`|`, lapply(vcols, function(cc) !is.na(clean[[cc]])), FALSE)
  ov <- sort(obs_date[has_obs & obs_date >= min(era5$date) & obs_date <= max(era5$date)])
  min_years <- 7L
  valid_dates <- NULL
  cv0 <- function(yrs, o0, o1) list(sufficient = FALSE, years = yrs, valid_years = 0L,
           valid_months = 0L, calib_months = NA_integer_, valid_ym = character(0),
           valid_start = as.Date(NA), valid_end = as.Date(NA), calib_start = o0, calib_end = o1)
  if (length(ov) > 0) {
    o0 <- min(ov); o1 <- max(ov)
    yrs <- as.numeric(difftime(o1, o0, units = "days")) / 365.25
    cv <- cv0(yrs, o0, o1)
    if (yrs >= min_years) {
      # tiered amount held out: >30 yr -> 10, >20 -> 5, >10 -> 3, else 2 instances of EACH
      # calendar month, so the held-out set is evenly distributed (k Jan, k Feb, ... k Dec).
      valid_years <- if (yrs > 30) 10L else if (yrs > 20) 5L else if (yrs > 10) 3L else 2L
      ym       <- format(ov, "%Y-%m")
      obs_n    <- table(ym)                                # observed days per overlap year-month
      ym_first <- as.Date(paste0(names(obs_n), "-01"))
      complete <- names(obs_n)[as.integer(obs_n) >= 0.9 * lubridate::days_in_month(ym_first)]
      cmo      <- as.integer(substr(complete, 6, 7))       # calendar month of each complete month
      # per calendar month, randomly hold out min(valid_years, half the complete instances) --
      # capping at half keeps enough of every month in the calibration fit
      sel <- unlist(lapply(1:12, function(mo) {
        cand <- complete[cmo == mo]
        k <- min(valid_years, floor(length(cand) / 2))
        if (k > 0) .sample_stable(cand, k, seed = valid_seed + mo) else character(0)
      }))
      if (length(complete) >= 24L && length(sel) >= 12L) {
        sel         <- sort(sel)
        valid_dates <- ov[ym %in% sel]
        cv <- list(sufficient = TRUE, years = yrs, valid_years = valid_years,
                   valid_months = length(sel), calib_months = length(complete) - length(sel),
                   valid_ym = sel, valid_start = min(valid_dates), valid_end = max(valid_dates),
                   calib_start = o0, calib_end = o1)
      }
    }
  } else {
    cv <- cv0(0, as.Date(NA), as.Date(NA))
  }

  # 3) per-variable bias-correct + infill over the FULL range = union(ERA5, obs) dates:
  #    forward to the most recent ERA5, back to min(1950, station start). Pre-1950 dates
  #    have no ERA5, so pre-1950 gaps remain NA (not infillable; flagged below).
  out <- data.frame(date = sort(unique(c(era5$date, obs_date))))
  bc_detail <- list()   # obs / era5 / era5_bc per variable, for CDF review plots
  for (ecol in names(var_map)) {
    m <- var_map[[ecol]]
    if (!ecol %in% names(clean) || !m$era5 %in% names(era5)) next
    # skip variables the station never measured (e.g. temperature at a precip-only station):
    # the column may be present but empty/all-NA. Without observations there is nothing to
    # calibrate against, so don't emit a (misleading) pure-ERA5 series for it.
    if (!is.numeric(clean[[ecol]]) || all(is.na(clean[[ecol]]))) next
    merged <- merge(era5[, c("date", m$era5)],
                    data.frame(date = obs_date, obs = clean[[ecol]]),
                    by = "date", all = TRUE)          # full outer join (keep pre-1950 obs)
    names(merged)[names(merged) == m$era5] <- "era5"
    bc  <- bias_correct_monthly(merged, ratio = m$ratio, trace = 0.05, valid_dates = valid_dates)
    inf <- infill_series(bc)
    bc_detail[[ecol]] <- bc
    j <- match(out$date, inf$date)
    out[[ecol]]                   <- inf$value[j]
    out[[paste0(ecol, "_source")]] <- inf$source[j]
  }
  # optional: clip to the station's period of record (default FALSE -> keep the max range)
  if (isTRUE(clip_to_obs) && any(has_obs)) {
    rng <- range(obs_date[has_obs])
    out <- out[out$date >= rng[1] & out$date <= rng[2], , drop = FALSE]
  }
  # observations before 1950 (ERA5 start) cannot be infilled -> flag for a UI warning
  pre1950 <- any(has_obs & obs_date < as.Date("1950-01-01"))
  list(data = out, qaqc = qc, qaqc_summary = attr(qc, "qaqc_summary"),
       bc = bc_detail, pre1950 = pre1950, cv = cv, era5_grid = era5_grid)
}

#' Station Local Standard Time offset (hours from UTC), derived from the station's own
#' ECCC hourly data (LOCAL_DATE - UTC_DATE) -- robust to DST / permanent-DST / tz boundaries.
#' Falls back to round(lon/15) with a warning if the hourly probe fails.
#' @param climate_id ECCC Climate ID. @param lon decimal longitude (fallback only).
station_lst_offset <- function(climate_id, lon = NULL) {
  # Best effort: derive the exact offset from the station's hourly record (LOCAL - UTC).
  # Most ECCC climate stations are daily-only (no hourly), so this quietly fails and we
  # fall back to the standard-time offset from longitude. The probe is fully silenced
  # (the empty hourly query otherwise prints a fread size-0 warning).
  off <- tryCatch({
    tmp <- NULL
    invisible(utils::capture.output(suppressWarnings(suppressMessages(
      tmp <- get_ec_met(climate_id, res = "hourly",
                        dt1 = "2015-07-01T00:00:00Z", dt2 = "2015-07-02T06:00:00Z", limit = 48)))))
    if (is.data.frame(tmp) && nrow(tmp) > 0 && all(c("LOCAL_DATE", "UTC_DATE") %in% names(tmp)))
      eccc_lst_offset(tmp$LOCAL_DATE, tmp$UTC_DATE) else NA_integer_
  }, error = function(e) NA_integer_)
  if (is.na(off)) {
    if (is.null(lon)) stop("Could not derive LST offset and no longitude fallback given.")
    off <- as.integer(round(lon / 15))   # standard-time offset from longitude (silent)
  }
  as.integer(off)
}
