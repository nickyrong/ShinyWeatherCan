# functions_qaqc.R -- automatic QA/QC for ECCC daily climate data.
#
# Used by the Advanced Usage (infilling) tab BEFORE bias-correction/infilling: it flags
# (does not delete) suspicious observed values so they can be excluded from the QDM
# calibration and optionally removed before gaps are filled with bias-corrected ERA5.
#
# Standards basis -- daily meteorological time-series QC (NOT field/lab sampling QA/QC such
# as the BC Field Sampling Manual or CSA, which cover sample collection/analysis, not station
# time series). Tests follow the recognized climate-data standards:
#   * WMO-No. 1186  Guidelines on QC Procedures for Data from Automatic Weather Stations
#   * WMO-No. 100   Guide to Climatological Practices
#   * Durre et al. (2010) "Comprehensive Automated QA of Daily Surface Observations",
#                   J. Appl. Meteor. Climatol. 49:1615-1633  (the GHCN-Daily QC suite)
#
# Checks (each maps to the above categories):
#   range        gross/plausible-limit: temp outside [-65, 50] C; precip < 0 or > 400 mm
#                (Canadian plausible bounds; widen toward WMO world records if desired)
#   consistency  internal consistency: Tmax < Tmin, or Tmean outside [Tmin, Tmax] (tolerance)
#   spike        temporal consistency (WMO step test / GHCN spike-dip): a day differing from
#                BOTH neighbours by >= spike_temp C in the same direction (temperature)
#   outlier      climatological outlier: robust per-calendar-month modified z-score (MAD) > k
#   flatline     persistence: >= N consecutive identical values (stuck sensor); precip dry-day
#                zeros exempt
#
# Output: input df + one "<VAR>_qc" character column per checked variable, each value one of
#   "ok","range","consistency","spike","outlier","flatline"; attr(df,"qaqc_summary") counts.
#   qaqc_clean(df) returns a copy with flagged (!= "ok") values set to NA.

library(dplyr)
library(lubridate)

#' QA/QC thresholds (override any as needed).
qaqc_config <- function(temp_range   = c(-65, 50),   # deg C (Canadian records ~ -63 .. 49.6)
                        precip_range = c(0, 400),     # mm/day
                        outlier_k    = 5,             # modified z-score (MAD) threshold; conservative
                        flatline_run = 5,             # >= this many identical consecutive values = stuck
                        spike_temp   = 25,            # deg C step to BOTH neighbours = spike/dip (GHCN)
                        mean_tol     = 0.1) {         # deg C tolerance for Tmean within [Tmin, Tmax]
  list(temp_range = temp_range, precip_range = precip_range, outlier_k = outlier_k,
       flatline_run = flatline_run, spike_temp = spike_temp, mean_tol = mean_tol)
}

#' Auto-detect ECCC daily variable roles from column names.
qaqc_detect_vars <- function(df) {
  nm  <- names(df)
  num <- vapply(df, is.numeric, logical(1))
  temp   <- nm[grepl("TEMP",   nm, ignore.case = TRUE) & num]
  precip <- nm[grepl("PRECIP", nm, ignore.case = TRUE) & num]
  # only match NUMERIC columns: precip-only stations still carry empty MEAN/MIN/MAX_TEMPERATURE
  # columns (parsed as all-NA logical), which must NOT be picked as tmin/tmax/tmean -- they get
  # no _qc column, so the consistency check would then setflag() a NULL column and error.
  first <- function(pat) { h <- nm[grepl(pat, nm, ignore.case = TRUE) & num]; if (length(h)) h[1] else NA_character_ }
  list(temp = temp, precip = precip,
       tmin  = first("MIN.*TEMP|TEMP.*MIN"),
       tmax  = first("MAX.*TEMP|TEMP.*MAX"),
       tmean = first("MEAN.*TEMP|TEMP.*MEAN"))
}

.qc_range <- function(x, lo, hi) !is.na(x) & (x < lo | x > hi)

# >= run consecutive identical (non-NA) values
.qc_flatline <- function(x, run) {
  flag <- rep(FALSE, length(x))
  r <- rle(x)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  for (i in seq_along(r$lengths))
    if (!is.na(r$values[i]) && r$lengths[i] >= run) flag[starts[i]:ends[i]] <- TRUE
  flag
}

# temporal consistency: spike/dip -- day differs from BOTH neighbours by >= thr, same sense
# (WMO-No. 1186 step test; Durre et al. 2010 GHCN spike-dip, thr ~ 25 C for temperature)
.qc_spike_dip <- function(x, thr) {
  n <- length(x); flag <- rep(FALSE, n)
  if (n < 3L) return(flag)
  d_in  <- c(NA, diff(x))   # x[t] - x[t-1]
  d_out <- c(diff(x), NA)   # x[t+1] - x[t]
  spike <- !is.na(d_in) & !is.na(d_out) & d_in >=  thr & d_out <= -thr  # higher than both
  dip   <- !is.na(d_in) & !is.na(d_out) & d_in <= -thr & d_out >=  thr  # lower than both
  flag[spike | dip] <- TRUE
  flag
}

# robust per-calendar-month outliers via modified z-score (MAD)
.qc_outlier_monthly <- function(x, month, k) {
  flag <- rep(FALSE, length(x))
  for (m in unique(month)) {
    idx <- which(month == m & !is.na(x))
    if (length(idx) < 10L) next
    v <- x[idx]; med <- median(v); s <- mad(v)  # mad() is already scaled (*1.4826)
    if (is.na(s) || s == 0) next
    flag[idx[abs(v - med) / s > k]] <- TRUE
  }
  flag
}

#' Run QA/QC on an ECCC daily data.frame. Returns df + <VAR>_qc flag columns.
qaqc_daily <- function(df, date_col = "LOCAL_DATE", config = qaqc_config(), vars = NULL) {
  stopifnot(date_col %in% names(df))
  df <- as.data.frame(df)
  if (is.null(vars)) vars <- qaqc_detect_vars(df)
  mon     <- lubridate::month(as.Date(df[[date_col]]))
  checked <- unique(c(vars$temp, vars$precip))
  if (!length(checked)) { warning("QAQC: no temperature/precip columns detected."); return(df) }
  for (v in checked) df[[paste0(v, "_qc")]] <- "ok"

  # first-flag-wins: only overwrite cells still "ok"
  setflag <- function(df, v, mask, label) {
    col <- paste0(v, "_qc"); hit <- mask & df[[col]] == "ok"
    df[[col]][hit] <- label; df
  }

  # 1) range / impossible
  for (v in vars$temp)   df <- setflag(df, v, .qc_range(df[[v]], config$temp_range[1],   config$temp_range[2]),   "range")
  for (v in vars$precip) df <- setflag(df, v, .qc_range(df[[v]], config$precip_range[1], config$precip_range[2]), "range")

  # 2) consistency (temperature)
  if (!is.na(vars$tmin) && !is.na(vars$tmax)) {
    bad <- !is.na(df[[vars$tmin]]) & !is.na(df[[vars$tmax]]) & df[[vars$tmax]] < df[[vars$tmin]]
    for (v in c(vars$tmin, vars$tmax)) df <- setflag(df, v, bad, "consistency")
  }
  if (!is.na(vars$tmean) && !is.na(vars$tmin) && !is.na(vars$tmax)) {
    bad <- !is.na(df[[vars$tmean]]) & !is.na(df[[vars$tmin]]) & !is.na(df[[vars$tmax]]) &
      (df[[vars$tmean]] < df[[vars$tmin]] - config$mean_tol |
       df[[vars$tmean]] > df[[vars$tmax]] + config$mean_tol)
    df <- setflag(df, vars$tmean, bad, "consistency")
  }

  # 3) temporal consistency: spike/dip (temperature)
  for (v in vars$temp) df <- setflag(df, v, .qc_spike_dip(df[[v]], config$spike_temp), "spike")

  # 4) climatological outliers (temperature only; precip too skewed -> rely on range)
  for (v in vars$temp) df <- setflag(df, v, .qc_outlier_monthly(df[[v]], mon, config$outlier_k), "outlier")

  # 5) flatline / stuck sensor (precip: exempt dry-day zeros)
  for (v in vars$temp)   df <- setflag(df, v, .qc_flatline(df[[v]], config$flatline_run), "flatline")
  for (v in vars$precip) {
    xz <- df[[v]]; xz[!is.na(xz) & xz == 0] <- NA_real_
    df <- setflag(df, v, .qc_flatline(xz, config$flatline_run), "flatline")
  }

  attr(df, "qaqc_summary") <- setNames(lapply(checked, function(v)
    table(factor(df[[paste0(v, "_qc")]],
                 levels = c("ok", "range", "consistency", "spike", "outlier", "flatline")))), checked)
  df
}

#' Return a copy with flagged (!= "ok") values set to NA.
qaqc_clean <- function(df) {
  for (qc in grep("_qc$", names(df), value = TRUE)) {
    v <- sub("_qc$", "", qc)
    if (v %in% names(df)) df[[v]][df[[qc]] != "ok"] <- NA
  }
  df
}
