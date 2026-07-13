# functions_bias_correct.R -- QDM bias-correction of ERA5-Land to QAQC'd ECCC.
#
# Quantile Delta Mapping (MBC::QDM) fit PER CALENDAR MONTH, following the pattern in
# nick_r_toolkit/Monthly_Bias_Correction.R:
#   - temperature   -> additive       (ratio = FALSE)
#   - precipitation -> multiplicative (ratio = TRUE, trace handling for drizzle)
# Calibration = dates where BOTH the QAQC-cleaned observation and ERA5 exist. The fitted
# monthly transfer is applied to the full ERA5 series, so any ECCC gap can be infilled.

library(lubridate)

# QDM -- Quantile Delta Mapping (Cannon, Sobie & Murdock 2015, J. Climate 28:6938-6959),
# ported verbatim from the MBC package (A. Cannon, GPL-2/3) so the app needs NO MBC -> energy
# -> gsl dependency chain (the deploy server lacks the system libgsl). Pure base R; identical
# to MBC::QDM given the same RNG state (ratio mode draws runif() for sub-trace values).
qdm <- function(o.c, m.c, m.p, ratio = FALSE, trace = 0.05, trace.calc = 0.5 * trace,
                jitter.factor = 0, n.tau = NULL, ratio.max = 2, ratio.max.trace = 10 * trace,
                ECBC = FALSE, ties = "first", subsample = NULL, pp.type = 7) {
  if (jitter.factor == 0 && (length(unique(o.c)) == 1 || length(unique(m.c)) == 1 ||
                             length(unique(m.p)) == 1)) {
    jitter.factor <- sqrt(.Machine$double.eps)
  }
  if (jitter.factor > 0) {
    o.c <- jitter(o.c, jitter.factor)
    m.c <- jitter(m.c, jitter.factor)
    m.p <- jitter(m.p, jitter.factor)
  }
  if (ratio) {
    epsilon <- .Machine$double.eps
    o.c[o.c < trace.calc] <- runif(sum(o.c < trace.calc), min = epsilon, max = trace.calc)
    m.c[m.c < trace.calc] <- runif(sum(m.c < trace.calc), min = epsilon, max = trace.calc)
    m.p[m.p < trace.calc] <- runif(sum(m.p < trace.calc), min = epsilon, max = trace.calc)
  }
  n <- length(m.p)
  if (is.null(n.tau)) n.tau <- n
  tau <- seq(0, 1, length = n.tau)
  if (!is.null(subsample)) {
    quant.o.c <- rowMeans(apply(replicate(subsample, sample(o.c, size = length(tau))), 2,
                                quantile, probs = tau, type = pp.type, names = FALSE))
    quant.m.c <- rowMeans(apply(replicate(subsample, sample(m.c, size = length(tau))), 2,
                                quantile, probs = tau, type = pp.type, names = FALSE))
    quant.m.p <- rowMeans(apply(replicate(subsample, sample(m.p, size = length(tau))), 2,
                                quantile, probs = tau, type = pp.type, names = FALSE))
  } else {
    quant.o.c <- quantile(o.c, tau, type = pp.type, names = FALSE)
    quant.m.c <- quantile(m.c, tau, type = pp.type, names = FALSE)
    quant.m.p <- quantile(m.p, tau, type = pp.type, names = FALSE)
  }
  tau.m.p <- approx(quant.m.p, tau, m.p, rule = 2, ties = "ordered")$y
  if (ratio) {
    approx.t.qmc.tmp <- approx(tau, quant.m.c, tau.m.p, rule = 2, ties = "ordered")$y
    delta.m <- m.p / approx.t.qmc.tmp
    delta.m[(delta.m > ratio.max) & (approx.t.qmc.tmp < ratio.max.trace)] <- ratio.max
    mhat.p <- approx(tau, quant.o.c, tau.m.p, rule = 2, ties = "ordered")$y * delta.m
  } else {
    delta.m <- m.p - approx(tau, quant.m.c, tau.m.p, rule = 2, ties = "ordered")$y
    mhat.p <- approx(tau, quant.o.c, tau.m.p, rule = 2, ties = "ordered")$y + delta.m
  }
  mhat.c <- approx(quant.m.c, quant.o.c, m.c, rule = 2, ties = "ordered")$y
  if (ratio) {
    mhat.c[mhat.c < trace] <- 0
    mhat.p[mhat.p < trace] <- 0
  }
  if (ECBC) {
    if (length(mhat.p) == length(o.c)) {
      mhat.p <- sort(mhat.p)[rank(o.c, ties.method = ties)]
    } else {
      stop("Schaake shuffle failed due to incompatible lengths")
    }
  }
  list(mhat.c = mhat.c, mhat.p = mhat.p)
}

#' Bias-correct one ERA5 daily variable to match observed (ECCC) via monthly QDM.
#' @param merged data.frame: date (Date), obs (numeric, QAQC-cleaned; NA = gap/missing),
#'   era5 (numeric, complete series at the same dates).
#' @param ratio TRUE for precipitation (multiplicative), FALSE for temperature (additive).
#' @param trace precip drizzle threshold (mm); ignored for temperature.
#' @param min_overlap minimum calibration days in a month to fit QDM; below this, fall back
#'   to a simple monthly mean shift (additive) / scale (ratio).
#' @param valid_dates Date vector of held-out VALIDATION dates (random complete months chosen by
#'   infill_station); excluded from the QDM fit. NULL = use the whole overlap for calibration.
#' @return merged + `era5_bc` (bias-corrected ERA5, complete) and `bc_method` per row.
bias_correct_monthly <- function(merged, ratio = FALSE, trace = 0.05, min_overlap = 30,
                                 valid_dates = NULL) {
  stopifnot(all(c("date", "obs", "era5") %in% names(merged)))
  m <- as.data.frame(merged)
  mon <- lubridate::month(as.Date(m$date))
  m$era5_bc   <- NA_real_
  m$bc_method <- NA_character_
  # validation = the held-out overlap dates (random complete months chosen by the caller);
  # calibration = the rest of the overlap. QDM is fit on calibration only.
  is_valid <- if (is.null(valid_dates)) rep(FALSE, nrow(m)) else as.Date(m$date) %in% as.Date(valid_dates)
  for (mo in 1:12) {
    ix <- which(mon == mo)
    if (!length(ix)) next
    o <- m$obs[ix]; e <- m$era5[ix]; iv <- is_valid[ix]
    ev <- which(!is.na(e))                      # rows with ERA5 present (pre-1950 has none)
    if (!length(ev)) next                       # no ERA5 -> era5_bc stays NA (not infillable)
    cal <- which(!iv & !is.na(o) & !is.na(e))   # fit on CALIBRATION overlap (exclude validation)
    if (length(cal) >= min_overlap) {
      fit <- tryCatch(qdm(o.c = o[cal], m.c = e[cal], m.p = e[ev],
                          ratio = ratio, trace = trace),
                      error = function(err) NULL)
      if (!is.null(fit)) { m$era5_bc[ix[ev]] <- as.numeric(fit$mhat.p); m$bc_method[ix[ev]] <- "QDM"; next }
    }
    if (length(cal) > 0) {                      # fallback: monthly mean shift / scale
      if (ratio) {
        f <- mean(o[cal], na.rm = TRUE) / max(mean(e[cal], na.rm = TRUE), 1e-6)
        m$era5_bc[ix[ev]] <- e[ev] * f
      } else {
        m$era5_bc[ix[ev]] <- e[ev] + (mean(o[cal], na.rm = TRUE) - mean(e[cal], na.rm = TRUE))
      }
      m$bc_method[ix[ev]] <- "mean-adjust"
    } else {                                    # no overlap at all: raw ERA5
      m$era5_bc[ix[ev]] <- e[ev]; m$bc_method[ix[ev]] <- "raw"
    }
  }
  if (ratio) m$era5_bc[!is.na(m$era5_bc) & m$era5_bc < 0] <- 0
  # tag overlap dates as calibration / validation (for the CDF review split)
  overlap <- !is.na(m$obs) & !is.na(m$era5)
  m$set <- ifelse(!overlap, NA_character_, ifelse(is_valid, "validation", "calibration"))
  m
}

#' 12-panel monthly empirical-CDF review plot: observed vs raw ERA5 vs bias-corrected ERA5.
#' Lets a user visually confirm QDM shifted the ERA5 distribution onto the observed one.
#' @param bc output of bias_correct_monthly() (columns date, obs, era5, era5_bc).
#' @param var_name label for the plot title. @return a plotly subplot (4x3 months).
cdf_monthly_plotly <- function(bc, var_name = "", set = NULL) {
  stopifnot(all(c("date", "obs", "era5", "era5_bc") %in% names(bc)))
  bc <- as.data.frame(bc)
  if (!is.null(set) && "set" %in% names(bc)) bc <- bc[!is.na(bc$set) & bc$set == set, , drop = FALSE]
  bc$m <- as.integer(format(as.Date(bc$date), "%m"))
  ecdf_df <- function(x) { x <- sort(x[is.finite(x)]); if (!length(x)) NULL else
                           data.frame(x = x, y = seq_along(x) / length(x)) }
  panel <- function(mo) {
    d <- bc[bc$m == mo, ]
    o <- ecdf_df(d$obs); r <- ecdf_df(d$era5); b <- ecdf_df(d$era5_bc)
    sl <- (mo == 1L)  # show legend once
    p <- plotly::plot_ly()
    if (!is.null(r)) p <- plotly::add_lines(p, data = r, x = ~x, y = ~y, name = "ERA5 raw",
                       line = list(color = "#d62728", dash = "dot"), legendgroup = "raw", showlegend = sl)
    if (!is.null(b)) p <- plotly::add_lines(p, data = b, x = ~x, y = ~y, name = "ERA5 bias-corrected",
                       line = list(color = "#1f77b4"), legendgroup = "bc", showlegend = sl)
    if (!is.null(o)) p <- plotly::add_lines(p, data = o, x = ~x, y = ~y, name = "Observed (ECCC)",
                       line = list(color = "black", width = 2), legendgroup = "obs", showlegend = sl)
    plotly::layout(p, xaxis = list(title = month.name[mo]), yaxis = list(title = ""))
  }
  plotly::subplot(lapply(1:12, panel), nrows = 4, shareY = TRUE, titleX = TRUE, margin = 0.042) |>
    plotly::layout(legend = list(orientation = "h", x = 0.5, xanchor = "center",
                                 y = 1.0, yanchor = "bottom", font = list(size = 12)),
                   margin = list(t = 42, b = 40))
}

#' Infill observed gaps with bias-corrected ERA5 and tag the data source.
#' @return merged_bc + `value` (obs where present, else era5_bc) and `source`
#'   ("ECCC observed" / "ERA5 bias-corrected"; NA where neither is available).
infill_series <- function(merged_bc) {
  stopifnot(all(c("date", "obs", "era5_bc") %in% names(merged_bc)))
  d <- merged_bc
  d$value  <- ifelse(!is.na(d$obs), d$obs, d$era5_bc)
  d$source <- ifelse(!is.na(d$obs), "ECCC observed",
                     ifelse(!is.na(d$era5_bc), "ERA5 bias-corrected", NA_character_))
  d
}
