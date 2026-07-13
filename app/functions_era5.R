# functions_era5.R -- ERA5-Land ARCO access for the Advanced Usage (infilling) tab.
#
# Thin R wrapper over app/py/era5_extract.py -- as a subprocess when a concrete interpreter
# is resolved (server), else via reticulate (local dev). The Python side opens the ERA5-Land
# geo-chunked ARCO Zarr stores and returns a daily data.frame aggregated on the station's
# Local Standard Time day. See app/CLAUDE.md for the verified data facts.
#
# DEPLOYMENT (shinyapps.io / local) -- no server-side Python install required:
#   * By default no RETICULATE_PYTHON is set, so reticulate self-manages an ephemeral,
#     uv-provisioned environment with the deps below (xarray zarr<3 fsspec aiohttp pandas)
#     the first time infilling runs -- see .era5_mod() / py_require() -- which is the
#     documented shinyapps.io pattern. Deploy with options(rsconnect.python.enabled = FALSE)
#     so rsconnect leaves Python discovery to reticulate (see README + rsconnect issue #1143).
#   * The CDS key is ALWAYS supplied by the user at run time (passed from server.R through
#     get_era5_daily(cds_key=) -> the Python side); it is NOT read from or stored on the server.
#
# WHY A SUBPROCESS is still supported: the ARCO store is https-only, so fsspec/aiohttp need
# Python's ssl. If (and only if) RETICULATE_PYTHON points at a *concrete* interpreter, we run
# era5_extract.py as a SEPARATE PROCESS via its own CLI, which avoids an embedded-_ssl /
# system-libcrypto SONAME conflict some Linux hosts hit (an embedded _ssl can inherit an older
# libcrypto and fail "OPENSSL_x.y.z not found", while a fresh Python process resolves its own
# OpenSSL via rpath). With no explicit interpreter (shinyapps.io / local dev) the in-process
# reticulate + uv path is used (no such conflict there).

# ---- locate the bundled python module (app/py/era5_extract.py) --------------
# The app runs from app/ (shiny::runApp), so "py" resolves; "app/py" covers repo-root runs.
.era5_py_dir <- function() {
  cand <- c("py", file.path("app", "py"))
  for (d in cand) if (file.exists(file.path(d, "era5_extract.py")))
    return(normalizePath(d))
  stop("era5_extract.py not found (looked in: ", paste(cand, collapse = ", "), ")")
}

# ---- resolve the python interpreter (deploy-agnostic) -----------------------
# Honour an explicit RETICULATE_PYTHON only (for a local dev machine that already has the
# deps). Otherwise return "" so .era5_mod() lets reticulate self-provision an ephemeral
# uv env from PyPI (the shinyapps.io path). Do NOT hardcode a server interpreter path here,
# and do NOT set RETICULATE_PYTHON to a local venv when deploying (see the DEPLOYMENT note).
.era5_python <- function() {
  Sys.getenv("RETICULATE_PYTHON", "")
}

# ---- lazily import the python module (once per session) ---------------------
.era5_mod <- local({
  mod <- NULL
  function() {
    if (is.null(mod)) {
      if (!requireNamespace("reticulate", quietly = TRUE))
        stop("Package 'reticulate' is required for ERA5-Land infilling but is not installed.")
      py <- .era5_python()
      if (nzchar(py)) {
        reticulate::use_python(py, required = TRUE)   # explicit interpreter (RETICULATE_PYTHON)
      } else {
        # No explicit interpreter (shinyapps.io / local dev): let reticulate auto-provision
        # an ephemeral env with the deps from PyPI via uv (reticulate >= 1.41).
        try(reticulate::py_require(c("xarray", "zarr<3", "fsspec", "aiohttp", "pandas"),
                                   python_version = "3.12"),
            silent = TRUE)
      }
      mod <<- reticulate::import_from_path("era5_extract", path = .era5_py_dir())
    }
    mod
  }
})

# ---- subprocess runner (server path) ----------------------------------------
# Extra env for the child Python. Prepend the interpreter's own lib dir so its bundled
# OpenSSL wins even if the parent (embedded R) process put a different libcrypto on
# LD_LIBRARY_PATH. (Running Python fresh is what actually fixes the SSL conflict -- see the
# DEPLOYMENT note above -- this just hardens against an unusual LD_LIBRARY_PATH.)
.era5_child_env <- function(py) {
  if (.Platform$OS.type != "unix") return(character())
  libdir <- suppressWarnings(normalizePath(file.path(dirname(py), "..", "lib"), mustWork = FALSE))
  if (!nzchar(libdir) || !dir.exists(libdir)) return(character())
  ld <- Sys.getenv("LD_LIBRARY_PATH", "")
  paste0("LD_LIBRARY_PATH=", if (nzchar(ld)) paste(libdir, ld, sep = ":") else libdir)
}

# Run the interpreter `py` with `args`, capturing stdout+stderr. A CDS key, if given, is
# passed via the inherited environment (CDSAPI_KEY) -- NOT argv -- so it never shows in `ps`.
.era5_run_py <- function(py, args, key = NULL) {
  if (!is.null(key) && nzchar(key)) {
    old <- Sys.getenv("CDSAPI_KEY", unset = NA_character_)
    Sys.setenv(CDSAPI_KEY = key)
    on.exit(if (is.na(old)) Sys.unsetenv("CDSAPI_KEY") else Sys.setenv(CDSAPI_KEY = old), add = TRUE)
  }
  out <- suppressWarnings(
    system2(py, args, stdout = TRUE, stderr = TRUE, env = .era5_child_env(py)))
  status <- attr(out, "status")
  list(ok = is.null(status) || status == 0, output = out, status = status)
}

#' Is the ERA5 Python backend usable? (interpreter + deps + a WORKING ssl module)
#' Returns TRUE, or FALSE with a warning describing what's missing. When a concrete
#' interpreter is resolved this probes it as a subprocess (so it exercises the exact ssl
#' path used for extraction); otherwise it falls back to the reticulate import (local dev).
era5_ready <- function() {
  py <- .era5_python()
  if (nzchar(py)) {
    tryCatch({
      res <- .era5_run_py(py, c("-c", shQuote("import ssl, xarray, zarr, fsspec, aiohttp, pandas")))
      if (!isTRUE(res$ok)) stop(paste(res$output, collapse = "\n"))
      TRUE
    }, error = function(e) { warning("ERA5 backend not ready (", py, "): ", conditionMessage(e)); FALSE })
  } else {
    tryCatch({ .era5_mod(); TRUE },
             error = function(e) { warning("ERA5 backend not ready: ", conditionMessage(e)); FALSE })
  }
}

#' Station Local Standard Time offset (hours from UTC), derived from ECCC's own data.
#' ECCC timestamps use fixed LST (no DST, unaffected by civil-time changes such as BC's
#' permanent UTC-7 move), so offset = LOCAL_DATE - UTC_DATE. Do NOT use a tz library.
#' @param local_date,utc_date POSIXct/character vectors from ECCC climate-hourly.
eccc_lst_offset <- function(local_date, utc_date) {
  d <- as.numeric(difftime(as.POSIXct(local_date, tz = "UTC"),
                           as.POSIXct(utc_date,   tz = "UTC"), units = "hours"))
  d <- round(d[is.finite(d)])
  if (!length(d)) stop("Cannot derive LST offset: no valid LOCAL/UTC pairs.")
  as.integer(names(sort(table(d), decreasing = TRUE))[1])  # modal offset
}

#' Daily ERA5-Land at a station point, aggregated on the station's LST day.
#' @param lat,lon Decimal degrees (ECCC inventory Latitude/Longitude..Decimal.Degrees.).
#' @param lst_offset_hours Integer hours from UTC (use eccc_lst_offset()).
#' @return data.frame: date (Date), era5_t2m_min/mean/max (degC), era5_tp_mm (mm), era5_n_hours.
get_era5_daily <- function(lat, lon, lst_offset_hours, cds_key = NULL) {
  stopifnot(is.numeric(lat), is.numeric(lon), is.numeric(lst_offset_hours))
  key <- if (is.null(cds_key) || !nzchar(cds_key)) NULL else as.character(cds_key)
  py <- .era5_python()
  if (nzchar(py)) {
    # server / explicit interpreter: run the extractor as a fresh SUBPROCESS via its CLI so
    # Python's ssl resolves the interpreter's own OpenSSL (see DEPLOYMENT note above).
    script  <- file.path(.era5_py_dir(), "era5_extract.py")
    out_csv <- tempfile("era5_", fileext = ".csv")
    on.exit(unlink(out_csv), add = TRUE)
    args <- c(shQuote(script),
              "--lat",    sprintf("%.6f", lat),
              "--lon",    sprintf("%.6f", lon),
              "--offset", as.character(as.integer(lst_offset_hours)),
              "--out",    shQuote(out_csv))
    res <- .era5_run_py(py, args, key = key)
    if (!isTRUE(res$ok) || !file.exists(out_csv))
      stop("ERA5 extraction failed (interpreter ", py, "):\n", paste(res$output, collapse = "\n"))
    daily <- utils::read.csv(out_csv, stringsAsFactors = FALSE)
  } else {
    # local dev: no explicit interpreter -> reticulate in-process (uv-provisioned deps)
    daily <- as.data.frame(
      .era5_mod()$daily_at_point(lat, lon, as.integer(lst_offset_hours), key = key))
  }
  daily$date <- as.Date(daily$date)
  daily
}
