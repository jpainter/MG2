# Outlier detection functions for MG2.
# Migrated from R/originals/Cleaning.R

# Shared mutable state for time-throttled Shiny progress updates.
# Initialised by mad_outliers() / seasonal_outliers() before each scan;
# read/written by extremely_mad() and the seasonal group_modify loop.
.mg2_scan_state <- new.env(parent = emptyenv())
.mg2_scan_state$n        <- 0L    # series processed so far
.mg2_scan_state$total    <- 0L    # total series in this scan
.mg2_scan_state$last_t   <- 0     # proc.time elapsed at last UI update
.mg2_scan_state$interval <- 10    # minimum seconds between UI updates

# Worker count helper ---------------------------------------------------------
#
# On macOS/Linux: multicore (fork) is used — workers inherit the parent process
# so packages are not reloaded and data is not serialized.  Use all cores minus
# 2 (reserved for main R session + OS), no memory cap needed.
#
# On Windows: multisession spawns separate R processes that each load packages
# independently.  `per_worker_mb` is the estimated RAM cost per worker (e.g.
# ~200 MB for MAD-only workers, ~400 MB for seasonal workers that load forecast).
# We reserve 2 GB for the main session and cap by free memory.
#
# @noRd
.mg2_n_workers <- function(per_worker_mb = 400L) {
  n_cores <- max(1L, parallelly::availableCores() - 2L)
  if (.Platform$OS.type == "unix") return(n_cores)   # fork: no per-worker memory cost
  free_mb <- tryCatch(
    as.integer(parallelly::freeMemory() / 1024^2),
    error = function(e) 4000L                         # conservative fallback if unavailable
  )
  mem_limit <- max(1L, floor((free_mb - 2000L) / per_worker_mb))
  min(n_cores, mem_limit)
}

#' Flag Values Exceeding a MAD-Based Threshold
#'
#' For a numeric vector `x`, computes the median and median absolute deviation
#' (MAD), then flags values that deviate from the median by more than
#' `deviation × MAD`. When MAD is near zero, falls back to a trimmed SD.
#' Returns `NA` for all values when the series is entirely below
#' `smallThreshold`.
#'
#' @param x Numeric vector.
#' @param deviation Numeric. Multiple of MAD used as threshold (default: `15`).
#' @param smallThreshold Numeric. Series with all values ≤ this are skipped
#'   (default: `50`).
#' @param key_entry_error Logical vector. Pre-flagged key-entry errors (unused
#'   internally; kept for API compatibility).
#' @param over_max Logical vector. Pre-flagged over-max values (unused
#'   internally; kept for API compatibility).
#' @param maximum_allowed Numeric. Pre-flagged maximum cap (unused internally).
#' @param logical Logical. When `TRUE` return a logical flag vector; when
#'   `FALSE` return values with non-outliers set to `NA` (default: `FALSE`).
#' @param .progress Logical. Update Shiny progress bar (default: `FALSE`).
#' @param total Integer. Total series count for progress increment calculation.
#'
#' @return Logical or numeric vector the same length as `x`.
#' @export
extremely_mad <- function(x,
                          deviation       = 15,
                          smallThreshold  = 50,
                          key_entry_error = NA,
                          over_max        = NA,
                          maximum_allowed = NA,
                          logical         = FALSE,
                          .progress       = FALSE,
                          total           = NA) {
  if (.progress) {
    .mg2_scan_state$n <- .mg2_scan_state$n + 1L
    now <- proc.time()[["elapsed"]]
    if (now - .mg2_scan_state$last_t >= .mg2_scan_state$interval) {
      .mg2_scan_state$last_t <- now
      pct <- min(.mg2_scan_state$n / max(.mg2_scan_state$total, 1L), 1)
      shiny::setProgress(
        value  = pct,
        detail = sprintf("%d of %d series (%.0f%%)",
                         .mg2_scan_state$n, .mg2_scan_state$total, pct * 100)
      )
    }
  }

  y <- x

  # All-small series: skip
  if (!is.null(smallThreshold) && all(y <= smallThreshold | is.na(y))) {
    return(if (logical) rep(NA, length(y)) else y)
  }

  medianVal          <- stats::median(y, na.rm = TRUE)
  medianAbsDeviation <- stats::mad(y, na.rm = TRUE)

  # When MAD is negligible, fall back to trimmed SD × 0.6745
  if (medianAbsDeviation < .01 * medianVal) {
    q01 <- stats::quantile(x, .01, na.rm = TRUE)
    q99 <- stats::quantile(x, .99, na.rm = TRUE)
    medianAbsDeviation <- 0.6745 * stats::sd(x[x > q01 & x < q99], na.rm = TRUE)
  }

  extreme <- y > (medianVal + deviation * medianAbsDeviation) |
             y < (medianVal - deviation * medianAbsDeviation)

  if (logical) return(extreme)

  y[!extreme] <- NA
  return(y)
}


#' Detect Seasonally-Adjusted Outliers
#'
#' Uses `forecast::tsclean()` to produce a seasonally-adjusted forecast for
#' each series and flags values that deviate from the forecast by more than
#' `deviation × MAD`.
#'
#' @param x Numeric vector (one time series).
#' @param smallThreshold Numeric. Skip series with all values ≤ this
#'   (default: `100`).
#' @param deviation Numeric. MAD multiplier for outlier threshold (default: `3`).
#' @param logical Logical. Return logical flag when `TRUE`, cleaned series when
#'   `FALSE` (default: `FALSE`).
#' @param interpolate Logical. Interpolate missing values (default: `FALSE`).
#' @param .lambda Numeric. Box-Cox lambda passed to `tsclean` (default: `0.5`).
#' @param .progress Logical. Update Shiny progress bar (default: `FALSE`).
#' @param total Integer. Total series count for progress increment calculation.
#'
#' @return Logical or numeric vector the same length as `x`.
#' @export
unseasonal <- function(x,
                       smallThreshold = 100,
                       deviation      = 3,
                       logical        = FALSE,
                       interpolate    = FALSE,
                       .lambda        = 0.5,
                       .progress      = FALSE,
                       total          = NA) {
  if (.progress && !is.na(total)) {
    shiny::setProgress(detail = "Searching for seasonal outliers within each orgUnit")
    shiny::incProgress(amount = 1 / total)
  }

  if (all(is.na(x))) {
    return(if (logical) rep(NA, length(x)) else x)
  }

  if (!is.null(smallThreshold) && all(x <= smallThreshold | is.na(x))) {
    return(if (logical) rep(NA, length(x)) else x)
  }

  if (!requireNamespace("forecast", quietly = TRUE)) {
    warning("Package 'forecast' is required for seasonal outlier detection.")
    return(if (logical) rep(NA, length(x)) else x)
  }

  x.ts       <- stats::ts(x, frequency = 12)
  x.forecast <- as.numeric(forecast::tsclean(x.ts,
                                             replace.missing = interpolate,
                                             lambda          = .lambda))

  if (!logical) return(x.forecast)

  MAD     <- stats::mad(x, na.rm = TRUE)
  # as.logical() strips the "ts" class that `>= deviation` inherits when x.ts
  # is a ts object — without it, rbindlist sees a class mismatch between buckets
  # where some series were processed (class "ts") and others were skipped
  # (class "logical" from rep(NA, n)).
  outlier <- as.logical(abs((x.forecast - x.ts) / MAD) >= deviation)

  return(outlier)
}


#' Flag MAD-Based Outliers Across All Series in a tsibble
#'
#' Applies [extremely_mad()] across every org unit × data element series,
#' flagging key-entry errors, over-max values, and MAD extremes at 15×, 10×,
#' and 5× thresholds.
#'
#' @param d A tsibble (output of [data_1()]).
#' @param .total Integer. Number of series (used for progress). If `NULL`,
#'   computed automatically from `tsibble::n_keys(d)`.
#' @param .threshold Numeric. All-small threshold; series with all values ≤
#'   this are skipped (default: `50`).
#' @param key_entry_errors Numeric vector. Pre-specified key-entry error values.
#'   If `NULL`, detected automatically.
#' @param progress Logical. Show Shiny progress increments (default: `TRUE`).
#'
#' @return The input tsibble with additional logical flag columns:
#'   `key_entry_error`, `over_max`, `mad15`, `mad10`, `mad5`.
#' @export
mad_outliers <- function(d,
                         .total          = NULL,
                         .threshold      = 50,
                         key_entry_errors = NULL,
                         progress        = TRUE,
                         .progress_fn    = NULL) {
  if (is.null(.total)) {
    .total <- if (tsibble::is_tsibble(d)) tsibble::n_keys(d) else
      data.table::uniqueN(data.table::as.data.table(d), by = c("orgUnit", "data.id"))
  }

  # Initialise time-throttled progress state for this scan
  .mg2_scan_state$n      <- 0L
  .mg2_scan_state$total  <- .total
  .mg2_scan_state$last_t <- proc.time()[["elapsed"]]

  if (is.null(key_entry_errors)) {
    large_values <- dplyr::count(
      dplyr::ungroup(tibble::as_tibble(
        dplyr::filter(d, nchar(as.character(original)) > 3, effectiveLeaf)
      )),
      original
    ) |>
      dplyr::arrange(-n)

    top10_median <- median(
      dplyr::pull(dplyr::filter(large_values, dplyr::row_number() < 11), n),
      na.rm = TRUE
    )

    key_entry_errors <- dplyr::pull(
      dplyr::filter(large_values, n > 3 * top10_median),
      original
    )

    if (rlang::is_empty(key_entry_errors)) key_entry_errors <- NA
  }

  # ── data.table fast path ─────────────────────────────────────────────────
  if (requireNamespace("data.table", quietly = TRUE)) {
    if (tsibble::is_tsibble(d)) {
      key_vars <- tsibble::key_vars(d)
      idx_var  <- tsibble::index_var(d)
    } else {
      key_vars <- c("orgUnit", "data.id")
      idx_var  <- if ("Month" %in% names(d)) "Month" else "Week"
    }
    dt <- data.table::as.data.table(tibble::as_tibble(d))

    # ── Fast vectorized pre-processing (no per-group R calls) ────────────────
    dt[, .max := dplyr::if_else(
      grepl("stock", data, ignore.case = TRUE) &
        grepl("out|rupture", data, ignore.case = TRUE) &
        effectiveLeaf,
      31, NA_real_
    )]
    if (!all(is.na(key_entry_errors))) {
      dt[, key_entry_error := dplyr::if_else(!is.na(original),
                                              original %in% key_entry_errors, NA)]
    } else {
      dt[, key_entry_error := NA]
    }
    dt[, over_max := dplyr::if_else(!is.na(.max) & !is.na(original),
                                    original > .max, NA)]
    dt[, AllSmall := !is.null(.threshold) &&
         all(is.na(original) | original <= .threshold),
       by = key_vars]
    dt[, not_key_or_over_under := dplyr::if_else(
      (is.na(over_max) | !over_max) &
        (is.na(key_entry_error) | !key_entry_error) &
        !AllSmall,
      original, NA_real_
    )]

    # ── Determine parallelism ─────────────────────────────────────────────────
    use_parallel <- .total > 1L &&
      requireNamespace("furrr",  quietly = TRUE) &&
      requireNamespace("future", quietly = TRUE)
    # MAD workers only need data.table (~200 MB on multisession Windows).
    # On macOS/Linux multicore is used, so no per-worker memory cost.
    n_workers <- if (use_parallel) .mg2_n_workers(per_worker_mb = 200L) else 1L
    use_parallel <- use_parallel && n_workers > 1L

    if (is.function(.progress_fn))
      .progress_fn(sprintf("MAD: starting%s",
        if (use_parallel) sprintf(" — %d cores", n_workers) else ""))

    if (use_parallel) {
      # ── Bucket-parallel path (same pattern as seasonal_outliers) ─────────
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      old_max_size <- getOption("future.globals.maxSize")
      on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)
      options(future.globals.maxSize = 2 * 1024^3)   # 2 GB — for large bucket data.tables
      # multicore (fork) on macOS/Linux: workers inherit parent memory — no
      # package reload, no data serialization, copy-on-write overhead only.
      # multisession on Windows (fork unavailable).
      .plan_type <- if (.Platform$OS.type == "unix") future::multicore else future::multisession
      future::plan(.plan_type, workers = n_workers)

      n_buckets   <- n_workers * 10L
      unique_keys <- unique(dt[, .SD, .SDcols = key_vars])
      unique_keys[, mg2_bucket := (seq_len(.N) - 1L) %% n_buckets]
      data.table::setkeyv(dt,          key_vars)
      data.table::setkeyv(unique_keys, key_vars)
      dt[unique_keys, mg2_bucket := i.mg2_bucket]
      bucket_list <- lapply(seq_len(n_buckets) - 1L, function(b) dt[mg2_bucket == b])

      .xmad <- extremely_mad
      environment(.xmad) <- baseenv()

      .opts <- furrr::furrr_options(
        seed     = NULL,
        packages = "data.table",
        globals  = list(.xmad = .xmad, key_vars = key_vars, .threshold = .threshold)
      )

      .process_bucket_mad <- function(sub) {
        sub[, mad15 := .xmad(not_key_or_over_under, deviation = 15,
                              smallThreshold  = .threshold,
                              key_entry_error = key_entry_error,
                              over_max        = over_max,
                              maximum_allowed = .max,
                              logical         = TRUE), by = key_vars]
        sub[, mad10 := .xmad(not_key_or_over_under, deviation = 10,
                              smallThreshold  = .threshold,
                              maximum_allowed = .max,
                              logical         = TRUE), by = key_vars]
        sub[, mad5  := .xmad(not_key_or_over_under, deviation = 5,
                              smallThreshold  = .threshold,
                              maximum_allowed = .max,
                              logical         = TRUE), by = key_vars]
        sub
      }
      environment(.process_bucket_mad) <- list2env(
        list(.xmad = .xmad, key_vars = key_vars, .threshold = .threshold),
        parent = baseenv()
      )

      n_steps        <- 10L
      step_sz        <- ceiling(n_buckets / n_steps)
      bucket_results <- vector("list", n_buckets)

      for (.step in seq_len(n_steps)) {
        step_idx     <- seq((.step - 1L) * step_sz + 1L,
                            min(.step * step_sz, n_buckets))
        bucket_results[step_idx] <- suppressWarnings(
          furrr::future_map(bucket_list[step_idx], .process_bucket_mad, .options = .opts)
        )
        pct    <- min(.step / n_steps, 1)
        n_done <- round(pct * .total)
        cat(sprintf("\r    MAD: %d/%d series (%.0f%%)", n_done, .total, pct * 100))
        flush.console()
        if (is.function(.progress_fn))
          .progress_fn(sprintf("MAD: %d/%d (%.0f%%)", n_done, .total, pct * 100))
      }
      cat("\n")

      dt <- data.table::rbindlist(bucket_results, use.names = TRUE, fill = TRUE)
      dt[, mg2_bucket := NULL]

      # Restore index class if rbindlist stripped it
      if (idx_var %in% names(dt)) {
        idx_cls <- if (idx_var == "Month") "yearmonth" else "yearweek"
        if (!inherits(dt[[idx_var]], idx_cls))
          data.table::set(dt, j = idx_var,
                          value = structure(as.numeric(dt[[idx_var]]), class = idx_cls))
      }

    } else {
      # ── Sequential path ───────────────────────────────────────────────────
      dt[, mad15 := extremely_mad(
        not_key_or_over_under, deviation = 15, smallThreshold = .threshold,
        key_entry_error = key_entry_error, over_max = over_max,
        maximum_allowed = .max, logical = TRUE,
        .progress = progress, total = .total
      ), by = key_vars]
      dt[, mad10 := extremely_mad(
        not_key_or_over_under, deviation = 10, smallThreshold = .threshold,
        maximum_allowed = .max, logical = TRUE, .progress = FALSE
      ), by = key_vars]
      dt[, mad5  := extremely_mad(
        not_key_or_over_under, deviation = 5, smallThreshold = .threshold,
        maximum_allowed = .max, logical = TRUE, .progress = FALSE
      ), by = key_vars]
    }

    m_o <- tsibble::as_tsibble(
      tibble::as_tibble(dt),
      key   = dplyr::all_of(key_vars),
      index = dplyr::all_of(idx_var)
    )
    return(m_o)
  }

  # ── Fallback: original tsibble grouped-mutate path ────────────────────────
  m_o <- d |>
    tsibble::group_by_key() |>
    dplyr::mutate(
      .max = dplyr::if_else(
        grepl("stock", data, ignore.case = TRUE) &
          grepl("out|rupture", data, ignore.case = TRUE) &
          effectiveLeaf,
        31, NA_real_
      ),
      key_entry_error = dplyr::if_else(
        !all(is.na(key_entry_errors)) & !is.na(original),
        original %in% key_entry_errors,
        NA
      ),
      over_max = dplyr::if_else(
        !is.na(.max) & !is.na(original),
        original > .max,
        NA
      ),
      AllSmall = !is.null(.threshold) &&
        all(is.na(original) | original <= .threshold),
      not_key_or_over_under = dplyr::if_else(
        (is.na(over_max) | !over_max) &
          (is.na(key_entry_error) | !key_entry_error) &
          !AllSmall,
        original, NA_real_
      ),
      mad15 = extremely_mad(
        not_key_or_over_under,
        deviation       = 15,
        smallThreshold  = .threshold,
        key_entry_error = key_entry_error,
        over_max        = over_max,
        maximum_allowed = .max,
        logical         = TRUE,
        .progress       = progress,
        total           = .total
      ),
      mad10 = extremely_mad(
        not_key_or_over_under,
        deviation       = 10,
        smallThreshold  = .threshold,
        maximum_allowed = .max,
        logical         = TRUE,
        .progress       = FALSE
      ),
      mad5 = extremely_mad(
        not_key_or_over_under,
        deviation       = 5,
        smallThreshold  = .threshold,
        maximum_allowed = .max,
        logical         = TRUE,
        .progress       = FALSE
      )
    )

  return(m_o)
}


#' Flag Seasonal Outliers Across All Series in a tsibble
#'
#' Applies [unseasonal()] to every series, flagging values that deviate from a
#' seasonal forecast by more than 5× MAD (`seasonal5`) and 3× MAD
#' (`seasonal3`).
#'
#' @param d A tsibble (output of [mad_outliers()]).
#' @param .total Integer. Number of series (used for progress). If `NULL`,
#'   computed from `tsibble::n_keys(d)`.
#' @param .threshold Numeric. All-small threshold (default: `50`).
#' @param mad Character. Name of MAD flag column to exclude before seasonal
#'   detection (default: `"mad10"`).
#' @param tests Character vector. Which seasonal tests to run; subset of
#'   `c("seasonal5", "seasonal3")` (default: both).
#' @param progress Logical. Use `progressr` progress bar (default: `FALSE`).
#'
#' @return The input tsibble with `not_mad`, `seasonal5`, and `seasonal3`
#'   columns added.
#' @export
seasonal_outliers <- function(d,
                              .total         = NULL,
                              .threshold     = 50,
                              mad            = "mad10",
                              tests          = c("seasonal5", "seasonal3"),
                              progress       = FALSE,
                              shiny_progress = FALSE,
                              .progress_fn   = NULL) {

  if (tsibble::is_tsibble(d)) {
    key_vars <- tsibble::key_vars(d)
    idx_var  <- tsibble::index_var(d)
  } else {
    key_vars <- c("orgUnit", "data.id")
    idx_var  <- if ("Month" %in% names(d)) "Month" else "Week"
  }

  # Convert to data.table once; all operations below stay in data.table
  dt <- data.table::as.data.table(tibble::as_tibble(d))

  if (is.null(.total))
    .total <- data.table::uniqueN(dt, by = key_vars)

  # Determine whether parallel execution is available
  use_parallel <- .total > 1L &&
    requireNamespace("furrr",  quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)

  # Seasonal workers load forecast (~400 MB on multisession Windows).
  # On macOS/Linux multicore is used, so no per-worker memory cost.
  n_workers <- if (use_parallel) .mg2_n_workers(per_worker_mb = 400L) else 1L
  use_parallel <- use_parallel && n_workers > 1L

  if (is.function(.progress_fn))
    .progress_fn(sprintf("starting%s",
      if (use_parallel) sprintf(" — %d cores", n_workers) else ""))

  # Capture unseasonal() for workers — strip MG2 namespace so workers don't
  # attempt to load the package; all calls inside use :: notation or base/stats.
  .unseasonal <- unseasonal
  environment(.unseasonal) <- baseenv()

  # ── Vectorised not_mad (row-level, no group context needed) ──────────────
  # Compute once globally so workers receive it pre-filled rather than
  # recomputing it per series inside the by expression.
  mad_vec <- dt[[mad]]
  dt[, not_mad := data.table::fifelse(!mad_vec, original, NA_real_)]

  if (use_parallel) {
    # ── Bucket-parallel path ────────────────────────────────────────────────
    # Problem with the old per-series split: splitting into .total (~149k) R
    # objects and then rbind-ing them back is the dominant bottleneck —
    # dplyr/vctrs type-checks every one of them.
    #
    # New approach: create (n_workers × 10) coarse buckets so each furrr call
    # dispatches exactly n_workers tasks.  Each task receives one compact
    # data.table (~.total / n_buckets rows) and applies tsclean() per key via
    # data.table `by`.  Final rbindlist over n_buckets objects is instant.
    #
    # Object count: n_buckets (e.g. 80)  vs old: .total (e.g. 149 176)
    # rbindlist calls: 1 over n_buckets  vs old: 1 over .total

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    old_max_size <- getOption("future.globals.maxSize")
    on.exit(options(future.globals.maxSize = old_max_size), add = TRUE)
    options(future.globals.maxSize = 2 * 1024^3)   # 2 GB — for large bucket data.tables
    # multicore (fork) on macOS/Linux: workers inherit parent memory — forecast
    # is not reloaded, bucket data is not serialized, copy-on-write only.
    # multisession on Windows (fork unavailable).
    .plan_type <- if (.Platform$OS.type == "unix") future::multicore else future::multisession
    future::plan(.plan_type, workers = n_workers)

    n_buckets <- n_workers * 10L

    # Assign a bucket ID to each unique key, then join to all rows
    unique_keys <- unique(dt[, .SD, .SDcols = key_vars])
    unique_keys[, mg2_bucket := (seq_len(.N) - 1L) %% n_buckets]
    data.table::setkeyv(dt,          key_vars)
    data.table::setkeyv(unique_keys, key_vars)
    dt[unique_keys, mg2_bucket := i.mg2_bucket]

    # Pre-split into n_buckets data.tables (cheap — no per-series overhead)
    bucket_list <- lapply(seq_len(n_buckets) - 1L, function(b) dt[mg2_bucket == b])

    .opts <- furrr::furrr_options(
      seed     = NULL,
      packages = c("forecast", "data.table"),
      globals  = list(
        .unseasonal = .unseasonal,
        key_vars    = key_vars,
        .threshold  = .threshold,
        tests       = tests
      )
    )

    # Worker: apply tsclean() per key within the bucket using data.table by
    .process_bucket <- function(sub) {
      sub[, c("seasonal5", "seasonal3", "expected") := {
        nm <- not_mad
        list(
          if ("seasonal5" %in% tests)
            .unseasonal(nm, smallThreshold = .threshold, deviation = 5, logical = TRUE)
          else
            rep(NA, .N),
          if ("seasonal3" %in% tests)
            .unseasonal(nm, smallThreshold = .threshold, deviation = 3, logical = TRUE)
          else
            rep(NA, .N),
          if (any(c("seasonal5", "seasonal3") %in% tests))
            .unseasonal(nm, smallThreshold = .threshold, deviation = 3, logical = FALSE)
          else
            rep(NA_real_, .N)
        )
      }, by = key_vars]
      sub
    }
    # Strip closure to prevent furrr serializing the entire call frame
    environment(.process_bucket) <- list2env(
      list(.unseasonal = .unseasonal, key_vars = key_vars,
           .threshold = .threshold, tests = tests),
      parent = baseenv()
    )

    # Process in 10 rounds of n_workers buckets each — gives 10 progress ticks
    n_steps      <- 10L
    step_sz      <- ceiling(n_buckets / n_steps)   # buckets per round
    bucket_results <- vector("list", n_buckets)

    for (.step in seq_len(n_steps)) {
      step_idx <- seq(
        from = (.step - 1L) * step_sz + 1L,
        to   = min(.step * step_sz, n_buckets)
      )
      step_results <- suppressWarnings(
        furrr::future_map(bucket_list[step_idx], .process_bucket, .options = .opts)
      )
      bucket_results[step_idx] <- step_results

      pct    <- min(.step / n_steps, 1)
      n_done <- round(pct * .total)
      cat(sprintf("\r    seasonal: %d/%d series (%.0f%%)", n_done, .total, pct * 100))
      flush.console()
      if (is.function(.progress_fn))
        .progress_fn(sprintf("seasonal: %d/%d (%.0f%%)", n_done, .total, pct * 100))
    }
    cat("\n")

    if (is.function(.progress_fn))
      .progress_fn(sprintf("seasonal: %d series — building tsibble", .total))

    # Combine: n_buckets objects instead of .total — fast
    cat(sprintf("  combining %d bucket results...\n", n_buckets))
    flush.console()

    dt_result <- data.table::rbindlist(bucket_results, use.names = TRUE, fill = TRUE)
    dt_result[, mg2_bucket := NULL]

  } else {
    # ── Sequential path: data.table by (no split/bind overhead) ─────────────
    # For small datasets (n_workers == 1) there is no parallelism benefit, so
    # we apply tsclean() per key directly via data.table's by engine.
    dt[, c("seasonal5", "seasonal3", "expected") := {
      nm <- not_mad
      list(
        if ("seasonal5" %in% tests)
          .unseasonal(nm, smallThreshold = .threshold, deviation = 5, logical = TRUE)
        else
          rep(NA, .N),
        if ("seasonal3" %in% tests)
          .unseasonal(nm, smallThreshold = .threshold, deviation = 3, logical = TRUE)
        else
          rep(NA, .N),
        if (any(c("seasonal5", "seasonal3") %in% tests))
          .unseasonal(nm, smallThreshold = .threshold, deviation = 3, logical = FALSE)
        else
          rep(NA_real_, .N)
      )
    }, by = key_vars]

    if (shiny_progress)
      shiny::setProgress(value = 1, detail = sprintf("%d series complete", .total))

    dt_result <- dt
  }

  # Reconstruct tsibble
  cat("  rebuilding tsibble...\n")
  flush.console()
  if (shiny_progress)
    shiny::setProgress(detail = "building result...")

  data1.seasonal <- tibble::as_tibble(dt_result) |>
    tsibble::as_tsibble(
      key   = dplyr::all_of(key_vars),
      index = dplyr::all_of(idx_var)
    )

  return(data1.seasonal)
}
