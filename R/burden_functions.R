# Burden estimation functions
# Each function returns list($subnational, $national)  -  both data.tables with
# columns: method, year, region, category, estimate, lower, upper

# -- helpers ------------------------------------------------------------------

# Draw `n_fac` independent samples from `x` and sum them, repeated `n_boot`
# times.  Summing independent draws gives variance proportional to n_fac
# (correct), not n_fac^2 (the bug that results from multiplying a single draw).
.burden_resample_sum <- function(x, n_fac, n_boot) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0L || n_fac == 0L) return(rep(0, n_boot))
  if (n_fac == 1L) return(sample(x, n_boot, replace = TRUE))
  # Efficient: draw an n_fac x n_boot matrix and colSums
  colSums(matrix(sample(x, n_fac * n_boot, replace = TRUE), nrow = n_fac))
}

# Legacy scalar sampler kept for places that don't need the sum-of-n form.
.burden_resample <- function(x, n) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0L) return(rep(0, n))
  sample(x, n, replace = TRUE)
}

# Build facility universe from the FULL data (all elements), not just the
# target element. Gives the complete orgUnit -> region -> Selected mapping.
.fac_universe <- function(data, region_col) {
  dt <- data.table::as.data.table(data)
  univ <- unique(dt[!is.na(get(region_col)),
                    c("orgUnit", region_col, "Selected"), with = FALSE])
  univ
}

.burden_ci <- function(samples) {
  list(
    estimate = as.integer(round(mean(samples))),
    lower    = as.integer(round(stats::quantile(samples, 0.025, names = FALSE))),
    upper    = as.integer(round(stats::quantile(samples, 0.975, names = FALSE)))
  )
}

.national_from_regions <- function(region_sample_list, method, cat) {
  valid <- Filter(Negate(is.null), region_sample_list)
  if (length(valid) == 0L) return(NULL)
  nat <- Reduce("+", valid)
  ci  <- .burden_ci(nat)
  data.frame(method = method, region = "National", category = cat,
             estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
             stringsAsFactors = FALSE)
}

.champ_resample_fallback <- function(ann_cat_yr, region_col, reg, n) {
  # Try regional champions; fall back to national champions
  champ <- ann_cat_yr[get(region_col) == reg & Selected == "Champion", total]
  champ <- champ[champ > 0]
  if (length(champ) == 0L) {
    champ <- ann_cat_yr[Selected == "Champion", total]
    champ <- champ[champ > 0]
  }
  champ
}

# -- format helper (exported) -------------------------------------------------

#' Format a burden estimate as "N \[L - U\]"
#' @param estimate Numeric. Point estimate.
#' @param lower Numeric. Lower bound of the confidence interval.
#' @param upper Numeric. Upper bound of the confidence interval.
#' @return Character string formatted as `"N [lower-upper]"`.
#' @export
format_burden_estimate <- function(estimate, lower, upper) {
  fmt <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")
  paste0(fmt(estimate), " [", fmt(lower), "-", fmt(upper), "]")
}

# -- Method A: Champion Multiple -----------------------------------------------

#' Burden Method A: Champion Multiple
#'
#' For each region and category, sample from the champion annual-total
#' distribution to predict non-champion facility totals.
#'
#' @param data data.table from `selectedData()` (columns: orgUnit, Month,
#'   data, value, Selected, and a region column).
#' @param target_elements character; which values of the `data` column to use.
#' @param region_col character; name of the region column (typically
#'   `levelNames[2]`).
#' @param period_start yearmonth; start of the estimate window (NULL = all data).
#' @param period_end yearmonth; end of the estimate window (NULL = all data).
#' @param neighbor_list named list; maps each region to a character vector of
#'   neighbouring region names used as a fallback when a region has no
#'   champion facilities.
#' @param n_bootstrap integer; bootstrap draws (default 1000).
#' @return list with `$subnational` and `$national` data.tables.
#' @export
burden_a <- function(data, target_elements, region_col,
                     period_start = NULL, period_end = NULL,
                     neighbor_list = NULL,
                     n_bootstrap = 1000L) {
  dt <- data.table::as.data.table(data)

  fac_univ <- .fac_universe(dt, region_col)
  regions  <- sort(unique(fac_univ[[region_col]]))

  dt_tgt <- dt[get("data") %in% target_elements]
  if (nrow(dt_tgt) == 0L) return(NULL)

  if (!is.null(period_start)) dt_tgt <- dt_tgt[Month >= period_start]
  if (!is.null(period_end))   dt_tgt <- dt_tgt[Month <= period_end]
  if (nrow(dt_tgt) == 0L) return(NULL)

  fac_totals <- dt_tgt[!is.na(value),
    .(total = sum(value, na.rm = TRUE)),
    by = c("orgUnit", "data")
  ]

  sub_rows <- nat_rows <- list()

  for (cat in target_elements) {
    cat_totals <- fac_totals[get("data") == cat]
    reg_samps  <- vector("list", length(regions))
    names(reg_samps) <- regions

    # Pre-build national champion vector once per category
    nat_champ_ous <- fac_univ[Selected == "Champion", orgUnit]

    build_champ_vec <- function(ous) {
      tbl <- merge(data.table::data.table(orgUnit = ous),
                   cat_totals[, .(orgUnit, total)],
                   by = "orgUnit", all.x = TRUE)
      tbl[is.na(total), total := 0L]
      tbl$total
    }

    # Fallback hierarchy: neighbors -> national -> zero.
    # Returns a numeric vector (champion per-facility totals) from the best
    # available reference population, or NULL if nothing is available.
    get_fallback_vec <- function(reg) {
      # 1. Neighboring provinces
      if (!is.null(neighbor_list) && reg %in% names(neighbor_list)) {
        nbr_regs <- neighbor_list[[reg]]
        nbr_ous  <- fac_univ[get(region_col) %in% nbr_regs &
                              Selected == "Champion", orgUnit]
        if (length(nbr_ous) > 0L) {
          v <- build_champ_vec(nbr_ous)
          if (sum(v) > 0L) return(v)
        }
      }
      # 2. National
      if (length(nat_champ_ous) > 0L) {
        v <- build_champ_vec(nat_champ_ous)
        if (sum(v) > 0L) return(v)
      }
      NULL
    }

    for (reg in regions) {
      reg_facs  <- fac_univ[get(region_col) == reg]
      champ_ous <- reg_facs[Selected == "Champion", orgUnit]
      nc_n      <- nrow(reg_facs[Selected != "Champion"])

      needs_fallback <- length(champ_ous) == 0L

      if (!needs_fallback) {
        champ_vec <- build_champ_vec(champ_ous)
        champ_sum <- sum(champ_vec)
        # All regional champions had 0 for this element  -  no signal locally
        if (champ_sum == 0L) needs_fallback <- TRUE
      }

      if (needs_fallback) {
        fb <- get_fallback_vec(reg)
        if (is.null(fb)) {
          reg_samps[[reg]] <- rep(0, n_bootstrap)
          next
        }
        n_all <- nrow(reg_facs)
        samps <- .burden_resample_sum(fb, n_all, n_bootstrap)
        reg_samps[[reg]] <- samps
        ci <- .burden_ci(samps)
        sub_rows[[length(sub_rows) + 1L]] <- data.frame(
          method = "A", region = reg, category = cat,
          estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
          stringsAsFactors = FALSE
        )
        next
      }

      # Sum nc_n independent draws from champion distribution (includes 0s)
      samps            <- champ_sum + .burden_resample_sum(champ_vec, nc_n, n_bootstrap)
      reg_samps[[reg]] <- samps

      ci <- .burden_ci(samps)
      sub_rows[[length(sub_rows) + 1L]] <- data.frame(
        method = "A", region = reg, category = cat,
        estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
        stringsAsFactors = FALSE
      )
    }

    nr <- .national_from_regions(reg_samps, "A", cat)
    if (!is.null(nr)) nat_rows[[length(nat_rows) + 1L]] <- nr
  }

  if (length(sub_rows) == 0L) return(NULL)
  list(
    subnational = data.table::rbindlist(sub_rows, fill = TRUE),
    national    = data.table::rbindlist(nat_rows, fill = TRUE)
  )
}

# -- Method B: Attendance-Based Champion Multiple ------------------------------

#' Burden Method B: Attendance-Based Champion Multiple
#'
#' Fits a log-log regression (cases ~ attendance) on champion facilities and
#' predicts non-champion totals from their attendance.
#'
#' @param data data.table from `selectedData()`.
#' @param target_elements character; target `data` column values.
#' @param attendance_elements character; attendance `data` column values.
#' @param cat_map named character vector: names = target categories, values =
#'   corresponding attendance categories. `NULL` -> single attendance element
#'   used for all target categories (or 1:1 by position).
#' @param region_col character; region column name.
#' @param n_bootstrap integer; bootstrap draws.
#' @return list with `$subnational` and `$national` data.tables.
#' @export
burden_b <- function(data, target_elements, attendance_elements,
                     cat_map = NULL, region_col,
                     n_bootstrap = 1000L) {
  dt <- data.table::as.data.table(data)

  # Default category map
  if (is.null(cat_map)) {
    if (length(attendance_elements) == 1L) {
      cat_map <- stats::setNames(rep(attendance_elements, length(target_elements)),
                                 target_elements)
    } else {
      n <- min(length(target_elements), length(attendance_elements))
      cat_map <- stats::setNames(attendance_elements[seq_len(n)], target_elements[seq_len(n)])
    }
  }

  tgt_ann <- dt[get("data") %in% target_elements & !is.na(value),
    .(total = sum(value, na.rm = TRUE)),
    by = c("orgUnit", "data", region_col, "Selected")
  ]
  att_ann <- dt[get("data") %in% attendance_elements & !is.na(value),
    .(attend_total = sum(value, na.rm = TRUE)),
    by = c("orgUnit", "data")
  ]

  sub_rows <- nat_rows <- list()

  for (cat in target_elements) {
      att_cat <- cat_map[[cat]]
      if (is.null(att_cat) || is.na(att_cat)) next

      tgt_sub <- tgt_ann[get("data") == cat]
      att_sub <- att_ann[get("data") == att_cat]

      # Champions with both target and attendance
      champ_joined <- merge(
        tgt_sub[Selected == "Champion"],
        att_sub[, .(orgUnit, attend_total)],
        by = "orgUnit"
      )
      champ_joined <- champ_joined[total > 0 & attend_total > 0]

      if (nrow(champ_joined) < 3L) next

      fit <- tryCatch(
        stats::lm(log(total) ~ log(attend_total), data = champ_joined),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      alpha <- stats::coef(fit)[[1L]]
      beta  <- stats::coef(fit)[[2L]]
      sigma <- stats::sigma(fit)

      regions <- unique(tgt_sub[[region_col]])
      reg_samps <- vector("list", length(regions))
      names(reg_samps) <- regions

      for (reg in regions) {
        champ_sum <- sum(tgt_sub[get(region_col) == reg & Selected == "Champion",
                                 total], na.rm = TRUE)

        nc_tgt <- tgt_sub[get(region_col) == reg & Selected == "Non-Champion"]
        nc_att <- att_sub[orgUnit %in% nc_tgt$orgUnit]

        nc_samps <- rep(0, n_bootstrap)

        # Facilities with attendance data: predict from regression
        for (i in seq_len(nrow(nc_att))) {
          a <- nc_att$attend_total[i]
          if (!is.finite(a) || a <= 0) next
          eps   <- stats::rnorm(n_bootstrap, 0, sigma)
          preds <- exp(alpha + beta * log(a) + eps)
          nc_samps <- nc_samps + pmax(0, preds)
        }

        # Facilities without attendance: fall back to champion resampling
        n_no_att <- nrow(nc_tgt) - nrow(nc_att)
        if (n_no_att > 0L) {
          champ_tot <- .champ_resample_fallback(tgt_sub, region_col, reg, n_bootstrap)
          if (length(champ_tot) > 0L)
            nc_samps <- nc_samps + n_no_att * .burden_resample(champ_tot, n_bootstrap)
        }

        samps <- champ_sum + nc_samps
        reg_samps[[reg]] <- samps
        ci <- .burden_ci(samps)
        sub_rows[[length(sub_rows) + 1L]] <- data.frame(
          method = "B", region = reg, category = cat,
          estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
          stringsAsFactors = FALSE
        )
      }

      nr <- .national_from_regions(reg_samps, "B", cat)
      if (!is.null(nr)) nat_rows[[length(nat_rows) + 1L]] <- nr
  }

  if (length(sub_rows) == 0L) return(NULL)
  list(
    subnational = data.table::rbindlist(sub_rows, fill = TRUE),
    national    = data.table::rbindlist(nat_rows, fill = TRUE)
  )
}

# -- Method C1: Facility-Level Linear Imputation -------------------------------

#' Burden Method C1: Facility-Level Imputation (Linear Regression)
#'
#' For each facility, fits a linear model `value ~ champion_mean` on observed
#' months (using the full history), then imputes missing months in the target
#' year using the model's prediction distribution.  Facilities with too few
#' observations are flagged as not modeled.
#'
#' @param data data.table from `selectedData()`  -  the **full available history**,
#'   not pre-filtered to the estimate period.  The full history improves model
#'   fitting; summation is restricted to \[period_start, period_end\].
#' @param target_elements character; target `data` values.
#' @param region_col character; region column name.
#' @param period_start,period_end yearmonth; start/end of the estimate period.
#'   When NULL the entire supplied dataset is both fitted and summed.
#' @param min_obs integer; minimum observed months to attempt fitting (default 3).
#' @param n_bootstrap integer; bootstrap draws.
#' @return list with `$subnational`, `$national`, and `$not_modeled`.
#' @export
burden_c1 <- function(data, target_elements, region_col,
                      period_start = NULL, period_end = NULL,
                      min_obs = 3L, n_bootstrap = 1000L) {
  dt <- data.table::as.data.table(data)
  dt[, .month_int := lubridate::month(Month)]

  # Facility universe from ALL elements  -  ground truth for who exists
  fac_univ <- .fac_universe(dt, region_col)
  regions  <- sort(unique(fac_univ[[region_col]]))

  dt_tgt <- dt[get("data") %in% target_elements]
  if (nrow(dt_tgt) == 0L) return(NULL)

  # Champion monthly means use the FULL history for stable seasonal estimates.
  champ_reg <- dt_tgt[Selected == "Champion" & !is.na(value),
    .(champ_mean = mean(value, na.rm = TRUE)),
    by = c(region_col, "data", ".month_int")
  ]
  champ_nat <- dt_tgt[Selected == "Champion" & !is.na(value),
    .(champ_nat = mean(value, na.rm = TRUE)),
    by = c("data", ".month_int")
  ]

  dt_period <- if (!is.null(period_start) || !is.null(period_end)) {
    tmp <- dt_tgt
    if (!is.null(period_start)) tmp <- tmp[Month >= period_start]
    if (!is.null(period_end))   tmp <- tmp[Month <= period_end]
    tmp
  } else dt_tgt

  sub_rows <- nat_rows <- list()
  not_modeled <- list()

  for (cat in target_elements) {
    dt_sub    <- dt_tgt[get("data") == cat]
    dt_sub_p  <- dt_period[get("data") == cat]
    if (nrow(dt_sub_p) == 0L) next

    reg_samps <- vector("list", length(regions))
    names(reg_samps) <- regions

    for (reg in regions) {
      reg_facs  <- fac_univ[get(region_col) == reg]
      fac_samps <- rep(0, n_bootstrap)

      # Facilities with any target data in the period  -  impute missing months
      facilities <- unique(dt_sub_p[get(region_col) == reg, orgUnit])

      for (fac in facilities) {
        fac_hist <- dt_sub[orgUnit == fac]           # full history

        # Attach champion monthly means; fill with national fallback
        fac_hist <- merge(fac_hist,
          champ_reg[get(region_col) == reg & get("data") == cat,
                    .(.month_int, champ_mean)],
          by = ".month_int", all.x = TRUE)
        fac_hist <- merge(fac_hist,
          champ_nat[get("data") == cat, .(.month_int, champ_nat)],
          by = ".month_int", all.x = TRUE)
        fac_hist[is.na(champ_mean), champ_mean := champ_nat]

        obs <- fac_hist[!is.na(value) & !is.na(champ_mean) & is.finite(champ_mean)]

        if (nrow(obs) < min_obs) {
          not_modeled[[length(not_modeled) + 1L]] <-
            data.frame(orgUnit = fac, category = cat, stringsAsFactors = FALSE)
          # Still credit actual reported values so that C1 >= reported.
          # Missing months are not imputed (treated as 0 = conservative).
          actual <- dt_sub_p[orgUnit == fac & get("data") == cat, value]
          fac_samps <- fac_samps + sum(actual, na.rm = TRUE)
          next
        }

        # Fit on log1p scale so predictions are always non-negative after
        # back-transformation with expm1().  This prevents implausible negative
        # confidence interval bounds that arise on the raw scale when sigma is
        # large relative to the mean (common for sparse facility data).
        fit <- tryCatch(
          stats::lm(log1p(value) ~ champ_mean, data = obs), error = function(e) NULL
        )
        if (is.null(fit)) {
          not_modeled[[length(not_modeled) + 1L]] <-
            data.frame(orgUnit = fac, category = cat, stringsAsFactors = FALSE)
          actual <- dt_sub_p[orgUnit == fac & get("data") == cat, value]
          fac_samps <- fac_samps + sum(actual, na.rm = TRUE)
          next
        }
        a_coef <- stats::coef(fit)[[1L]]
        b_coef <- stats::coef(fit)[[2L]]
        sigma  <- stats::sigma(fit)

        fac_data <- dt_sub_p[orgUnit == fac & get("data") == cat]
        fac_data <- merge(fac_data,
          fac_hist[, .(Month, .month_int, champ_mean, champ_nat)],
          by = c("Month", ".month_int"), all.x = TRUE)
        fac_data[is.na(champ_mean), champ_mean := champ_nat]

        ann_samps <- rep(0, n_bootstrap)
        for (i in seq_len(nrow(fac_data))) {
          if (!is.na(fac_data$value[i])) {
            ann_samps <- ann_samps + fac_data$value[i]
          } else {
            cm <- fac_data$champ_mean[i]
            if (!is.na(cm) && is.finite(cm)) {
              log_pred <- a_coef + b_coef * cm
              drawn    <- pmax(0, expm1(stats::rnorm(n_bootstrap, log_pred, sigma)))
              ann_samps <- ann_samps + drawn
            }
          }
        }
        fac_samps <- fac_samps + ann_samps
      }

      # Facilities in the region with NO target data at all:
      #   Champions -> assumed true zero (add nothing)
      #   Non-champions -> extrapolate using the champion per-facility distribution
      no_data_facs <- reg_facs[!orgUnit %in% facilities]
      nc_no_data   <- nrow(no_data_facs[Selected != "Champion"])

      if (nc_no_data > 0L) {
        # Champion per-facility totals over the period (0 for champions with no data)
        champ_ous_reg <- reg_facs[Selected == "Champion", orgUnit]
        if (length(champ_ous_reg) > 0L) {
          champ_period_totals <- dt_sub_p[orgUnit %in% champ_ous_reg & !is.na(value),
            .(total = sum(value, na.rm = TRUE)), by = "orgUnit"
          ]
          champ_tbl <- merge(
            data.table::data.table(orgUnit = champ_ous_reg),
            champ_period_totals, by = "orgUnit", all.x = TRUE
          )
          champ_tbl[is.na(total), total := 0L]
          champ_dist <- champ_tbl$total
        } else {
          # No regional champions -> use national
          champ_dist_nat <- dt_sub_p[Selected == "Champion" & !is.na(value),
            .(total = sum(value, na.rm = TRUE)), by = "orgUnit"
          ]$total
          champ_dist <- if (length(champ_dist_nat) > 0L) champ_dist_nat else 0L
        }
        fac_samps <- fac_samps + .burden_resample_sum(champ_dist, nc_no_data, n_bootstrap)
      }

      reg_samps[[reg]] <- fac_samps
      ci <- .burden_ci(fac_samps)
      sub_rows[[length(sub_rows) + 1L]] <- data.frame(
        method = "C1", region = reg, category = cat,
        estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
        stringsAsFactors = FALSE
      )
    }

    nr <- .national_from_regions(reg_samps, "C1", cat)
    if (!is.null(nr)) nat_rows[[length(nat_rows) + 1L]] <- nr
  }

  if (length(sub_rows) == 0L) return(NULL)
  list(
    subnational = data.table::rbindlist(sub_rows, fill = TRUE),
    national    = data.table::rbindlist(nat_rows, fill = TRUE),
    not_modeled = if (length(not_modeled)) data.table::rbindlist(not_modeled) else NULL
  )
}

# -- Method C2: Facility-Level ARIMA Imputation --------------------------------

#' Burden Method C2: Facility-Level Imputation (ARIMA)
#'
#' Fits `auto.arima` on each facility's full monthly series with the
#' regional champion mean as an external regressor.  Requires \pkg{forecast}.
#' Falls back to C1 (linear) when the series is too short (< `min_months`)
#' or fitting fails.
#'
#' @param data data.table from `selectedData()`  -  the **full available history**.
#' @param target_elements character; target `data` values.
#' @param region_col character; region column name.
#' @param period_start,period_end yearmonth; estimate period window (NULL = all data).
#' @param min_months integer; minimum months of history for ARIMA (default 12).
#' @param n_bootstrap integer; simulation draws.
#' @return list with `$subnational`, `$national`, and `$not_modeled`.
#' @export
burden_c2 <- function(data, target_elements, region_col,
                      period_start = NULL, period_end = NULL,
                      min_months = 12L, n_bootstrap = 1000L) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    message("Package 'forecast' required for C2; falling back to C1.")
    res <- burden_c1(data, target_elements, region_col,
                     period_start = period_start, period_end = period_end,
                     n_bootstrap = n_bootstrap)
    if (!is.null(res)) {
      res$subnational[, method := "C2"]
      res$national[,    method := "C2"]
    }
    return(res)
  }

  dt <- data.table::as.data.table(data)
  dt[, .month_int := lubridate::month(Month)]

  # Facility universe from ALL elements
  fac_univ <- .fac_universe(dt, region_col)
  regions  <- sort(unique(fac_univ[[region_col]]))

  dt_tgt <- dt[get("data") %in% target_elements]
  if (nrow(dt_tgt) == 0L) return(NULL)

  dt_period <- if (!is.null(period_start) || !is.null(period_end)) {
    tmp <- dt_tgt
    if (!is.null(period_start)) tmp <- tmp[Month >= period_start]
    if (!is.null(period_end))   tmp <- tmp[Month <= period_end]
    tmp
  } else dt_tgt

  # Champion monthly means use FULL history
  champ_reg <- dt_tgt[Selected == "Champion" & !is.na(value),
    .(champ_mean = mean(value, na.rm = TRUE)),
    by = c(region_col, "data", ".month_int")
  ]
  champ_nat <- dt_tgt[Selected == "Champion" & !is.na(value),
    .(champ_nat = mean(value, na.rm = TRUE)),
    by = c("data", ".month_int")
  ]

  sub_rows <- nat_rows <- list()
  not_modeled <- list()

  for (cat in target_elements) {
    dt_sub   <- dt_tgt[get("data") == cat]
    dt_sub_p <- dt_period[get("data") == cat]
    if (nrow(dt_sub_p) == 0L) next

    reg_samps <- vector("list", length(regions))
    names(reg_samps) <- regions

    for (reg in regions) {
      reg_facs   <- fac_univ[get(region_col) == reg]
      facilities <- unique(dt_sub_p[get(region_col) == reg, orgUnit])
      fac_samps  <- rep(0, n_bootstrap)

      for (fac in facilities) {
        # Full history: used for champion means and ARIMA/linear model fitting
        fac_hist <- dt_sub[orgUnit == fac]

        fac_hist <- merge(fac_hist,
          champ_reg[get(region_col) == reg & get("data") == cat,
                    .(.month_int, champ_mean)],
          by = ".month_int", all.x = TRUE)
        fac_hist <- merge(fac_hist,
          champ_nat[get("data") == cat, .(.month_int, champ_nat)],
          by = ".month_int", all.x = TRUE)
        fac_hist[is.na(champ_mean), champ_mean := champ_nat]
        data.table::setorder(fac_hist, Month)

        n_obs_hist <- sum(!is.na(fac_hist$value))
        use_arima  <- n_obs_hist >= min_months

        if (use_arima) {
          # Fit on log1p scale to keep predictions non-negative after expm1()
          ts_vals  <- stats::ts(log1p(fac_hist$value), frequency = 12)
          xreg_all <- matrix(fac_hist$champ_mean, ncol = 1)
          xreg_all[is.na(xreg_all)] <- 0

          fit_arima <- tryCatch(
            forecast::auto.arima(ts_vals, xreg = xreg_all,
                                 seasonal = TRUE, stepwise = TRUE,
                                 approximation = TRUE, allowdrift = FALSE),
            error = function(e) NULL
          )

          if (!is.null(fit_arima)) {
            fitted_all  <- tryCatch(as.numeric(fitted(fit_arima)), error = function(e) NULL)
            sigma_arima <- sqrt(fit_arima$sigma2)

            if (!is.null(fitted_all) && sigma_arima > 0) {
              if (length(fitted_all) < nrow(fac_hist)) {
                fitted_all <- c(rep(NA_real_, nrow(fac_hist) - length(fitted_all)),
                                fitted_all)
              }

              fac_hist[, .fitted := fitted_all[seq_len(.N)]]

              fac_period_rows <- if (!is.null(period_start) || !is.null(period_end)) {
                tmp <- fac_hist
                if (!is.null(period_start)) tmp <- tmp[Month >= period_start]
                if (!is.null(period_end))   tmp <- tmp[Month <= period_end]
                tmp
              } else fac_hist

              ann_samps <- rep(0, n_bootstrap)
              for (i in seq_len(nrow(fac_period_rows))) {
                if (!is.na(fac_period_rows$value[i])) {
                  ann_samps <- ann_samps + fac_period_rows$value[i]
                } else {
                  fv <- fac_period_rows$.fitted[i]   # on log1p scale
                  if (!is.na(fv) && is.finite(fv)) {
                    drawn <- pmax(0, expm1(stats::rnorm(n_bootstrap, fv, sigma_arima)))
                    ann_samps <- ann_samps + drawn
                  }
                }
              }
              fac_hist[, .fitted := NULL]
              fac_samps <- fac_samps + ann_samps
              next
            }
          }
        }

        # Fallback: C1 linear model on log1p scale
        obs <- fac_hist[!is.na(value) & !is.na(champ_mean) & is.finite(champ_mean)]
        if (nrow(obs) < 3L) {
          not_modeled[[length(not_modeled) + 1L]] <-
            data.frame(orgUnit = fac, category = cat, stringsAsFactors = FALSE)
          actual <- dt_sub_p[orgUnit == fac & get("data") == cat, value]
          fac_samps <- fac_samps + sum(actual, na.rm = TRUE)
          next
        }
        fit_lm <- tryCatch(
          stats::lm(log1p(value) ~ champ_mean, data = obs), error = function(e) NULL
        )
        if (is.null(fit_lm)) {
          not_modeled[[length(not_modeled) + 1L]] <-
            data.frame(orgUnit = fac, category = cat, stringsAsFactors = FALSE)
          actual <- dt_sub_p[orgUnit == fac & get("data") == cat, value]
          fac_samps <- fac_samps + sum(actual, na.rm = TRUE)
          next
        }
        a_coef <- stats::coef(fit_lm)[[1L]]
        b_coef <- stats::coef(fit_lm)[[2L]]
        sigma  <- stats::sigma(fit_lm)

        fac_period_rows <- if (!is.null(period_start) || !is.null(period_end)) {
          tmp <- fac_hist
          if (!is.null(period_start)) tmp <- tmp[Month >= period_start]
          if (!is.null(period_end))   tmp <- tmp[Month <= period_end]
          tmp
        } else fac_hist

        ann_samps <- rep(0, n_bootstrap)
        for (i in seq_len(nrow(fac_period_rows))) {
          if (!is.na(fac_period_rows$value[i])) {
            ann_samps <- ann_samps + fac_period_rows$value[i]
          } else {
            cm <- fac_period_rows$champ_mean[i]
            if (!is.na(cm) && is.finite(cm)) {
              log_pred <- a_coef + b_coef * cm
              drawn    <- pmax(0, expm1(stats::rnorm(n_bootstrap, log_pred, sigma)))
              ann_samps <- ann_samps + drawn
            }
          }
        }
        fac_samps <- fac_samps + ann_samps
      }

      # Non-champions with NO target data in the period -> champion-distribution fallback
      no_data_facs <- reg_facs[!orgUnit %in% facilities]
      nc_no_data   <- nrow(no_data_facs[Selected != "Champion"])

      if (nc_no_data > 0L) {
        champ_ous_reg <- reg_facs[Selected == "Champion", orgUnit]
        if (length(champ_ous_reg) > 0L) {
          champ_period_totals <- dt_sub_p[orgUnit %in% champ_ous_reg & !is.na(value),
            .(total = sum(value, na.rm = TRUE)), by = "orgUnit"
          ]
          champ_tbl <- merge(
            data.table::data.table(orgUnit = champ_ous_reg),
            champ_period_totals, by = "orgUnit", all.x = TRUE
          )
          champ_tbl[is.na(total), total := 0L]
          champ_dist <- champ_tbl$total
        } else {
          champ_dist_nat <- dt_sub_p[Selected == "Champion" & !is.na(value),
            .(total = sum(value, na.rm = TRUE)), by = "orgUnit"
          ]$total
          champ_dist <- if (length(champ_dist_nat) > 0L) champ_dist_nat else 0L
        }
        fac_samps <- fac_samps + .burden_resample_sum(champ_dist, nc_no_data, n_bootstrap)
      }

      reg_samps[[reg]] <- fac_samps
      ci <- .burden_ci(fac_samps)
      sub_rows[[length(sub_rows) + 1L]] <- data.frame(
        method = "C2", region = reg, category = cat,
        estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
        stringsAsFactors = FALSE
      )
    }

    nr <- .national_from_regions(reg_samps, "C2", cat)
    if (!is.null(nr)) nat_rows[[length(nat_rows) + 1L]] <- nr
  }

  if (length(sub_rows) == 0L) return(NULL)
  list(
    subnational = data.table::rbindlist(sub_rows, fill = TRUE),
    national    = data.table::rbindlist(nat_rows, fill = TRUE),
    not_modeled = if (length(not_modeled)) data.table::rbindlist(not_modeled) else NULL
  )
}

# -- Method D: Care-Seeking Adjustment ----------------------------------------

#' Burden Method D: Care-Seeking Adjustment
#'
#' Divides each existing estimate by an assumed care-seeking proportion to
#' produce an estimate of the total burden including persons who never sought
#' care.
#'
#' @param results_list named list of burden results (from A, B, C1, C2) where
#'   each element is a `list($subnational, $national)`.
#' @param care_seeking named numeric vector of care-seeking proportions, one
#'   per category string.  Any category not listed uses `default_p`.
#' @param default_p default care-seeking proportion (applied when category not
#'   in `care_seeking`).
#' @param n_bootstrap integer; samples for propagating care-seeking uncertainty.
#' @return data.table with columns: base_method, region, category,
#'   estimate, lower, upper, care_p (care-seeking-adjusted).
#' @export
burden_d_adjust <- function(results_list, care_seeking = c("default" = 0.80),
                             default_p = 0.80, n_bootstrap = 1000L) {
  rows <- list()
  for (method_name in names(results_list)) {
    res <- results_list[[method_name]]
    if (is.null(res)) next
    for (level in c("national", "subnational")) {
      df <- res[[level]]
      if (is.null(df) || nrow(df) == 0L) next

      for (i in seq_len(nrow(df))) {
        cat_i <- df$category[i]
        p     <- if (!is.null(care_seeking[[cat_i]])) care_seeking[[cat_i]] else default_p
        # Propagate uncertainty: draw p from Beta matched to p with small SD (~0.03)
        # Treat p as Beta mean: alpha = p*(p*(1-p)/0.03^2 - 1), beta = (1-p)*...
        # For simplicity use p +/- 5% via uniform
        p_draws <- stats::runif(n_bootstrap, max(0.01, p - 0.05), min(0.99, p + 0.05))
        base_samps <- stats::rnorm(n_bootstrap,
          mean = df$estimate[i],
          sd   = max(1, (df$upper[i] - df$lower[i]) / (2 * 1.96))
        )
        adj_samps <- pmax(0, base_samps / p_draws)
        ci <- .burden_ci(adj_samps)
        rows[[length(rows) + 1L]] <- data.frame(
          base_method = method_name,
          region      = df$region[i],
          category    = cat_i,
          level       = level,
          estimate    = ci$estimate,
          lower       = ci$lower,
          upper       = ci$upper,
          care_p      = round(p, 3),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0L) return(NULL)
  data.table::rbindlist(rows, fill = TRUE)
}

# -- Method E: Robust Malaria Incidence Estimator -----------------------------
#
# Full 5-equation implementation of the Robust Estimator.
# Reference: Thwing, Plucinski, Painter et al. (2020).
#   A Robust Estimator of Malaria Incidence from Routine Health Facility Data.
#   Am J Trop Med Hyg 102(4):811-820.  doi:10.4269/ajtmh.19-0600
#
# Variable naming follows the original correct_incidence() code in
# adjustedCorrectedIncidence.R (which implements the same paper):
#   D  = test negatives  (B* in paper notation)
#   E  = test positives  (A  in paper notation)
#   Fn = non-tested      (C  in paper notation)
#
# Parameters (all bootstrap vectors of length n):
#   beta     = proportion of non-malaria consults that are febrile
#              (gold-standard surveys: <5 yrs median 0.73, >=5 yrs median 0.57)
#   alpha    = ratio: TPR among untested febrile / TPR among tested febrile
#              (health facility exit surveys: median 0.48)
#   si       = standard non-malaria fever incidence per 1,000 pop (gamma x 1000)
#              (<5 yrs: 2,000; >=5 yrs: 1,000)
#   lambdamin = minimum malaria-attributable fraction (median 0.75, range 0.5-0.95)
.robust_estimator <- function(D, E, Fn, pop,
                               beta, alpha, si, lambdamin) {
  n <- length(beta)
  cases_out     <- numeric(n)
  incidence_out <- rep(NA_real_, n)

  if (any(is.na(c(D, E, Fn))) || D < 0 || E < 0 || Fn < 0) {
    return(list(cases = rep(max(0, E), n), incidence = incidence_out))
  }
  if ((D + E) == 0) {
    return(list(cases = rep(0, n), incidence = incidence_out))
  }

  TPR <- E / (D + E)

  for (b in seq_len(n)) {
    be <- beta[b]; al <- alpha[b]; lm <- lambdamin[b]
    TPR_nt <- min(al * TPR, 0.999)   # estimated TPR among untested febrile patients

    # Equation 1: B* (untested non-malaria fever)
    denom  <- (1 - be) / be + TPR_nt / (1 - TPR_nt) + 1
    B_star <- max(0, (Fn - D * (1 - be) / be) / denom)

    # Equation 2: A* (untested malaria cases)
    A_star <- max(0, TPR_nt / (1 - TPR_nt) * B_star)

    # Total malaria = confirmed + estimated untested
    total_malaria    <- E + A_star
    cases_out[b]     <- total_malaria

    # Equations 4-5: incidence (requires population)
    if (!is.na(pop) && pop > 0) {
      TPR_all <- total_malaria / max(1, B_star + A_star + D + E)
      lambda  <- 1 + (lm - 1) * TPR_all          # Eq 4: malaria-attributable fraction
      nmf_inc <- (D + B_star + (1 - lambda) * total_malaria) / pop * 1000  # observed NMF rate
      if (nmf_inc > 0) {
        # Eq 5: fully corrected incidence per 1,000 (adjusts for both testing gap
        # and low facility utilisation relative to expected NMF rate)
        incidence_out[b] <- total_malaria * lambda / pop * (si / nmf_inc) * 1000
      } else {
        incidence_out[b] <- total_malaria / pop * 1000
      }
    }
  }
  list(cases = cases_out, incidence = incidence_out)
}

#' Burden Method E: Robust Malaria Incidence Estimator (champion facilities only)
#'
#' Implements the full 5-equation Robust Estimator of Thwing, Plucinski,
#' Painter et al. (2020, AJTMH 102:811-820) to correct confirmed case counts
#' for incomplete testing coverage and, when population data are provided,
#' to estimate community-level incidence per 1,000 population.
#'
#' Only champion facilities that consistently report all three required
#' elements (confirmed cases, total attendance, patients tested) are included.
#' Non-champion facilities are excluded  -  they do not influence the estimate.
#' Elements are summed across qualifying champions within each region before
#' the correction is applied.
#'
#' Uncertainty is propagated by drawing beta, alpha, and lambdamin simultaneously from
#' uniform distributions spanning the published ranges from Thwing et al. 2020.
#'
#' @param data data.table from `selectedData()` with a `Selected` column.
#' @param confirmed_elements character; `data` values for confirmed cases.
#' @param attendance_elements character; `data` values for total attendance.
#' @param tested_elements character; `data` values for patients tested.
#' @param population_element character or NULL; `data` value for population.
#' @param cat_map_attend named character; confirmed -> attendance category map.
#' @param cat_map_tested named character; confirmed -> tested category map.
#' @param beta_young numeric; beta for < 5 years (Thwing 2020 default 0.73).
#' @param beta_old numeric; beta for >= 5 years (Thwing 2020 default 0.57).
#' @param alpha numeric; TPR ratio untested/tested febrile (default 0.48).
#' @param gamma_young numeric; expected NMF episodes/person/year < 5 yrs (default 2.0).
#' @param gamma_old numeric; expected NMF episodes/person/year >= 5 yrs (default 1.0).
#' @param lambdamin numeric; minimum malaria-attributable fraction (default 0.75).
#' @param young_pattern regex matching "younger" category labels.
#' @param min_months integer; minimum reported months to qualify (default 10).
#' @param region_col character; region column name.
#' @param n_bootstrap integer; bootstrap draws.
#' @return list with `$subnational` and `$national` data.tables.  When
#'   population data are available each row also has `incidence_per_1k`,
#'   `incidence_lower`, `incidence_upper`.
#' @export
burden_aci <- function(data,
                       confirmed_elements,
                       attendance_elements,
                       tested_elements,
                       population_element = NULL,
                       cat_map_attend  = NULL,
                       cat_map_tested  = NULL,
                       beta_young      = 0.73,
                       beta_old        = 0.57,
                       alpha           = 0.48,
                       gamma_young     = 2.0,
                       gamma_old       = 1.0,
                       lambdamin       = 0.75,
                       young_pattern   = "<5|under.5|under 5|younger|<five",
                       min_months      = 10L,
                       region_col,
                       n_bootstrap     = 1000L) {
  dt <- data.table::as.data.table(data)
  dt[, .month_int := lubridate::month(Month)]

  if (is.null(cat_map_attend))
    cat_map_attend <- stats::setNames(rep(attendance_elements[1L], length(confirmed_elements)),
                                      confirmed_elements)
  if (is.null(cat_map_tested))
    cat_map_tested <- stats::setNames(rep(tested_elements[1L], length(confirmed_elements)),
                                      confirmed_elements)

  all_elems <- c(confirmed_elements, attendance_elements, tested_elements,
                 if (!is.null(population_element)) population_element)

  # Totals over the full filtered period  -  champions only
  ann <- dt[
    get("data") %in% all_elems & !is.na(value) & Selected == "Champion",
    .(annual   = sum(value, na.rm = TRUE),
      n_months = data.table::uniqueN(.month_int)),
    by = c("orgUnit", "data", region_col)
  ]

  has_pop   <- !is.null(population_element) && nchar(population_element) > 0L
  sub_rows  <- nat_rows <- list()

  for (cat in confirmed_elements) {
    att_cat  <- cat_map_attend[[cat]]
    test_cat <- cat_map_tested[[cat]]
    if (is.null(att_cat) || is.null(test_cat)) next

    is_young <- grepl(young_pattern, cat, ignore.case = TRUE)
    b_lo <- if (is_young) 0.65 else 0.49
    b_hi <- if (is_young) 0.84 else 0.61
    si   <- if (is_young) gamma_young * 1000 else gamma_old * 1000

    has_conf  <- ann[get("data") == cat      & n_months >= min_months, orgUnit]
    has_att   <- ann[get("data") == att_cat  & n_months >= min_months, orgUnit]
    has_test  <- ann[get("data") == test_cat & n_months >= min_months, orgUnit]
    qualified <- Reduce(intersect, list(has_conf, has_att, has_test))

    regions   <- unique(ann[get("data") == cat][[region_col]])
    reg_samps <- vector("list", length(regions))
    names(reg_samps) <- regions

    for (reg in regions) {
      qual_reg <- intersect(qualified,
                            ann[get(region_col) == reg & get("data") == cat, orgUnit])

      if (length(qual_reg) == 0L) {
        reg_samps[[reg]] <- rep(0, n_bootstrap)
        sub_rows[[length(sub_rows) + 1L]] <- data.frame(
          method = "E", region = reg, category = cat, n_champions = 0L,
          estimate = NA_integer_, lower = NA_integer_, upper = NA_integer_,
          incidence_per_1k = NA_real_, incidence_lower = NA_real_,
          incidence_upper  = NA_real_, stringsAsFactors = FALSE
        )
        next
      }

      A_reg  <- sum(ann[orgUnit %in% qual_reg & get("data") == att_cat,  annual], na.rm=TRUE)
      T_reg  <- sum(ann[orgUnit %in% qual_reg & get("data") == test_cat, annual], na.rm=TRUE)
      E_reg  <- sum(ann[orgUnit %in% qual_reg & get("data") == cat,      annual], na.rm=TRUE)
      D_reg  <- max(0, T_reg - E_reg)
      Fn_reg <- max(0, A_reg - T_reg)

      pop_reg <- if (has_pop)
        sum(ann[orgUnit %in% qual_reg & get("data") == population_element, annual], na.rm=TRUE)
      else NA_real_
      if (!is.na(pop_reg) && pop_reg == 0) pop_reg <- NA_real_

      beta_b   <- stats::runif(n_bootstrap, b_lo, b_hi)
      alpha_b  <- stats::runif(n_bootstrap, 0.42, 0.55)
      lambda_b <- stats::runif(n_bootstrap, 0.50, 0.95)

      res   <- .robust_estimator(D_reg, E_reg, Fn_reg, pop_reg,
                                  beta_b, alpha_b, si, lambda_b)
      samps <- pmax(0, res$cases)
      reg_samps[[reg]] <- samps
      ci <- .burden_ci(samps)

      inc_ci <- if (has_pop && any(!is.na(res$incidence))) {
        s <- res$incidence[!is.na(res$incidence)]
        list(estimate = round(mean(s), 1),
             lower    = round(stats::quantile(s, 0.025, names=FALSE), 1),
             upper    = round(stats::quantile(s, 0.975, names=FALSE), 1))
      } else list(estimate=NA_real_, lower=NA_real_, upper=NA_real_)

      sub_rows[[length(sub_rows) + 1L]] <- data.frame(
        method = "E", region = reg, category = cat,
        n_champions = length(qual_reg),
        estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
        incidence_per_1k = inc_ci$estimate,
        incidence_lower  = inc_ci$lower,
        incidence_upper  = inc_ci$upper,
        stringsAsFactors = FALSE
      )
    }

    nr <- .national_from_regions(Filter(function(s) any(s > 0), reg_samps), "E", cat)
    if (!is.null(nr)) nat_rows[[length(nat_rows) + 1L]] <- nr
  }

  if (length(sub_rows) == 0L) return(NULL)
  list(
    subnational = data.table::rbindlist(sub_rows, fill = TRUE),
    national    = data.table::rbindlist(nat_rows, fill = TRUE)
  )
}

# -- Population rate helper ----------------------------------------------------

#' Add per-100,000 population rate to a burden result
#'
#' Joins population totals to a burden result and adds `rate_per_100k`,
#' `rate_lower`, and `rate_upper` columns.
#'
#' @param result list with `$subnational` and `$national` (from any burden
#'   function).
#' @param pop_data data.table from `selectedData()` containing a population
#'   data element.
#' @param pop_element character; the `data` column value for population.
#' @param region_col character; region column name.
#' @return Modified result list with rate columns added.
#' @export
add_population_rate <- function(result, pop_data, pop_element, region_col) {
  if (is.null(result) || is.null(pop_data)) return(result)

  pd <- data.table::as.data.table(pop_data)
  pd[, .year := lubridate::year(Month)]
  pd <- pd[get("data") == pop_element & !is.na(value)]

  # Annual population per region per year
  pop_reg <- pd[,
    .(population = sum(value, na.rm = TRUE)),
    by = c(region_col, ".year")
  ]
  data.table::setnames(pop_reg, region_col, "region")
  data.table::setnames(pop_reg, ".year", "year")

  # National population per year
  pop_nat <- pop_reg[, .(population = sum(population, na.rm = TRUE)), by = "year"]
  pop_nat[, region := "National"]

  pop_all <- data.table::rbindlist(list(pop_reg, pop_nat), fill = TRUE)

  .add_rate <- function(df) {
    if (is.null(df) || nrow(df) == 0L) return(df)
    df2 <- merge(as.data.frame(df), as.data.frame(pop_all),
                 by = c("region", "year"), all.x = TRUE)
    df2$rate_per_100k <- round(df2$estimate / df2$population * 1e5, 1)
    df2$rate_lower    <- round(df2$lower    / df2$population * 1e5, 1)
    df2$rate_upper    <- round(df2$upper    / df2$population * 1e5, 1)
    df2
  }

  list(
    subnational = .add_rate(result$subnational),
    national    = .add_rate(result$national)
  )
}

# -- Summary helpers -----------------------------------------------------------

#' Combine burden results into a wide display table
#'
#' Pivots multiple method results so each row is a region x year x category and
#' each pair of columns is one method's estimate with CI.
#'
#' @param results_list named list of burden results.
#' @param level "national" or "subnational".
#' @param show_rate Logical; when `TRUE` and incidence columns are present,
#'   append a rate-per-100k column (default `FALSE`).
#' @return data.frame suitable for DT::datatable().
#' @export
burden_summary_table <- function(results_list, level = "national",
                                  show_rate = FALSE) {
  rows <- list()
  has_rate <- FALSE

  for (nm in names(results_list)) {
    df <- results_list[[nm]][[level]]
    if (is.null(df) || nrow(df) == 0L) next
    df2 <- as.data.frame(df)
    df2$method <- nm
    df2$ci_fmt <- mapply(format_burden_estimate,
                         df2$estimate, df2$lower, df2$upper)
    if (show_rate && "rate_per_100k" %in% names(df2) &&
        !all(is.na(df2$rate_per_100k))) {
      df2$rate_fmt <- paste0(
        df2$rate_per_100k, " [",
        df2$rate_lower, "-", df2$rate_upper, "]"
      )
      has_rate <- TRUE
    }
    rows[[nm]] <- df2
  }
  if (length(rows) == 0L) return(NULL)
  long <- data.table::rbindlist(rows, fill = TRUE)

  # Pivot count estimates: rows = region x year x category, cols = method
  wide <- data.table::dcast(
    long,
    region + category ~ method,
    value.var = "ci_fmt"
  )

  # Optionally pivot rates alongside
  if (has_rate && "rate_fmt" %in% names(long)) {
    rate_wide <- data.table::dcast(
      long,
      region + category ~ method,
      value.var = "rate_fmt"
    )
    method_cols <- setdiff(names(rate_wide), c("region", "year", "category"))
    data.table::setnames(rate_wide, method_cols,
                         paste0(method_cols, "/100k"))
    wide <- merge(wide, rate_wide, by = c("region", "category"),
                  all.x = TRUE)
  }

  as.data.frame(wide)
}

# -- Category totals ----------------------------------------------------------

#' Add "Total" rows that sum across all categories
#'
#' For each region, sums estimates across categories and combines uncertainty
#' via SE_total = sqrtsum()SE_i^2 (assumes independence across categories).  A "Total"
#' row is appended to both `$subnational` and `$national` when more than one
#' category is present.
#'
#' @param result list with `$subnational` and `$national` data.tables.
#' @return Modified result with Total rows appended.
#' @export
add_category_totals <- function(result) {
  if (is.null(result)) return(NULL)

  z975 <- stats::qnorm(0.975)

  .add_total <- function(df) {
    if (is.null(df) || nrow(df) == 0L) return(df)
    df <- as.data.frame(df)
    if (!"category" %in% names(df)) return(df)
    if (length(unique(df$category)) <= 1L) return(df)   # already one category

    # Identify non-estimate columns to carry through
    group_cols <- intersect(c("method", "region"), names(df))

    totals <- do.call(rbind, lapply(
      split(df, df[, group_cols, drop = FALSE]),
      function(g) {
        est   <- sum(g$estimate, na.rm = TRUE)
        se_i  <- (g$upper - g$lower) / (2 * z975)
        se_t  <- sqrt(sum(se_i^2, na.rm = TRUE))
        row   <- g[1L, , drop = FALSE]
        row$category <- "Total"
        row$estimate <- as.integer(round(est))
        row$lower    <- as.integer(round(est - z975 * se_t))
        row$upper    <- as.integer(round(est + z975 * se_t))
        # Zero out any extra columns we don't know how to aggregate
        for (col in setdiff(names(row), c(group_cols, "category",
                                           "estimate", "lower", "upper"))) {
          if (is.numeric(row[[col]])) row[[col]] <- NA_real_
          else if (is.integer(row[[col]])) row[[col]] <- NA_integer_
        }
        row
      }
    ))

    data.table::rbindlist(list(
      data.table::as.data.table(df),
      data.table::as.data.table(totals)
    ), fill = TRUE)
  }

  list(
    subnational = .add_total(result$subnational),
    national    = .add_total(result$national)
  )
}

# -- Significance grouping -----------------------------------------------------

#' Group regions by statistical significance
#'
#' Clusters sub-national regions into K groups such that regions within a group
#' are not significantly different from each other and regions across groups
#' are.  Uses hierarchical clustering on a pairwise Z-score distance matrix
#' (distance = |est_i - est_j| / sqrt(SE_i^2 + SE_j^2)).
#'
#' @param subnational data.frame or data.table from a burden result
#'   `$subnational`  -  must contain columns `region`, `category`, `estimate`,
#'   `lower`, `upper`.
#' @param n_groups integer; number of groups (2-5).
#' @param category character; which category to group on.  If NULL, estimates
#'   are summed across categories before clustering.
#' @return Named integer vector: names = region, values = group number
#'   (1 = lowest burden, K = highest burden).  Returns NULL if there are
#'   fewer regions than requested groups.
#' @export
compute_burden_groups <- function(subnational, n_groups, category = NULL) {
  df <- as.data.frame(subnational)
  df <- df[df$region != "National" & !is.na(df$estimate), ]

  if (!is.null(category) && "category" %in% names(df))
    df <- df[df$category == category, ]

  # Sum across categories if multiple remain
  if ("category" %in% names(df) && length(unique(df$category)) > 1L) {
    df <- stats::aggregate(
      cbind(estimate, lower, upper) ~ region,
      data = df, FUN = sum
    )
  }

  df <- df[!is.na(df$estimate), ]
  if (nrow(df) < n_groups) return(NULL)

  # Pairwise Z-score distance: how many SEs apart are two regions?
  z975 <- stats::qnorm(0.975)
  se   <- pmax(1, (df$upper - df$lower) / (2 * z975))
  n    <- nrow(df)
  dmat <- matrix(0, n, n, dimnames = list(df$region, df$region))

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      dmat[i, j] <- abs(df$estimate[i] - df$estimate[j]) /
                    sqrt(se[i]^2 + se[j]^2)
    }
  }

  hc     <- stats::hclust(stats::as.dist(dmat), method = "ward.D2")
  groups <- stats::cutree(hc, k = n_groups)

  # Reorder labels: group 1 = lowest mean estimate
  grp_means <- tapply(df$estimate, groups, mean)
  rank_map <- rank(grp_means, ties.method = "first")
  # Index rank_map by group label (character), preserve region names
  groups   <- stats::setNames(rank_map[as.character(groups)], names(groups))

  groups   # named integer vector: names = region
}

# -- Wrapper -------------------------------------------------------------------

#' Run all requested burden estimation methods
#'
#' A non-Shiny entry point that calls each requested method in sequence and
#' returns a named list of results suitable for scripted analysis or testing.
#'
#' @param data data.table from `selectedData()`  -  must have columns `orgUnit`,
#'   `Month`, `data`, `value`, `Selected`, and a region column.
#' @param target_elements character; `data` values for the outcome (e.g.
#'   confirmed malaria cases).
#' @param region_col character; name of the region column (typically
#'   `levelNames[2]`).
#' @param methods character vector of methods to run.  Any subset of
#'   `c("A","B","C1","C2","E")`.  Default: all five.
#' @param attendance_elements character or NULL; required for methods B and E.
#' @param tested_elements character or NULL; required for method E.
#' @param population_element character or NULL; triggers per-100k rates for all
#'   methods and the incidence output for method E.
#' @param cat_map_attend named character; confirmed -> attendance category map.
#' @param cat_map_tested named character; confirmed -> tested category map.
#' @param beta_young,beta_old numeric; beta for method E (default Thwing 2020).
#' @param alpha numeric; alpha for method E (default 0.48).
#' @param gamma_young,gamma_old numeric; gamma for method E (default 2.0 / 1.0).
#' @param lambdamin numeric; lambdamin for method E (default 0.75).
#' @param run_d logical; whether to apply the care-seeking adjustment (D).
#' @param care_seeking named numeric; care-seeking proportions by category for D.
#' @param default_p numeric; fallback care-seeking proportion for D.
#' @param n_bootstrap integer; bootstrap draws (default 1000).
#' @param verbose logical; print progress messages (default TRUE).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`A`, `B`, `C1`, `C2`, `E`}{Results from each method (if run),
#'       each a `list($subnational, $national)`.}
#'     \item{`D`}{Care-seeking adjusted table (if `run_d = TRUE`).}
#'     \item{`summary_national`}{Wide data.frame of national totals,
#'       all methods side by side.}
#'     \item{`summary_subnational`}{Wide data.frame of sub-national totals.}
#'     \item{`log`}{Character vector of timestamped progress messages.}
#'   }
#' @export
run_burden_estimates <- function(
    data,
    target_elements,
    region_col,
    methods           = c("A", "B", "C1", "C2", "E"),
    attendance_elements = NULL,
    tested_elements     = NULL,
    population_element  = NULL,
    cat_map_attend    = NULL,
    cat_map_tested    = NULL,
    beta_young        = 0.73,
    beta_old          = 0.57,
    alpha             = 0.48,
    gamma_young       = 2.0,
    gamma_old         = 1.0,
    lambdamin         = 0.75,
    run_d             = FALSE,
    care_seeking      = c(default = 0.80),
    default_p         = 0.80,
    n_bootstrap       = 1000L,
    verbose           = TRUE
) {
  log_lines <- character(0)
  .log <- function(msg) {
    line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
    log_lines <<- c(log_lines, line)
    if (verbose) message(line)
  }

  results <- list(A = NULL, B = NULL, C1 = NULL, C2 = NULL, E = NULL, D = NULL)

  .log(sprintf("Starting burden estimation: methods = %s", paste(methods, collapse = ", ")))

  if ("A" %in% methods) {
    .log("Method A: Champion Multiple...")
    results$A <- tryCatch(
      burden_a(data, target_elements, region_col, n_bootstrap = n_bootstrap),
      error = function(e) { .log(paste("  ERROR:", e$message)); NULL }
    )
    if (!is.null(results$A))
      .log(sprintf("  Done. %d region-category rows.", nrow(results$A$subnational)))
    else
      .log("  No results  -  check champion facilities are defined.")
  }

  if ("B" %in% methods) {
    if (is.null(attendance_elements)) {
      .log("Method B skipped: attendance_elements not supplied.")
    } else {
      .log("Method B: Attendance-Based Champion Multiple...")
      results$B <- tryCatch(
        burden_b(data, target_elements, attendance_elements,
                 cat_map = cat_map_attend, region_col, n_bootstrap),
        error = function(e) { .log(paste("  ERROR:", e$message)); NULL }
      )
      if (!is.null(results$B))
        .log(sprintf("  Done. %d rows.", nrow(results$B$subnational)))
      else
        .log("  No results  -  need >= 3 champion facilities with attendance data.")
    }
  }

  if ("C1" %in% methods) {
    .log("Method C1: Linear Imputation...")
    results$C1 <- tryCatch(
      burden_c1(data, target_elements, region_col, n_bootstrap = n_bootstrap),
      error = function(e) { .log(paste("  ERROR:", e$message)); NULL }
    )
    if (!is.null(results$C1)) {
      nm <- if (!is.null(results$C1$not_modeled)) nrow(results$C1$not_modeled) else 0L
      .log(sprintf("  Done. %d facilities not modeled (flagged for fallback).", nm))
    } else {
      .log("  No results.")
    }
  }

  if ("C2" %in% methods) {
    .log("Method C2: ARIMA Imputation (may be slow)...")
    results$C2 <- tryCatch(
      burden_c2(data, target_elements, region_col, n_bootstrap = n_bootstrap),
      error = function(e) { .log(paste("  ERROR:", e$message)); NULL }
    )
    if (!is.null(results$C2)) {
      nm <- if (!is.null(results$C2$not_modeled)) nrow(results$C2$not_modeled) else 0L
      .log(sprintf("  Done. %d facilities fell back to linear.", nm))
    } else {
      .log("  No results.")
    }
  }

  if ("E" %in% methods) {
    if (is.null(attendance_elements) || is.null(tested_elements)) {
      .log("Method E skipped: attendance_elements and tested_elements required.")
    } else {
      .log("Method E: Robust Estimator (Thwing/Plucinski/Painter 2020)...")
      results$E <- tryCatch(
        burden_aci(data,
                   confirmed_elements  = target_elements,
                   attendance_elements = attendance_elements,
                   tested_elements     = tested_elements,
                   population_element  = population_element,
                   cat_map_attend      = cat_map_attend,
                   cat_map_tested      = cat_map_tested,
                   beta_young          = beta_young,
                   beta_old            = beta_old,
                   alpha               = alpha,
                   gamma_young         = gamma_young,
                   gamma_old           = gamma_old,
                   lambdamin           = lambdamin,
                   region_col          = region_col,
                   n_bootstrap         = n_bootstrap),
        error = function(e) { .log(paste("  ERROR:", e$message)); NULL }
      )
      if (!is.null(results$E))
        .log(sprintf("  Done. %d rows.", nrow(results$E$subnational)))
      else
        .log("  No results  -  check all three elements are present in champion data.")
    }
  }

  # Population rates
  if (!is.null(population_element)) {
    .log("Adding population rates (per 100,000)...")
    for (nm in c("A", "B", "C1", "C2")) {
      if (!is.null(results[[nm]])) {
        results[[nm]] <- tryCatch(
          add_population_rate(results[[nm]], data, population_element, region_col),
          error = function(e) results[[nm]]
        )
      }
    }
  }

  # Method D
  if (run_d) {
    base_list <- Filter(Negate(is.null),
                        results[c("A", "B", "C1", "C2", "E")])
    if (length(base_list) == 0L) {
      .log("Method D skipped: no base estimates available.")
    } else {
      .log("Method D: Care-Seeking Adjustment...")
      results$D <- tryCatch(
        burden_d_adjust(base_list, care_seeking = care_seeking,
                        default_p = default_p, n_bootstrap = n_bootstrap),
        error = function(e) { .log(paste("  ERROR:", e$message)); NULL }
      )
      if (!is.null(results$D))
        .log(sprintf("  Done. %d rows.", nrow(results$D)))
    }
  }

  .log("All methods complete.")

  # Build summary tables
  base_list <- Filter(Negate(is.null), results[c("A", "B", "C1", "C2", "E")])
  has_pop   <- !is.null(population_element)

  results$summary_national    <- burden_summary_table(base_list, "national",    has_pop)
  results$summary_subnational <- burden_summary_table(base_list, "subnational", has_pop)
  results$log <- log_lines

  results
}
