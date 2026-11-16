#' functions for data manipulation

# Local shim for purrr::is_empty - avoids a hard purrr Imports dependency
#' @noRd
.is_empty <- function(x) length(x) == 0L

#' Get level names from org units
#' @param orgUnits data frame of org units with level and levelName columns
#' @param .cat logical; if TRUE print progress messages
#' @return character vector of level names in ascending order
#' @noRd
getLevelNames = function(orgUnits, .cat = FALSE) {
  if ('sf' %in% class(orgUnits)) {
    orgUnits = orgUnits %>% st_drop_geometry()
  }
  if (.cat) {
    cat('\n* levelNames:')
  }
  l = count(orgUnits %>% as_tibble, level, levelName) %>%
    dplyr::filter(!is.na(level)) %>%
    arrange(level) %>%
    pull(levelName) %>%
    unique
  l = l[!is.na(l)]
  if (.cat) {
    cat('\n - end levelNames:', paste(l, collapse = ", "))
  }
  return(l)
}

# getLevelNames = function( orgUnits = NULL , .cat = FALSE  ){
#
#     if ( .cat ) cat( '\n* reporting_widget levelNames():' )
#
#     if ( is.null( orgUnits ) ){
#       cat( "\n - orgUnits missing")
#       return()
#     }
#
#     LevelNames = count( orgUnits %>% as_tibble, level, levelName ) %>%
#       arrange( level ) %>% pull(levelName )
#
#     LevelNames = LevelNames[ !is.na( LevelNames ) ]
#
#     if ( .cat ) cat( '\n - :' , LevelNames  )
#     return( LevelNames )
# }

error_factor = function(x) {
  # Ensure all six flag columns exist.  Combined datasets start with only
  # key_entry_error and over_max populated; MAD/seasonal columns are added
  # FALSE here so error_factor() doesn't crash on datasets that haven't been
  # through the Outliers tab yet.
  flag_cols <- c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3",
                 "missing_numerator", "missing_denominator")
  missing   <- setdiff(flag_cols, names(x))
  if (length(missing) > 0) {
    x <- as.data.table(x)
    x[, (missing) := FALSE]
  }
  x %>%
    mutate(
      error = case_when(
        (!key_entry_error &
          !over_max &
          !mad15 &
          !mad10 &
          !seasonal5 &
          !seasonal3) ~
          'none',
        seasonal3 ~ 'seasonal3',
        seasonal5 ~ 'seasonal5',
        mad10 ~ 'mad10',
        mad15 ~ 'mad15',
        over_max ~ 'over_max',
        key_entry_error ~ 'key_entry_error',
        TRUE ~ 'none'
      ) %>%
        factor(
          c(
            'All',
            'key_entry_error',
            'over_max',
            'mad15',
            'mad10',
            'seasonal5',
            'seasonal3',
            'none'
          ),
          ordered = TRUE
        )
    )
}

#' Apply cleaning and filtering to a dataset
#' @param d data frame; the dataset to clean
#' @param .effectiveLeaf logical; filter to effective leaf org units
#' @param source character; data source label
#' @param error character; error-level name to apply (NULL uses original values)
#' @param algorithm character; outlier algorithm column to use for cleaning
#' @param .cat logical; print progress messages to the console
#' @return cleaned data frame
#' @export
cleanedData = function(
  d,
  .effectiveLeaf = TRUE,
  source = 'Original',
  error = NULL,
  algorithm = 'seasonal3',
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R cleanedData:')
  }

  .period = dataPeriod(d)
  if (.cat) {
    cat('\n - period is', .period)
  }

  if (.cat) {
    cat('\n - filtering by effectiveLeaf', .effectiveLeaf)
  }
  # effectiveLeaf is always TRUE for modern DHIS2 downloads (API returns only
  # org units where data was entered). This filter is a no-op on new datasets
  # and is retained only for backward compatibility with legacy files.
  d = (if (is.data.table(d)) d else as.data.table(d))[effectiveLeaf == .effectiveLeaf]

  if (nrow(d) == 0) {
    if (.cat) {
      cat('\n - d1 has zero rows')
    }
    return(d)
  }

  if (is.null(error) || error == "Original") {
    if (.cat) {
      cat('\n- source is original')
    }
    d[, dataCol := original]
  }

  if (!is.null(error) & 'error' %in% names(d)) {
    if (.cat) {
      cat("\n - error level:", error)
    }
    error.levels = levels(d$error)
    error.factor.value = which(error == error.levels)
    d[, dataCol := fifelse(as.numeric(error) > error.factor.value, original, NA_real_)]
  }

  if (.cat) {
    cat('\n - nrow( d ):', nrow(d))
  }
  removed = sum(is.na(d$dataCol))
  if (.cat) {
    cat(
      '\n - error set this many values to NA:',
      removed,
      "(",
      percent(removed / nrow(d)),
      ")"
    )
  }

  if (.cat) {
    cat('\n - removing rows where d is NA_: ', sum(d$data %in% "NA_"), 'rows')
  }
  d = d[!data %in% 'NA_']

  # if ( .cat ) cat( '\n - nrow( d ):' , nrow( d ))

  return(d)
}

#' Find most frequently reporting org units
#' @param data tsibble or data frame of prepared indicator data
#' @param endingMonth yearmonth; last month of the reporting window (NULL uses data maximum)
#' @param startingMonth yearmonth; first month of the reporting window (NULL uses data minimum)
#' @param period character; time index column name ("Month" or "Week")
#' @param missing_reports integer; number of allowed missing months (default 0)
#' @param count.any logical; deprecated — use `reporting_rule` instead
#' @param reporting_rule character; one of `"all_categories"`, `"all_elements"`,
#'   `"any_selected"`, or `"any_data"` — controls which facilities are counted
#'   as consistently reporting.
#' @param data_categories character vector; data element categories to consider
#' @param strict logical; how the champion denominator is built. `TRUE`
#'   (default) counts every calendar period in `[startingMonth, endingMonth]`,
#'   so a system-wide gap month penalises any facility that missed it. `FALSE`
#'   counts only periods that appear somewhere in the data for that year.
#' @param testing logical; save intermediate objects for debugging
#' @param .cat logical; print progress messages to the console
#' @return A character vector of `orgUnit` ids that reported consistently
#'   (within `missing_reports` missed periods) in every year of the window.
#' @export
mostFrequentReportingOUs <- function(
  data,
  endingMonth = NULL,
  startingMonth = NULL,
  period = NULL,
  missing_reports = 0,
  count.any = NULL,          # deprecated — use reporting_rule
  reporting_rule = NULL,     # "all_categories" | "all_elements" | "any_selected" | "any_data"
  data_categories = NULL,
  strict = TRUE,             # TRUE: require all calendar months; FALSE: only months with any reports
  testing = FALSE,
  .cat = FALSE
) {
  # Resolve reporting_rule from legacy count.any if not supplied explicitly.
  # Old count.any = TRUE  → "any_data"    (any non-NA value counts)
  # Old count.any = FALSE → "any_selected" (any selected element counts)
  if (is.null(reporting_rule)) {
    reporting_rule <- if (isTRUE(count.any) || is.null(count.any)) "any_data" else "any_selected"
  }
  if (.cat) {
    cat('\n* data Functions.R mostFrequentReportingOUs')
  }

  if (is.null(period)) {
    period = dataPeriod(data)
  }
  if (.cat) {
    cat('\n - period is:', period)
  }

  if (is.null(startingMonth)) {
    startingMonth = min(data$period, na.rm = TRUE)
  }
  if (is.null(endingMonth)) {
    endingMonth = max(data$period, na.rm = TRUE)
  }

  if (period %in% 'Month') {
    if (!'yearmonth' %in% class(startingMonth)) {
      startingMonth = as.yearmonth(startingMonth)
    }
    if (!'yearmonth' %in% class(endingMonth)) {
      endingMonth = as.yearmonth(endingMonth)
    }
  }

  if (period %in% 'Week') {
    if (!'yearweek' %in% class(startingMonth)) {
      startingMonth = yearweek(startingMonth)
    }
    if (!'yearweek' %in% class(endingMonth)) endingMonth = yearweek(endingMonth)
  }

  # Stay in data.table throughout — avoids dplyr + tibble round-trips on 5M+ rows.
  # is.data.table() check avoids a 1300MB copy when the input is already a data.table.
  dt = if (is.data.table(data)) data else as.data.table(data)

  if (.cat) {
    cat(
      '\n - mostFrequentReportingOUs between ',
      as.character(startingMonth),
      as.character(endingMonth)
    )
  }

  # ── Date-window filter (applies to all rules) ──────────────────────────────
  if (period %in% 'Month') {
    .sm_int = unclass(startingMonth)
    .em_int = unclass(endingMonth)
    dt = dt[unclass(Month) >= .sm_int & unclass(Month) <= .em_int]
  } else if (period %in% 'Week') {
    .sm_int = unclass(yearweek(startingMonth))
    .em_int = unclass(yearweek(endingMonth))
    dt = dt[unclass(Week) >= .sm_int & unclass(Week) <= .em_int]
  }
  dt = dt[!is.na(original)]

  if (.cat) cat('\n - reporting_rule:', reporting_rule)

  # "Complete" (orgUnit, period) tuples per reporting rule: facility_reports() holds
  # the rule, shared with the reporting widget's charts (R/fct_reporting.R)
  has_cats <- !is.null(data_categories) && length(data_categories) > 0
  dt_rep <- facility_reports(dt, period, reporting_rule, data_categories)

  if (testing) saveRDS(dt_rep, "mostFrequentReportingOUs.data.rds")

  if (.cat) {
    cat('\n - mr (data.table)')
    cat('\n - nrow(dt_rep):', nrow(dt_rep), '| unique orgUnits:', uniqueN(dt_rep$orgUnit))
  }
  .t0_mr <- proc.time()["elapsed"]

  # data.table::year(as.IDate()) uses pure C integer arithmetic — avoids tsibble's
  # format.yearmonth() string-formatting dispatch, which is ~5-10x slower on large vectors.
  dt_rep[, year := data.table::year(as.IDate(unclass(.SD[[1L]]), origin = "1970-01-01")),
         .SDcols = period]

  # distinct (year, orgUnit, period) tuples → count reported periods per org/year
  mr = dt_rep[, .(report_periods = .N), by = .(year, orgUnit)]
  if (.cat) cat('\n - mr rows:', nrow(mr), '| unique orgUnits:', uniqueN(mr$orgUnit), '| unique years:', uniqueN(mr$year))

  # Denominator: how many periods should a champion have reported each year?
  #
  # strict = TRUE  (default): every calendar period in [startingMonth, endingMonth].
  #   A month with zero reporters still counts against any facility that missed it.
  #   System-wide gaps (strikes, form changes) are penalised — "champion" means
  #   reporting every calendar month. Boundary years handled correctly (partial year).
  #
  # strict = FALSE: only periods that appear anywhere in the data for that year.
  #   A system-wide gap month is invisible to the algorithm; a facility that reported
  #   whenever any other facility did is a champion, even if that was only 2 months.
  if (strict) {
    # Guard: endingMonth - 1 (applied by caller for current-month exclusion) can
    # flip before startingMonth when an element has only one period in the final
    # year (e.g. Jan 2026 − 1 = Dec 2025 < Jan 2026). Return empty if so.
    if (as.Date(startingMonth) > as.Date(endingMonth))
      return(character(0))
    if (period == "Month") {
      all_dates <- seq(as.Date(startingMonth), as.Date(endingMonth), by = "month")
      all_ints  <- unclass(tsibble::yearmonth(all_dates))
    } else {
      all_dates <- seq(as.Date(startingMonth), as.Date(endingMonth), by = "week")
      all_ints  <- unclass(tsibble::yearweek(all_dates))
    }
    periods_per_year <- data.table(int = all_ints)[
      , .(max = .N),
      by = .(year = data.table::year(as.IDate(int, origin = "1970-01-01")))
    ]
  } else {
    # The denominator window must come from the SELECTED elements for every
    # rule, including "any_data". Exempting any_data (as this did) let the
    # unchecked elements extend the window backwards: on PDR Lao the selected
    # element starts in 2022 but some element somewhere has 2021 data, so
    # any_data got a 6-year denominator while any_selected got 5. Champion
    # selection requires years == max_years, and only 96 of ~1500 facilities
    # have any 2021 data at all, so the *more inclusive* rule produced 5
    # champions where the narrower one produced 450 — the opposite of what
    # widening the definition should do.
    #
    # "any_data" is about what counts as evidence that a facility was active,
    # not about which periods are under analysis.
    dt_periods <- if (has_cats) dt[get("data") %chin% data_categories] else dt
    ppy <- unique(dt_periods[, period, with = FALSE])
    setnames(ppy, period, "period_val")
    ppy[, year := data.table::year(as.IDate(unclass(period_val), origin = "1970-01-01"))]
    periods_per_year <- ppy[, .(max = .N), by = year]
  }
  # max_years from selected-element denominator so count.any doesn't extend window
  max_years = uniqueN(periods_per_year$year)
  if (.cat) {
    cat('\n - max_years', max_years)
    cat('\n - periods per year:\n')
    print(periods_per_year)
  }

  # Only count years that are in the denominator
  mr_j = merge(mr, periods_per_year, by = "year")
  if (.cat) cat('\n - mr_j rows:', nrow(mr_j))

  s_all = mr_j[
    , .(years = .N, consistent = all(report_periods >= (max - missing_reports))),
    by = orgUnit
  ]
  if (.cat) {
    cat('\n - summary before filter: years==max_years:', sum(s_all$years == max_years),
        '| consistent:', sum(s_all$consistent),
        '| both:', sum(s_all$years == max_years & s_all$consistent))
  }
  s = s_all[years == max_years & consistent == TRUE, unique(orgUnit)]

  if (.cat) {
    cat(sprintf('\n - number reportingSelectedOUs: %d orgUnits  (mr step: %.1f sec)', length(s), proc.time()["elapsed"] - .t0_mr))
  }
  return(s)
}


# group_by_cols = function( data = NULL , levelNames = NULL,
#                           split = NULL,
#                           merge = TRUE ,
#                           .cat = FALSE ){
#     # req( input$split )
#     if ( .cat ) cat("\n* group_by_cols():")
#
#    .period = period( data )
#
#     group_by_cols =  c(.period , 'orgUnit', 'data' )
#
#     if ( !merge ) group_by_cols = c( group_by_cols, 'dataSet' )
#
#     if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
#
#     group_by_cols = c( group_by_cols, levelNames )
#
#     if ( .cat ) cat("\n - group_by_cols():", group_by_cols )
#
#     if ( !is.null( split ) ) group_by_cols = c( group_by_cols , split )
#
#     # if ( length( reportingSelectedOUs() > 0 ) )
#       # group_by_cols = c( group_by_cols , 'Facilities' )
#
#     # # If not merge when total, show separate datsets
#     # if ( !input$merge & input$all_categories ) group_by_cols = c( group_by_cols , 'dataSet' )
#     #
#     if ( .cat ) cat( "\n- end group_by_cols()" , unique( group_by_cols )  )
#     return( unique( group_by_cols ) )
#
# }

# data.total = function( data ,
#                       period = "Month" ,
#                       .group_by_cols = NULL ,
#                       dataSet = NULL ,
#                       merge = FALSE ,
#                       dataset_merge_average = FALSE ,
#                       startDisplayMonth = NULL  ,
#                       endDisplayMonth = NULL,
#                       .cat = FALSE
#                        ){
#     if ( .cat ) cat( '\n* data.total():' )
#
#     .period = dataPeriod( data )
#
#     if ( .cat ) cat( '\n - period:' , .period )
#
#     .dates = data %>% pull( !!rlang::sym( .period )  )
#     if ( is.null( startDisplayMonth ) )  startDisplayMonth = min( .dates , na.rm = TRUE  )
#     if ( is.null( endDisplayMonth ) ) endDisplayMonth = max( .dates , na.rm = TRUE   )
#
#     if ( is.null( .group_by_cols )) .group_by_cols = groupByCols( data )
#     if ( .cat ) cat( '\n - data.total .group_by_cols:'  , .group_by_cols )
#
#     # Total categories by facilities and datasets
#     # data = plotData
#
#       # Merge  datasets
#       # Set all dataSets to Combined and re-summaries taking mean
#       # #print( 'data.total datasets' );  #print( dataSets() )
#       if ( .cat ) cat( '\n - merge ', merge )
#       if ( .cat ) cat( '\n - data datsets ' , unique( dataSet) )
#
#       mergeDatasets = merge %>% str_replace_all( fixed("\n"), "")
#       if ( .cat ) cat( '\n - mergeDatasets:' , mergeDatasets )
#
#
#       if ( merge  ){
#
#       combineSelectDatasets = data %>%
#                 mutate( dataSet = dataSet %>% str_replace_all( fixed("\r\n"), "")
#               ) %>%
#                 mutate(
#                     dataSet = ifelse(
#                         str_replace_all(dataSet, fixed("\n"), "") %in%
#                           mergeDatasets , 'Combined' , dataSet) ,
#                     data = 'Total'
#                 ) %>%
#                 setDT() %>%
#                 .[ , .(dataCol = sum( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ]
#
#       if ( .cat ) cat('\n - Combining dataSets %in% input$merge:' , mergeDatasets )
#
#
#       } else { combineSelectDatasets = data }
#
#       # Testing
#       # saveRDS( combineSelectDatasets , 'combineSelectDatasets.rds' )
#
#       # data.table sum/mean
#
#       if ( dataset_merge_average ) {
#            if ( .cat )  cat( '\n** merge data.table MEAN')
#
#             dataMerge = combineSelectDatasets %>%
#                 mutate( dataSet = 'Merged') %>%
#                 setDT() %>%
#                 # Mean of dataSets within orgUnit
#                 .[  , .(dataCol = mean( dataCol , na.rm = TRUE  )) , by =  .group_by_cols ]
#
#             if ( .cat ) cat( '\ndataMerge done' );  # glimpse( dataMerge )
#
#       } else {
#           dataMerge = combineSelectDatasets
#           # cat('\n glimpse( dataMerge )\n' ); #print(glimpse( dataMerge ))
#       }
#
#       # Testing
#       # saveRDS( dataMerge, 'dataMerge.rds' )
#       # #print( dataMerge %>% duplicates %>% glimpse )
#
#     key.cols = setdiff( .group_by_cols , .period )
#     if ( .cat ) cat('\n - key.cols:' ,  key.cols )
#
#     data.total =
#         dataMerge %>%
#         # fill_gaps( .full = TRUE  ) %>%
#         mutate(
#                 total = replace_na( dataCol , 0)
#                 )  %>% # for plotting, replace missing with zero
#         as_tsibble( index = !! rlang::sym( .period )  ,
#                     key =  all_of(  {{ key.cols }} ) )
#
#     if ( .cat ) cat( '\n - data.total class' , class( data.total ) )
#     if ( .cat ) cat( '\n - data.total cols' , names( data.total ) )
#
#     # Filter display dates
#     # cat( '/n - data.total cols:' , names( data.total ) )
#
#     if ( .period %in% 'Month' ){
#       if ( .cat ) cat( '\n -  .period %in% Month' )
#       data.total = data.total %>%
#         filter(
#           Month >=  yearmonth( startDisplayMonth )  ,
#           Month <=  yearmonth( endDisplayMonth )
#         )
#     }
#
#     if ( .period %in% 'Week' ){
#       if ( .cat ) cat( '\n -  .period %in% weeks' )
#       data.total = data.total %>%
#         filter(
#           Week >=  yearweek( startDisplayMonth )  ,
#           Week <=  yearweek( endDisplayMonth )
#         )
#     }
#
#
#     # test:
#     # saveRDS( data.total, 'data.total.rds')
#
#     if ( .cat ) cat('\n- end data.total()')
#     return( data.total )
#
#
# }

backtick <- function(x) paste0("`", x, "`")

#' Determine the period type used in a dataset
#' @param data1 data frame; dataset with a time column
#' @param .cat logical; if TRUE print progress messages
#' @return character; "Monthly" or "Weekly"
#' @noRd
dataPeriod = function(data1, .cat = FALSE) {
  if (.cat) {
    cat('\n* reporting_widget period():')
  }

  search_for_weekly = any(vapply(data1, function(.x) any(class(.x) %in% "yearweek"), logical(1L)))

  period = ifelse(search_for_weekly, "Week", "Month")

  if (.cat) {
    cat('\n - dataPeriod is ', period)
  }

  return(period)
}


#' Build group-by column vector for data aggregation
#' @param selected logical; include the "Selected" column
#' @param dataset logical; include the "dataSet" column
#' @param orgUnit logical; include the "orgUnit" column
#' @param data logical; include the "data" column
#' @param period character; time index column name to include ("Month" or "Week")
#' @param hts logical; include hierarchical time-series columns
#' @param agg_level character; aggregation level name
#' @param levelNames character vector; administrative level names
#' @param split character; split variable name (or NULL)
#' @param .cat logical; print progress messages to the console
#' @noRd
groupByCols = function(
  selected = TRUE,
  dataset = TRUE,
  orgUnit = TRUE,
  data = FALSE,
  period = NULL,
  hts = FALSE,
  agg_level = NULL,
  levelNames = NULL,
  split = NULL,
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R groupByCols :')
  }

  group_by_cols = period

  if (selected) {
    group_by_cols = c(group_by_cols, 'Selected')
  }

  if (orgUnit) {
    group_by_cols = c(group_by_cols, 'orgUnit')
  }

  if (dataset) {
    group_by_cols = c(group_by_cols, 'dataSet')
  }

  if (data) {
    group_by_cols = c(group_by_cols, 'data')
  }

  if (.cat) {
    cat("\n - agg_level", agg_level)
  }

  if (!.is_empty(agg_level)) {
    group_by_cols = base::union(group_by_cols, agg_level)
  }

  if (hts && !is.null(levelNames)) {
    group_by_cols = base::union(group_by_cols, levelNames)
  }

  if (!is.null(split) && any(split != 'None')) {
    group_by_cols = base::union(group_by_cols, split)
  }

  if (.cat) {
    cat("\n - end group_by_cols()", unique(group_by_cols))
  }

  return(unique(group_by_cols))
}


# selectedData equivalent to MG2 reactive function plotData()
# selectedData = function( data1 ,
#                          levelNames = NULL ,
#                          # all_categories = TRUE ,
#                          data_categories = NULL ,
#                          alwaysReporting = TRUE ,
#                          missing_reports = 0 ,
#                          reportingSelectedOUs = NULL ,
#                          startingMonth = NULL ,
#                          endingMonth = NULL ,
#                          period = NULL ,
#                          # source = 'Original' ,
#                          level = 'leaf' ,
#                          level2 = NULL ,
#                          level3 = NULL ,
#                          level4 = NULL ,
#                          level5 = NULL ,
#                          .cat = FALSE ,
#                          ... ){
#
#    if ( .cat ) cat( "\n* data Functions.R selectedData:" )
#
#    if ( nrow( data1 ) == 0 ){
#         cat('\n - data1() has zero rows')
#         return()
#    }
#
#   if ( .cat ) cat( '\n - nrow( d ):' , nrow( data1 ))
#
#   if ( is.null( period ) ) period = dataPeriod( data1 )
#   if ( .cat ) cat( '\n - period is:', period )
#
#    # if ( is.null( startingMonth )) startingMonth = yearmonth( min( data1$period   , na.rm = T ) , format = "%B%Y" )
#    if ( is.null( startingMonth ) ) startingMonth = min( data1$period , na.rm = TRUE )
#    # if ( is.null( endingMonth )) endingMonth = yearmonth( max( data1$period   , na.rm = T ) , format = "%B%Y" )
#    if ( is.null( endingMonth ) ) endingMonth = max( data1$period , na.rm = TRUE )
#    if ( is.null( data_categories ) ) data_categories = unique( data1$data )
#    if ( is.null( levelNames ) ) levelNames = unique( data1$data )
#
#       # NB: setting data = setDT( data1()) has side effect of changing data1() to data.table.
#       data = as.data.table( data1  )
#
#       if ( .cat ) cat( '\n - data (d) converted to data.table' )
#
#       # period = dataPeriod( data1 )
#       # if (.cat ) cat('\n - period is', period )
#       #
#       # data = data[ , period := base::get( period )  , ]
#
#       if ( !.is_empty( level2 ) & !.is_empty( levelNames ) ){ data = data[ base::get( levelNames[2] )  %in%  level2 ,, ] }
#
#       if ( !.is_empty( level3 ) & !.is_empty( levelNames ) ){ data = data[ base::get( levelNames[3] )  %in%   level3 ,, ] }
#
#       if ( !.is_empty( level4 ) & !.is_empty( levelNames ) ){ data = data[ base::get( levelNames[4] )  %in%   level4 ,, ] }
#
#       if ( !.is_empty( level5 ) & !.is_empty( levelNames ) ){ data = data[ base::get( levelNames[5] )  %in%   level5  ,, ]  }
#
#       if ( level %in% 'leaf' ){
#
#         if ( .cat ) cat( '\n - leaf level data only' )
#         data = data[ effectiveLeaf == TRUE , , ]
#
#       } else {
#
#         if ( .cat ) cat( '\n - levelname', levelName )
#         level. = count( orgUnits %>% as_tibble, level, levelName ) %>%
#           filter(levelName  %in% input$level  ) %>% pull( level )
#
#         data = data[ level  %in% level. , , ]
#       }
#
#     if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
#
#     # if ( source %in% 'Original' ){
#     #   if ( .cat ) cat('\n - d() source is original')
#     #
#     #   data = data[ , dataCol := as.numeric( original ) , ]
#     # }
#     #
#     # if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
#     #
#     # if ( source %in% 'Cleaned' & 'seasonal3' %in% names(data) ){
#     #   if ( .cat ) cat( '\n -' , paste('cleaning removes', sum( data$value , na.rm = T ) - sum( data$seasonal3 , na.rm = T )  , 'data points' ) )
#     #
#     #   data = setDT( data )[ , dataCol := fifelse( seasonal3, original, NA_real_  ) , ]
#     #
#     #   # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
#     #   if ('mad15' %in% names( data )){
#     #     # data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
#     #     data = setDT( data )[, mad15 := fifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) , ]
#     #
#     #   }
#     #
#     #   if ('mad10' %in% names( data )){
#     #     # data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
#     #     data = setDT( data )[, mad10 := fifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) , ]
#     #
#     #   }
#     #
#     #   if ('mad5' %in% names( data )){
#     #     # data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
#     #     data = setDT( data )[, mad5 := fifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) , ]
#     #
#     #   }
#     #
#     #   if ('seasonal5' %in% names( data )){
#     #     # data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
#     #     data = setDT( data )[, seasonal5 := fifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) , ]
#     #   }
#     #
#     #   if ('seasonal3' %in% names( data )){
#     #     # data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
#     #     data = setDT( data )[, seasonal3 := fifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) , ]
#     #
#     #   }
#     #
#     #   if ( .cat ) cat( '\n -' , paste('cleaning changes total by', sum( data$original , na.rm = T ) - sum( data$dataCol , na.rm = T )) )
#     # }
#     #
#     # if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
#
#     # filter to selected category
#
#
#     if ( any( !is.null( data_categories ) ) ){
#       if ( .cat ) cat( '\n - selectedData filtered by' , paste( data_categories , collapse = "\n - ") )
#       data = data %>% filter( data %in% data_categories )
#       if ( .cat ) cat( '\n - nrow( d ):' , nrow( data ))
#     }
#
#     # Consistent reporting
#     if ( alwaysReporting ){
#
#      if ( .cat ) cat( '\n - alwaysReporting' )
#
#      # reportingSelectedOUs = NULL
#      if ( .is_empty( reportingSelectedOUs ) & nrow( data ) > 0 ){
#
#         if ( .cat ) cat( "\n - finding most frequently reporting OUs")
#
#         reportingSelectedOUs  = mostFrequentReportingOUs( data ,
#                                              # all_categories = all_categories ,
#                                              data_categories = data_categories ,
#                                              startingMonth = startingMonth ,
#                                              endingMonth = endingMonth ,
#                                              missing_reports = missing_reports
#
#                                              )
#      }
#
#         # Add var for selected ous
#         if ( .cat ){
#           cat( '\n - selectedData length( reportingSelectedOUs()): ' , length( reportingSelectedOUs ) )
#           cat( '\n - starting - ending: ' , startingMonth,  endingMonth )
#
#         }
#         data = setDT(data)[ , Selected := fifelse( orgUnit %in% reportingSelectedOUs,
#                                                           'Reporting Each Period',
#                                                           'Inconsistent Reporting') ] %>%
#           as_tibble(.)
#
#         if ( period %in% "Month" ) data = data %>%
#           filter( Month  >= as.yearmonth( startingMonth ) )
#
#         if ( period %in% "Week" ) data = data %>%
#           filter( Week  >= yearweek( startingMonth ) )
#
#     # data = data %>% filter( Selected %in% 'Reporting Each Period' )
#     } else {
#          data = setDT(data)[ , Selected := "All", ] %>% as_tibble(.)
#    }
#
#     if ( .cat ) cat( '\n - end  selectedData()' )  ; # #print( names( data ))
#     # TESTING
#     # saveRDS( data , "plotData.rds" )
#
#   return( data )
# }
#

#' Filter and select data by reporting level and categories
#' @param data1 data frame or tsibble; the full prepared dataset
#' @param levelNames character vector; administrative level names
#' @param data_categories character vector; data element categories to include
#' @param alwaysReporting logical; restrict to consistently reporting facilities
#' @param missing_reports integer; allowed missing months when filtering reporters
#' @param reportingSelectedOUs character vector; pre-computed set of reporting org units
#' @param startingMonth yearmonth; start of the analysis window
#' @param endingMonth yearmonth; end of the analysis window
#' @param level character or integer; `"leaf"` (default) for facility-level
#'   rows, otherwise a level name from `levelNames` or a level number
#' @param level2 character; optional level-2 org unit filter value
#' @param level3 character; optional level-3 org unit filter value
#' @param level4 character; optional level-4 org unit filter value
#' @param level5 character; optional level-5 org unit filter value
#' @param level6 character; optional level-6 org unit filter value
#' @param .cat logical; print progress messages to the console
#' @param ... additional arguments
#' @return A `data.table` of `data1` filtered to the requested level, org
#'   units and categories, with a `Selected` column: `"Champion"` /
#'   `"Non-Champion"` when `alwaysReporting = TRUE`, otherwise `"All"`.
#' @export
selectedData = function(
  data1,
  levelNames = NULL,
  # all_categories = TRUE ,
  data_categories = NULL,
  alwaysReporting = TRUE,
  missing_reports = 0,
  reportingSelectedOUs = NULL,
  startingMonth = NULL,
  endingMonth = NULL,
  # source = 'Original' ,
  level = 'leaf',
  level2 = NULL,
  level3 = NULL,
  level4 = NULL,
  level5 = NULL,
  level6 = NULL,
  .cat = FALSE,
  ...
) {
  if (.cat) {
    cat("\n* data Functions.R selectedData:")
  }
  .t0_selectedData <- proc.time()["elapsed"]

  if (nrow(data1) == 0) {
    if (.cat) cat('\n - data1() has zero rows')
    return()
  }

  if (.cat) {
    cat('\n - nrow( d ):', nrow(data1))
  }

  # Track whether caller supplied explicit date bounds.
  # When NULL, skip the full-data min/max scan — it's only needed if
  # mostFrequentReportingOUs() must be computed internally (rare path).
  .startingMonth_explicit = !is.null(startingMonth)
  .endingMonth_explicit   = !is.null(endingMonth)
  if (is.null(data_categories)) {
    data_categories = unique(data1$data)
  }
  if (is.null(levelNames)) {
    levelNames = unique(data1$data)
  }

  # NB: setting data = setDT( data1()) has side effect of changing data1() to data.table.
  # is.data.table() check avoids a ~650MB copy when cleanedData() already returned a data.table.
  data = if (is.data.table(data1)) data1 else as.data.table(data1)

  if (.cat) {
    cat('\n - data (d) converted to data.table')
  }

  # period = dataPeriod( data1 )
  # if (.cat ) cat('\n - period is', period )
  #
  # data = data[ , period := base::get( period )  , ]

  if (!.is_empty(level2) & !.is_empty(levelNames)) {
    data = data[base::get(levelNames[2]) %in% level2, , ]
  }

  if (!.is_empty(level3) & !.is_empty(levelNames)) {
    data = data[base::get(levelNames[3]) %in% level3, , ]
  }

  if (!.is_empty(level4) & !.is_empty(levelNames)) {
    data = data[base::get(levelNames[4]) %in% level4, , ]
  }

  if (!.is_empty(level5) & !.is_empty(levelNames)) {
    data = data[base::get(levelNames[5]) %in% level5, , ]
  }

  if (!.is_empty(level6) & !.is_empty(levelNames) & length(levelNames) >= 6) {
    data = data[base::get(levelNames[6]) %in% level6, , ]
  }

  # Combine level and data_categories filters into a single pass where possible
  # to avoid creating two full copies of a large data.table.
  has_cats = any(!is.null(data_categories))

  if (level %in% 'leaf') {
    if (.cat) cat('\n - leaf level data only')

    # effectiveLeaf is always TRUE for modern DHIS2 downloads; this filter is a
    # no-op on new datasets and is retained for legacy backward compatibility.
    if (has_cats) {
      if (.cat) cat('\n - selectedData filtered by data_categories')
      data = data[effectiveLeaf == TRUE & get("data") %in% data_categories]
    } else {
      data = data[effectiveLeaf == TRUE, , ]
    }
  } else {
    # `level` is a level name (one of `levelNames`, in hierarchy order) or a
    # level number. (This branch previously read Shiny's input$level and an
    # undefined orgUnits table, so it could not run outside the old app.)
    level. = if (is.numeric(level)) as.integer(level) else match(level, levelNames)
    if (anyNA(level.)) {
      stop("selectedData(): `level` must be 'leaf', a level number, or one of ",
           "`levelNames`; got ", paste(level, collapse = ", "), call. = FALSE)
    }
    if (.cat) cat('\n - level', paste(level, collapse = ", "), '->', level.)

    if (has_cats) {
      if (.cat) cat('\n - selectedData filtered by data_categories')
      data = data[get("level") %in% level. & get("data") %in% data_categories]
    } else {
      data = data[level %in% level., , ]
    }
  }

  if (.cat) {
    cat('\n - nrow( d ):', nrow(data))
  }

  # Consistent reporting
  if (alwaysReporting) {
    if (.cat) {
      cat('\n - alwaysReporting: add Selected as Champion or Non-Champion')
    }

    # TESTING
    # cat("\ - saving selectedData data ")
    # save( data , reportingSelectedOUs , data_categories ,
    #          startingMonth , endingMonth, missing_reports,
    #          file = "selectedData.rda" )

    # reportingSelectedOUs = NULL
    # Only recompute champions when none were supplied at all (standalone R usage).
    # When the caller passes an empty vector — e.g. because the strict Shiny
    # reporting rule returned zero qualifying facilities — honour that result
    # rather than silently falling back to a different rule (which produced
    # inconsistent champion sets and chart spikes).
    if (is.null(reportingSelectedOUs) & nrow(data) > 0) {
      if (.cat) {
        cat("\n - finding most frequently reporting OUs")
      }

      # Pass only the 4 columns that mostFrequentReportingOUs() actually uses.
      # data is already a data.table here. Selecting fewer columns and deduplicating
      # before the call dramatically reduces the row count passed (e.g. 2.6M → ~880K)
      # since many rows share the same (orgUnit, period) across data elements.
      rou_period  <- if ("Month" %in% names(data)) "Month" else "Week"
      rou_cols    <- intersect(c("orgUnit", rou_period, "data", "original"), names(data))
      data_for_rous <- unique(data[!is.na(original), rou_cols, with = FALSE])

      # Compute date bounds lazily — only in this rare path where they're needed.
      # Use the processed time column (Month/Week), not the raw DHIS2 period string.
      if (is.null(startingMonth)) startingMonth = min(data[[rou_period]], na.rm = TRUE)
      if (is.null(endingMonth))   endingMonth   = max(data[[rou_period]], na.rm = TRUE)

      reportingSelectedOUs = mostFrequentReportingOUs(
        data_for_rous,
        data_categories = data_categories,
        startingMonth = startingMonth,
        endingMonth = endingMonth,
        missing_reports = missing_reports
      )
    }

    # Add var for selected ous
    if (.cat) {
      cat(
        '\n - selectedData length( reportingSelectedOUs()): ',
        length(reportingSelectedOUs)
      )
    }

    if (.cat) {
      cat("\n - Adding variable Selected: Champion or Non-Champion")
    }
    data[, Selected := fifelse(orgUnit %in% reportingSelectedOUs, 'Champion', 'Non-Champion')]
    # Only apply date-window filter when caller supplied an explicit startingMonth.
    # When startingMonth was NULL, it equals min(data), so the filter is a no-op
    # that copies the entire dataset for nothing.
    if (.startingMonth_explicit) {
      data = data[unclass(Month) >= unclass(as.yearmonth(startingMonth))]
    }

  } else {
    if (.cat) {
      cat("\n - adding Selected = All to data")
    }
    data[, Selected := "All"]
  }

  if (.cat) {
    cat(sprintf('\n - end selectedData  %.1f sec  %d rows', proc.time()["elapsed"] - .t0_selectedData, nrow(data)))
  }

  # TESTING
  # if ( .cat ) cat( '\n - if Testing, saving  selectedData() as data.rds' )
  # saveRDS( data , "data.rds" )

  return(data)
}

# merge datasets
#' Merge and aggregate datasets across org units and periods
#' @param data data frame or tsibble; the selected dataset to aggregate
#' @param group_by_cols character vector; column names to group by
#' @param period character; time index column name ("Month" or "Week")
#' @param startMonth yearmonth; start of the aggregation window
#' @param endMonth yearmonth; end of the aggregation window
#' @param dataSets character vector; dataset names to combine into "Combined"
#' @param sum.data logical; sum values across org units (default TRUE)
#' @param mean.merge logical; use mean instead of sum when merging (default FALSE)
#' @param covariates character vector; covariate column names to retain
#' @param .cat logical; print progress messages to the console
#' @return A tibble with one row per combination of `group_by_cols`, an
#'   aggregated `dataCol` column and a `total` column (`dataCol` with `NA`
#'   replaced by 0), restricted to `startMonth`..`endMonth` when both are given.
#' @export
dataTotal = function(
  data = NULL,
  group_by_cols = NULL,
  period = "Month",
  startMonth = NULL,
  endMonth = NULL,
  dataSets = NULL,
  sum.data = TRUE,
  mean.merge = FALSE,
  covariates = NULL,
  .cat = TRUE
) {
  if (.cat) {
    cat('\n* data Functions.R dataTotal()')
  }
  .t0_dataTotal <- proc.time()["elapsed"]

  if (is.null(period)) {
    period = dataPeriod(data)
  }

  if (!is.null(dataSets) && (any(nchar(dataSets) > 0))) {
    if (.cat) {
      cat("\n - combineSelectDatasets ")
    }

    mergeDatasets = dataSets %>% str_replace_all(fixed("\n"), "")

    if (.cat) {
      cat("\n - mergeDatasets ")
    }

    if (any(!is.na(mergeDatasets)) & any(nchar(mergeDatasets) > 0)) {
      data =
        setDT(data)[any(mergeDatasets %in% dataSet), dataSet := 'Combined'] %>%

        as_tibble
    }
  }

  if (.cat) {
    cat("\n - grouping ")
  }

  # if ( is.null( group_by_cols ) ){
  #   if ( .cat ) cat( "\n - group_by_cols ")
  #   group_by_cols = groupByCols( period = period  )
  # }

  # if ( any( grepl( "avg_mm" , names( data ) ) ) ){
  #
  #       if ( .cat )  cat( '\n - with avg_mm' )
  #
  #       data = setDT( data ) %>%
  #             .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
  #                     avg_mm = mean( avg_mm , na.rm = TRUE  ) ) ,
  #                by =  group_by_cols ]
  #
  #     } else {
  #       if ( .cat )   cat( "\n - creating dataCol")
  #
  #       data = setDT( data ) %>%
  #             .[ , .( dataCol = sum( dataCol , na.rm = TRUE  )) , by =  group_by_cols ]
  #     }

  if (.cat) {
    cat("\n - summarising covariates: ", covariates)
  }

  summary_cols = NULL
  if (!any(is.null(covariates)) && any(nchar(covariates) > 0)) {
    summary_cols = c(group_by_cols, covariates) %>% unique
  }
  if (.cat) {
    cat("\n - summary_cols: ", summary_cols)
  }
  if (.cat) {
    cat("\n - summarise group by: ", group_by_cols)
  }

  group_by_cols = str_remove(group_by_cols, fixed("`"))

  # Drop any group-by columns that don't exist in the data (e.g. combined
  # datasets don't have a 'dataSet' column)
  missing_cols  <- setdiff(group_by_cols, names(data))
  if (length(missing_cols) > 0) {
    if (.cat) cat("\n - dropping missing group_by_cols:", missing_cols)
    group_by_cols <- intersect(group_by_cols, names(data))
  }

  # Detect ratio variables: combined datasets carry numerator/denominator columns
  # so the pipeline can compute sum(num)/sum(den) instead of sum(ratio).
  has_ratio_cols <- all(c("numerator", "denominator") %in% names(data))

  .t0_grp <- proc.time()["elapsed"]
  if (.cat) cat('\n - nrow before grouping:', nrow(data))

  # For ratio datasets, enforce validity rules before grouping:
  #   - Missing numerator + valid denominator → numerator = 0 (zero positives is plausible;
  #     reporters often leave blank rather than enter 0).
  #   - Present numerator but missing or zero denominator → exclude both (orphaned numerator:
  #     cannot have positive tests without a total-tests denominator).
  #   - numerator > denominator → exclude both (impossible ratio; e.g. more positives than tests).
  if (has_ratio_cols) {
    data <- as.data.table(data)
    # Step 1: invalid denominator (missing or zero) → exclude both
    data[is.na(denominator) | denominator == 0,
         `:=`(numerator = NA_real_, denominator = NA_real_)]
    # Step 2: impossible ratio (numerator > denominator) → exclude both
    data[!is.na(numerator) & !is.na(denominator) & numerator > denominator,
         `:=`(numerator = NA_real_, denominator = NA_real_)]
    # Step 3: missing numerator with valid denominator → impute as 0
    data[is.na(numerator) & !is.na(denominator), numerator := 0]
  }

  if (!any(is.null(summary_cols)) && any(nchar(summary_cols) > 0)) {
    if (.cat) {
      cat("\n - and all_of : ", summary_cols)
    }
    cov_cols = setdiff(summary_cols, group_by_cols)
    if (has_ratio_cols) {
      data = as.data.table(data)[,
        c(.(dataCol     = sum(dataCol, na.rm = TRUE),
            numerator   = if (all(is.na(numerator)))   NA_real_ else sum(numerator,   na.rm = TRUE),
            denominator = if (all(is.na(denominator))) NA_real_ else sum(denominator, na.rm = TRUE)),
          lapply(.SD, \(x) mean(as.numeric(x), na.rm = TRUE))),
        by = group_by_cols,
        .SDcols = cov_cols
      ]
    } else {
      data = as.data.table(data)[,
        c(.(dataCol = sum(dataCol, na.rm = TRUE)),
          lapply(.SD, \(x) mean(as.numeric(x), na.rm = TRUE))),
        by = group_by_cols,
        .SDcols = cov_cols
      ]
    }
  } else {
    if (has_ratio_cols) {
      data = as.data.table(data)[,
        .(dataCol     = sum(dataCol, na.rm = TRUE),
          numerator   = if (all(is.na(numerator)))   NA_real_ else sum(numerator,   na.rm = TRUE),
          denominator = if (all(is.na(denominator))) NA_real_ else sum(denominator, na.rm = TRUE)),
        by = group_by_cols
      ]
    } else {
      data = as.data.table(data)[,
        .(dataCol = sum(dataCol, na.rm = TRUE)),
        by = group_by_cols
      ]
    }
  }

  # For ratio rows (denominator present and > 0), recompute dataCol from the
  # aggregated components — do not sum ratio values across groups.
  if (has_ratio_cols) {
    data = as.data.table(data)
    data[!is.na(denominator) & denominator > 0, dataCol := numerator / denominator]
    data[!is.na(denominator) & denominator == 0, dataCol := NA_real_]
    if (.cat) cat('\n - ratio dataCol recomputed from numerator/denominator')
  }

  if (.cat) cat(sprintf('\n - grouping done: %.1f sec  %d rows out', proc.time()["elapsed"] - .t0_grp, nrow(data)))

  # data = setDT( data ) %>%
  #   .[ ,  lapply( .SD, sum  ) ,
  #      by =  group_by_cols ]

  # if ( !any(is.null( covariates )) && any( grepl( "avg_mm", covariates ) ) && ! any( grepl( "rdt_so", covariates ) )) {
  #
  #   if (.cat) cat("\n - avg_mm")
  #
  #   data = setDT( data ) %>%
  #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
  #             avg_mm = mean( avg_mm , na.rm = TRUE  )  ) ,
  #        by =  group_by_cols ]
  # }
  #
  # if ( !any(is.null( covariates )) && any( grepl( "rdt_so", covariates ) ) && ! any( grepl( "avg_mm", covariates ) )) {
  #
  #   if (.cat) cat("\n - rdt_so")
  #
  #   data = setDT( data ) %>%
  #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
  #             rdt_so = mean( rdt_so, na.rm = T ) ) ,
  #        by =  group_by_cols ]
  # }
  #
  # if ( !any(is.null( covariates )) && any( grepl( "avg_mm", covariates ) ) && any( grepl( "rdt_so", covariates ) ) ) {
  #
  #   if (.cat) cat("\n - avg_mm and rdt_so")
  #
  #   data = setDT( data ) %>%
  #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  ) ,
  #             avg_mm = mean( avg_mm , na.rm = TRUE  ) ,
  #             rdt_so = mean( rdt_so, na.rm = T )  ) ,
  #        by =  group_by_cols ]
  # }
  #
  # if ( any(is.null( covariates )) ) {
  #
  #   if (.cat) cat("\n - no covariates "
  #                 )
  #   data = setDT( data ) %>%
  #     .[ , .( dataCol = sum( dataCol , na.rm = TRUE  )  ) ,
  #        by =  group_by_cols ]
  # }
  #

  if (mean.merge) {
    if (.cat) {
      cat('\n -  merge data.table MEAN')
    }

    data = data %>%
      mutate(dataSet = 'Merged') %>%
      setDT() %>%
      # Mean of dataSets within orgUnit
      .[, .(dataCol = mean(dataCol, na.rm = TRUE)), by = group_by_cols]

    if (.cat) cat('\n - Merge done') # glimpse( dataMerge )
  }

  key.cols = setdiff(group_by_cols, period)

  if (.cat) {
    cat('\n - key.cols', key.cols)
  }

  if (period %in% 'Month' & !is.null(startMonth) & !is.null(endMonth)) {
    if (.cat) {
      cat('\n - period', period, startMonth, endMonth)
    }

    data = setDT(data)[
      which(
        Month >= yearmonth(startMonth) &
          Month <= yearmonth(endMonth)
      ),
    ]
  }

  if (period %in% 'Week' & !is.null(startMonth) & !is.null(endMonth)) {
    if (.cat) {
      cat('\n - period', period, startMonth, endMonth)
    }

    data = setDT(data)[
      which(
        Week >= yearweek(startMonth) &
          Week <= yearweek(endMonth)
      ),
    ]
  }

  ## NB does data.total need to be a Tsibble?--it is slow.

  data.total = setDT(data)[, total := replace_na(dataCol, 0), ] %>%
    as_tibble()

  # if (.cat ) cat( "\n - data.total columns:" , paste( names( data.total ) , collapse = ", "))

  if (.cat) {
    cat(sprintf('\n - end dataTotal  %.1f sec total  %d rows', proc.time()["elapsed"] - .t0_dataTotal, nrow(data.total)))
  }

  # saveRDS( data.total , "data.total.rds")

  return(data.total)
}

#' Build formula for hierarchical time-series aggregation
#' @param hts logical; use hierarchical time-series aggregation
#' @param levelNames character vector; administrative level names
#' @param agg_level character; target aggregation level name
#' @param all.levels logical; include all levels in the formula
#' @param num_facilities integer; number of facilities in the dataset
#' @param num_datasets integer; number of datasets in the dataset
#' @param split character; split variable ("None" or a column name)
#' @param .cat logical; print progress messages to the console
#' @noRd
htsFormula = function(
  hts = TRUE,
  levelNames = NULL,
  agg_level = NULL,
  all.levels = FALSE,
  num_facilities = NULL,
  num_datasets = NULL,
  split = 'None',
  .cat = FALSE
) {
  if (.cat) {
    cat("\n* htsFormula():")
  }

  htsFormula = ""

  if (hts) {
    if (is.null(levelNames)) {
      # (previously getLevelNames(orgUnits = orgUnits), with orgUnits undefined)
      stop("htsFormula(): `levelNames` is required when hts = TRUE", call. = FALSE)
    }

    adms = backtick(levelNames)

    if (all.levels) {
      htsFormula = paste(adms, collapse = "/")
    } else {
      if (!is.null(agg_level)) {
        if (.cat) {
          cat('\n - adms:', adms)
        }
        if (.cat) {
          cat('\n - input$agg_level:', agg_level)
        }

        hts_level = which(agg_level == levelNames)

        if (.cat) {
          cat('\n - hts_level:', hts_level)
        }

        # hts = paste( adms[1:( hts_level + 1 )] ,
        #            collapse = "/" )

        htsFormula = paste(adms[1:hts_level], collapse = "/")

        htsFormula = paste("(", hts, ")")
      }
    }
  }

  # if >1 Facilities (ie. selected)
  if (num_facilities > 1) {
    htsFormula = paste(
      'Selected *',
      htsFormula
    )
  }

  # if >1 dataset
  if (num_datasets > 1) {
    htsFormula = paste(
      'dataSet *',
      htsFormula
    )
  }

  # # Cross by split
  if (any(!split %in% 'None')) {
    htsFormula =
      paste(paste(backtick(split), collapse = " * "), '*', htsFormula)
  }
  #
  # Cross by selected and split
  # if ( length( reportingSelectedOUs() ) > 0  & !input$split %in% 'None' ) hts =
  #   paste( input$split ,  ' * Facilities * (', hts , ')' )

  # remove a trailing * ...
  htsFormula = str_trim(htsFormula)

  if (str_ends(htsFormula, fixed("*"))) {
    hts.length = nchar(htsFormula)
    htsFormula = substr(htsFormula, 1, hts.length - 1)
  }

  if (.cat) {
    cat("\n - end htsFormula():", htsFormula)
  }

  return(htsFormula)
}

#' Aggregate data using hierarchical time-series structure
#' @param data tsibble; the prepared dataset
#' @param hts logical; apply hierarchical aggregation
#' @param hts_formula character; hts formula string from `htsFormula()`
#' @param covariates character; covariate column names (space-separated)
#' @param group_by_cols character vector; column names used for grouping
#' @param .cat logical; print progress messages to the console
#' @param timing logical; print timing information
#' @param ... additional arguments
#' @noRd
htsData = function(
  data = NULL,
  hts = TRUE,
  hts_formula = NULL,
  covariates = "",
  group_by_cols = NULL,
  .cat = FALSE,
  timing = FALSE,
  ...
) {
  if (.cat) {
    cat('\n* htsData:')
  }

  .t0 <- Sys.time()

  if (is.null(data)) {
    if (.cat) {
      cat('\n - end htsData(): data is missing ')
    }
    return(data)
  }

  if (is.null(hts_formula)) {
    if (.cat) {
      cat('\n - end htsData(): hts_formula NULL ')
    }
    return(data)
  }

  if (is.null(group_by_cols)) {
    group_by_cols = groupByCols(period = dataPeriod(data))
  }

  if (!is_tsibble(data)) {
    if (.cat) {
      cat('\n - preparing data.total as tsibble')
    }

    period = dataPeriod(data)

    # Restore yearmonth/yearweek class stripped by data.table operations upstream.
    # yearmonth stores days-since-epoch (~18000-20000) internally; just reattach
    # the S3/vctrs class without re-interpreting the values (yearmonth(x) would
    # treat x as months-since-epoch and produce dates in year ~3491 AD).
    if (period == "Month" && !inherits(data[["Month"]], "yearmonth"))
      data[["Month"]] <- structure(data[["Month"]], class = c("yearmonth", "vctrs_vctr"))
    if (period == "Week" && !inherits(data[["Week"]], "yearweek"))
      data[["Week"]] <- structure(data[["Week"]], class = c("yearweek", "vctrs_vctr"))

    key.cols = setdiff(group_by_cols, period)

    data = data %>%
      as_tsibble(index = !!rlang::sym(period), key = all_of({{ key.cols }}))
  }

  # Testing
  # saveRDS( data.total(), 'data.total.hts.rds' )

  # exogenous variables

  if (.cat) {
    cat("\n - covariates:", covariates)
  }
  if (.cat) {
    cat("\n - names(data):", paste(names(data), collapse = ", "))
  }

  # if ( !any(is.null( covariates ))  ){

  # xreg.var = covariates

  # Use admin hts
  if (hts) {
    if (.cat) {
      cat("\n - hierarchical aggregations ")
    }

    if (any(grepl("avg_mm", covariates)) && !any(grepl("rdt_so", covariates))) {
      data = data %>%
        aggregate_key(
          .spec = !!rlang::parse_expr(hts_formula),
          total = sum(total, na.rm = T),
          avg_mm = mean(!!rlang::parse_expr('avg_mm'), na.rm = T)
          # xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
        )
    }

    if (any(grepl("rdt_so", covariates)) && !any(grepl("avg_mm", covariates))) {
      data = data %>%
        aggregate_key(
          .spec = !!rlang::parse_expr(hts_formula),
          total = sum(total, na.rm = T),
          rdt_so = mean(!!rlang::parse_expr('rdt_so'), na.rm = T)
          # xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
        )
    }

    if (any(grepl("avg_mm", covariates)) && any(grepl("rdt_so", covariates))) {
      data = data %>%
        aggregate_key(
          .spec = !!rlang::parse_expr(hts_formula),
          total = sum(total, na.rm = T),
          avg_mm = mean(!!rlang::parse_expr('avg_mm'), na.rm = T),
          rdt_so = mean(!!rlang::parse_expr('rdt_so'), na.rm = T)
          # xreg.var := sum( !!rlang::parse_expr( xreg.var ) , na.rm = T )
        )
    }

    if (
      !any(grepl("avg_mm", covariates)) && !any(grepl("rdt_so", covariates))
    ) {
      data = data %>%
        aggregate_key(
          .spec = !!rlang::parse_expr(hts_formula),
          total = sum(total, na.rm = T)
        )
    }
  }

  if (.cat) {
    cat('\n - end htsData(): ', paste(names(data), collapse = ", "))
  }

  if (timing) {
    cat(.mg2_elapsed(.t0))
  }

  return(data)
}


#' Aggregate data with optional covariates
#' @noRd
aggData = function(
  data.total = NULL,
  covariates = NULL,
  group_by_cols = NULL,
  .cat = FALSE,
  timing = FALSE,
  ...
) {
  if (.cat) {
    cat('\n* aggData:')
  }

  if (.cat) {
    cat('\n - covariates:', covariates)
  }
  if (.cat) {
    cat('\n - group_by_cols: ', group_by_cols)
  }
  # Testing
  # saveRDS( data.total , "data.total.rds")

  .t0 <- Sys.time()

  if (is.null(data.total)) {
    if (.cat) {
      cat('\n - end htsData(): data is missing ')
    }
    return(data.total)
  }

  if (is.null(group_by_cols)) {
    group_by_cols = groupByCols(period = dataPeriod(data))
  }

  # Drop any group-by columns that don't exist in the data (e.g. when agg_level
  # column isn't present at Federal level after a directory switch).
  missing_cols <- setdiff(group_by_cols, names(data.total))
  if (length(missing_cols) > 0) {
    if (.cat) cat("\n - dropping missing group_by_cols:", missing_cols)
    group_by_cols <- intersect(group_by_cols, names(data.total))
  }

  # if ( "orgUnit" %in% group_by_cols ) group_by_cols = setdiff( group_by_cols , "orgUnit" )

  # Testing
  # saveRDS( data.total(), 'data.total.hts.rds' )

  # exogenous variables

  # if ( !any(is.null( covariates ))  ){

  # xreg.var = covariates

  # Use admin hts
  if (.cat) {
    cat("\n - aggregations ")
  }

  summary_cols = NULL
  if (!any(is.null(covariates))) {
    summary_cols = c(summary_cols, covariates)
  }
  if (.cat) {
    cat("\n - summary_cols: ", summary_cols)
  }

  if (any(nchar(summary_cols) > 1)) {
    data = data.total %>%
      group_by_at(vars({{ group_by_cols }})) %>%
      summarise(
        across(total, \(x) sum(x, na.rm = TRUE)),
        across(all_of(summary_cols), \(x) mean(x, na.rm = TRUE))
      )
  } else {
    if (.cat) {
      cat("\n - no summary_cols")
    }
    data = data.total %>%
      group_by_at(vars({{ group_by_cols }})) %>%
      summarise(
        across(total, \(x) sum(x, na.rm = TRUE))
      )
  }

  if (timing) {
    cat(.mg2_elapsed(.t0))
  }

  if (!is_tsibble(data)) {
    if (.cat) {
      cat('\n - preparing data.total as tsibble')
    }

    period = dataPeriod(data)

    # Restore yearmonth/yearweek class stripped by data.table operations upstream.
    # yearmonth stores days-since-epoch (~18000-20000) internally; just reattach
    # the S3/vctrs class without re-interpreting the values (yearmonth(x) would
    # treat x as months-since-epoch and produce dates in year ~3491 AD).
    if (period == "Month" && !inherits(data[["Month"]], "yearmonth"))
      data[["Month"]] <- structure(data[["Month"]], class = c("yearmonth", "vctrs_vctr"))
    if (period == "Week" && !inherits(data[["Week"]], "yearweek"))
      data[["Week"]] <- structure(data[["Week"]], class = c("yearweek", "vctrs_vctr"))

    key.cols = setdiff(group_by_cols, period)

    data = data %>%
      as_tsibble(index = !!rlang::sym(period), key = all_of({{ key.cols }}))
  }

  return(data)
}








#' Compute mean absolute percentage error
#' @param preForecast fable or data frame; forecast output with a `.mean` column
#' @param trend.data tsibble; actual observed values
#' @param period character; time index column name ("Month" or "Week")
#' @param var character; name of the forecast point-estimate column (default ".mean")
#' @param .cat logical; print progress messages to the console
#' @noRd
MAPE = function(
  preForecast,
  trend.data,
  period = "Month",
  var = ".mean",
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* evaluation_widget MAPE()')
  }

  predicted = preForecast %>% as_tibble() %>% select(-total)
  actual = trend.data

  cols = intersect(names(predicted), names(actual))

  d = predicted %>%
    inner_join(actual, by = cols)

  e = d %>%
    as_tibble() %>%
    # group_by( orgUnit , data  )  %>%
    summarise(
      mape = ifelse(
        mean(total, na.rm = T) > 0,
        mean(abs(total - {{ var }}), na.rm = T) /
          mean(total, na.rm = T),
        NA
      )
    )

  if (.cat) {
    cat('\n* - ', e$mape)
  }

  return(scales::percent(e$mape))
}



#' Prepare Aggregated Time-Series Data for Modelling
#'
#' Filters, aggregates, and optionally arranges data into a hierarchical
#' time series (mts/hts) ready for fable model fitting.
#'
#' @param tibble.data A tsibble (output of [data_1()] after outlier detection).
#' @param .orgUnit Logical. Group by org unit (default `TRUE`).
#' @param .startingMonth Start of analysis period (yearmonth or character).
#' @param .endingMonth End of analysis period (yearmonth or character).
#' @param .missing_reports Integer. Allowed missing reports threshold.
#' @param selected.only Logical. Champion facilities only (default `TRUE`).
#' @param .error Character. Outlier filter level (e.g. `"mad15"`).
#' @param covariates Character vector of covariate column names.
#' @param .split Character. Column to split data by.
#' @param aggregate_by_split Logical. Aggregate within split levels.
#' @param levelNames Character vector of org unit level names.
#' @param hts Logical. Build hierarchical time series (default `FALSE`).
#' @param agg_level Character. Aggregation level name.
#' @param remove.aggregate Logical. Remove aggregate row (default `TRUE`).
#' @param .cat Logical. Print progress messages (default `FALSE`).
#' @param testing Logical. Save intermediate objects for debugging.
#' @param ... Passed to sub-functions.
#'
#' @return A tsibble aggregated to the requested level, ready for modelling.
#' @noRd
mable_data = function(
  tibble.data = NULL,
  .orgUnit = TRUE,
  .startingMonth = NULL,
  .endingMonth = NULL,
  .missing_reports = NULL,
  selected.only = TRUE,
  .error = NULL,
  covariates = NULL,
  .split = NULL,
  aggregate_by_split = TRUE,
  levelNames = NULL,
  hts = FALSE,
  agg_level = NULL,
  remove.aggregate = TRUE,
  .cat = FALSE,
  testing = FALSE,
  ...
) {
  if (.cat) cat("\n* mable_data")

  if (testing) {
    saveRDS(tibble.data, "tibble.data.rds")
    save(.orgUnit, hts, remove.aggregate, .error, .startingMonth, .endingMonth,
         .missing_reports, agg_level, levelNames, .split, covariates,
         file = "mable_data_parameters.rda")
  }

  d = tibble.data %>% error_factor

  if (is.null(.error) || .error == "Original") {
    if (.cat) cat("\n - source is original")
    d = d %>% mutate(dataCol = original)
  }

  if (!is.null(.error) & "error" %in% names(d)) {
    if (.cat) cat("\n - error level:", .error)
    error.levels = levels(d$error)
    error.factor.value = which(.error == error.levels)
    d = d %>%
      mutate(dataCol = ifelse(as.numeric(error) > error.factor.value, original, NA))
  }

  group_by_cols = groupByCols(
    period     = dataPeriod(d),
    selected   = FALSE,
    dataset    = FALSE,
    orgUnit    = .orgUnit,
    hts        = hts,
    agg_level  = agg_level,
    levelNames = levelNames,
    split      = .split,
    .cat       = .cat
  )

  if (.cat) cat("\n - group_by_cols", group_by_cols)

  num_facilities = mostFrequentReportingOUs(
    d,
    startingMonth  = .startingMonth,
    endingMonth    = .endingMonth,
    missing_reports = .missing_reports,
    .cat           = .cat
  ) %>% length()

  if (.cat) cat("\n - num_facilities:", num_facilities)

  .dataSets    = unique(d$dataSet)
  num_datasets = length(.dataSets)
  if (.cat) cat("\n - num_datasets:", num_datasets)

  data.total = dataTotal(
    data          = d,
    group_by_cols = group_by_cols,
    dataSets      = NULL,
    covariates    = covariates,
    .cat          = .cat
  )

  if (hts) {
    if (.cat) cat("\n - hts")
    hts_formula = htsFormula(
      hts            = hts,
      levelNames     = levelNames,
      agg_level      = agg_level,
      all.levels     = FALSE,
      num_facilities = num_facilities,
      num_datasets   = num_datasets,
      split          = .split,
      .cat           = .cat
    )
    data.agg.ts = htsData(
      data          = data.total,
      hts           = hts,
      hts_formula   = hts_formula,
      covariates    = covariates,
      group_by_cols = group_by_cols,
      .cat          = .cat
    )
  } else {
    if (.cat) cat("\n - aggData")
    data.agg.ts = aggData(
      data.total    = data.total,
      covariates    = covariates,
      group_by_cols = group_by_cols,
      .cat          = .cat
    )
  }

  if (testing) saveRDS(data.agg.ts, "data.agg.ts.rds")

  if (.cat) cat("\n - end mable_data")
  return(data.agg.ts)
}


#' Split a Time Series into Training, Test, and Post-Intervention Windows
#'
#' Creates a list of tsibble subsets used for pre/post intervention analysis
#' and model training/testing.
#'
#' @param data A tsibble aggregated by `mable_data()`.
#' @param startMonth Start of analysis window (yearmonth).
#' @param startEvalMonth First month of the evaluation (intervention) period.
#' @param numberTestMonths Number of months held out for model testing.
#' @param endEvalMonth Last month of the evaluation period.
#' @param unadjusted Logical. Use unadjusted values (currently unused).
#' @param grouping Logical. Summarise by a grouping variable.
#' @param groups Character. Column name to group by when `grouping = TRUE`.
#'
#' @return A named list with elements `fable.data`, `pre.intervention`,
#'   `pre.intervention.train`, `pre.intervention.test`, `post.intervention`,
#'   `post.intervention.yr1`, `post.intervention.yr2`, `post.intervention.yr3`.
#' @export
dataset = function(
  data            = NULL,
  startMonth      = tsibble::yearmonth("Jan 2015"),
  startEvalMonth  = tsibble::yearmonth("Jan 2020"),
  numberTestMonths = 12,
  endEvalMonth    = tsibble::yearmonth("Dec 2022"),
  unadjusted      = FALSE,
  grouping        = FALSE,
  groups          = "agegrp"
) {
  if (!"tbl_ts" %in% class(data)) {
    data = tsibble::tsibble(data, index = "Month", key = groups)
  }

  if (grouping) {
    groups = rlang::syms(groups)
    fable.data = data %>%
      dplyr::filter(Month >= startMonth) %>%
      dplyr::group_by({{ groups }}) %>%
      dplyr::summarise(total = sum(total, na.rm = TRUE))
  } else {
    fable.data = data %>%
      dplyr::filter(Month >= startMonth, Month <= endEvalMonth) %>%
      dplyr::summarise(total = sum(total, na.rm = TRUE))
  }

  pre.intervention = fable.data %>%
    dplyr::filter(Month < startEvalMonth)

  pre.intervention.train = fable.data %>%
    dplyr::filter(Month < startEvalMonth - numberTestMonths)

  pre.intervention.test = fable.data %>%
    dplyr::filter(
      Month >= startEvalMonth - numberTestMonths &
      Month <  startEvalMonth
    )

  post.intervention = fable.data %>%
    dplyr::filter(Month >= startEvalMonth & Month <= endEvalMonth)

  post.intervention.yr1 = fable.data %>%
    dplyr::filter(Month >= startEvalMonth & Month < startEvalMonth + 12)

  post.intervention.yr2 = fable.data %>%
    dplyr::filter(Month >= startEvalMonth + 12 & Month < startEvalMonth + 24)

  post.intervention.yr3 = fable.data %>%
    dplyr::filter(Month >= startEvalMonth + 24 & Month < startEvalMonth + 36)

  return(list(
    fable.data              = fable.data,
    pre.intervention        = pre.intervention,
    pre.intervention.train  = pre.intervention.train,
    pre.intervention.test   = pre.intervention.test,
    post.intervention       = post.intervention,
    post.intervention.yr1   = post.intervention.yr1,
    post.intervention.yr2   = post.intervention.yr2,
    post.intervention.yr3   = post.intervention.yr3
  ))
}


#' Yearly Summary Table with Percent Change (Flextable)
#'
#' Creates a colour-coded flextable showing yearly totals and year-on-year
#' percent change.  Increases are red, decreases are green.
#'
#' @param data A tsibble (output of `mable_data()`).
#' @param date_col Character. Name of the date index column (default `"Month"`).
#' @param value_col Character. Name of the value column (default `"total"`).
#' @param country_col Character. Name of the country/national column (default `"National"`).
#' @param table_title Character. Table title shown in the header.
#' @param positive_color Character. Hex colour for positive changes (default red).
#' @param negative_color Character. Hex colour for negative changes (default green).
#'
#' @return A `flextable` object.
#' @export
yearly_summary_table <- function(
  data,
  date_col       = "Month",
  value_col      = "total",
  country_col    = "National",
  table_title    = "Yearly Summary with Percent Change",
  positive_color = "#e74c3c",
  negative_color = "#27ae60"
) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer",   quietly = TRUE)) {
    stop(
      "Packages 'flextable' and 'officer' are required for yearly_summary_table(). ",
      "Install them with: install.packages(c('flextable', 'officer'))"
    )
  }
  if (!inherits(data, "tbl_ts")) stop("Data must be a tsibble object")
  if (!all(c(date_col, value_col) %in% colnames(data))) {
    stop("Specified columns not found in data")
  }

  # Detect whether the most recent year is partial (< 12 months of data).
  idx_sym     <- tsibble::index(data)
  idx_var     <- tsibble::index_var(data)
  all_years   <- as.integer(format(data[[idx_var]], "%Y"))
  last_year   <- max(all_years, na.rm = TRUE)
  last_months <- sort(unique(as.integer(format(data[[idx_var]][all_years == last_year], "%m"))))
  is_partial  <- length(last_months) < 12

  # Full-year totals for all years (used for all rows except the last when partial).
  # as_tibble() strips the tsibble class so subsequent mutate() calls on Year
  # (which is the tsibble index) don't trigger "Unsupported index type" errors.
  yearly_full <- data %>%
    dplyr::ungroup() %>%
    tsibble::index_by(Year = as.integer(format(!!idx_sym, "%Y"))) %>%
    dplyr::summarise(
      Total        = sum(!!rlang::sym(value_col), na.rm = TRUE),
      months_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(Year)

  if (is_partial) {
    # For the last row: show its partial total; compute % change against the
    # same months in the prior year so the comparison is like-for-like.
    # Use as_tibble() to drop tsibble key grouping before summarising.

    partial_label <- paste0(
      month.abb[min(last_months)], "\u2013", month.abb[max(last_months)]
    )

    # Subset total for every year: sum only the months present in last_months.
    # This gives the like-for-like denominator column alongside the % change.
    yearly_subset <- data %>%
      tibble::as_tibble() %>%
      dplyr::filter(as.integer(format(.data[[idx_var]], "%m")) %in% last_months) %>%
      dplyr::group_by(Year = as.integer(format(.data[[idx_var]], "%Y"))) %>%
      dplyr::summarise(`Subset Total` = sum(.data[[value_col]], na.rm = TRUE),
                       .groups = "drop")

    prior_partial_total <- yearly_subset$`Subset Total`[yearly_subset$Year == last_year - 1]
    prior_partial_total <- sum(prior_partial_total, na.rm = TRUE)  # collapse to scalar

    last_total <- yearly_full$Total[yearly_full$Year == last_year]
    last_pct   <- if (isTRUE(prior_partial_total != 0))
      round((last_total - prior_partial_total) / prior_partial_total * 100, 1)
    else NA_real_

    # Build yearly_data: full-year totals + subset totals + % change.
    # % change is full-year vs full-year for complete years; for the last (partial)
    # year it compares the same months in both years.
    yearly_data <- yearly_full %>%
      dplyr::left_join(yearly_subset, by = "Year") %>%
      dplyr::mutate(
        `Percent Change` = round((Total - dplyr::lag(Total)) / dplyr::lag(Total) * 100, 1)
      )
    last_row <- nrow(yearly_data)
    yearly_data$`Percent Change`[last_row] <- last_pct

    # Label the last year to make the partial period visible
    yearly_data <- yearly_data %>%
      dplyr::mutate(
        Year = dplyr::if_else(Year == last_year,
                              paste0(Year, "\n(", partial_label, ")"),
                              as.character(Year))
      )
    col_header <- "% Change"
  } else {
    yearly_data <- yearly_full %>%
      dplyr::mutate(
        `Percent Change` = round((Total - dplyr::lag(Total)) / dplyr::lag(Total) * 100, 1)
      )
    col_header <- "% Change from Previous Year"
  }

  yearly_data <- yearly_data %>%
    dplyr::mutate(
      `Change Direction` = dplyr::case_when(
        is.na(`Percent Change`)  ~ "baseline",
        `Percent Change` > 0    ~ "positive",
        `Percent Change` < 0    ~ "negative",
        TRUE                    ~ "zero"
      ),
      `Formatted Change` = dplyr::case_when(
        is.na(`Percent Change`)  ~ "\u2014",
        `Percent Change` > 0    ~ paste0("+", `Percent Change`, "%"),
        TRUE                    ~ paste0(`Percent Change`, "%")
      )
    )

  # For partial years: include Subset Total column between Total and % Change.
  has_subset <- is_partial && "Subset Total" %in% colnames(yearly_data)

  if (has_subset) {
    ft_data <- yearly_data %>%
      dplyr::select(Year, Total, `Subset Total`, `Formatted Change`)
  } else {
    ft_data <- yearly_data %>%
      dplyr::select(Year, Total, `Formatted Change`)
  }

  ft <- ft_data %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      Year               = "Year",
      Total              = "Annual\nTotal",
      `Subset Total`     = "Total",
      `Formatted Change` = col_header
    ) %>%
    flextable::colformat_double(j = "Total",        big.mark = ",", digits = 0) %>%
    flextable::colformat_double(j = "Year",         big.mark = "",  digits = 0)

  if (has_subset) {
    ft <- ft %>%
      flextable::colformat_double(j = "Subset Total", big.mark = ",", digits = 0)
  }

  ft <- ft %>%
    flextable::bg(
      i = which(yearly_data$`Change Direction` == "positive"),
      j = "Formatted Change", bg = positive_color
    ) %>%
    flextable::bg(
      i = which(yearly_data$`Change Direction` == "negative"),
      j = "Formatted Change", bg = negative_color
    ) %>%
    flextable::bg(
      i = which(yearly_data$`Change Direction` == "baseline"),
      j = "Formatted Change", bg = "#95a5a6"
    ) %>%
    flextable::color(
      i = which(yearly_data$`Change Direction` %in% c("positive", "negative", "baseline")),
      j = "Formatted Change", color = "white"
    ) %>%
    flextable::theme_vanilla() %>%
    flextable::bg(part = "header", bg = "#2c3e50") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::bold(part = "header") %>%
    flextable::align(
      j     = if (has_subset) c("Total", "Subset Total", "Formatted Change")
              else c("Total", "Formatted Change"),
      align = "center", part = "body"
    ) %>%
    flextable::align(j = "Year", align = "left", part = "body") %>%
    flextable::fontsize(part = "body", size = 11) %>%
    flextable::width(j = "Year",            width = 1) %>%
    flextable::width(j = "Total",           width = 1.3)

  if (has_subset) {
    ft <- ft %>%
      flextable::width(j = "Subset Total",    width = 1.3) %>%
      flextable::width(j = "Formatted Change", width = 1.2) %>%
      # Spanner row spanning Subset Total + % Change, labelled with the month range
      flextable::add_header_row(
        top       = TRUE,
        values    = c("", "", partial_label),
        colwidths = c(1L, 1L, 2L)
      ) %>%
      flextable::align(i = 1, align = "center", part = "header") %>%
      flextable::bg(i = 1, bg = "#3d5166", part = "header") %>%
      flextable::color(i = 1, color = "white", part = "header") %>%
      flextable::bold(i = 1, part = "header")
  } else {
    ft <- ft %>%
      flextable::width(j = "Formatted Change", width = 2)
  }

  ft <- ft %>%
    flextable::border_outer(
      border = officer::fp_border(color = "#2c3e50", width = 2)
    ) %>%
    flextable::border_inner_h(
      border = officer::fp_border(color = "#bdc3c7", width = 1)
    ) %>%
    flextable::border_inner_v(
      border = officer::fp_border(color = "#bdc3c7", width = 1)
    )

  return(ft)
}


#' Create Ensemble Forecast Combinations from Primary Forecasts
#'
#' Generates all pairwise and higher-order combinations of model forecasts,
#' averaging sample paths within each combination.
#'
#' @param primary.forecasts A fable object of primary model forecasts.
#' @param only.models A data frame with a `.model` column restricting which
#'   combinations are returned.
#'
#' @return A fable object with all combination models appended.
#' @noRd
combination_forecasts = function(primary.forecasts, only.models = NULL) {
  # distributional is in Suggests but always installed: fabletools imports it.
  if (!requireNamespace("distributional", quietly = TRUE)) {
    stop("Ensemble forecasts need the 'distributional' package.", call. = FALSE)
  }
  split_f = primary.forecasts %>% dplyr::group_by(.model) %>% dplyr::group_split()
  model.names = unique(primary.forecasts$.model)
  names(split_f) = model.names

  allf = primary.forecasts

  if (!is.null(only.models) && all(only.models$.model %in% model.names)) {
    return(primary.forecasts %>% dplyr::inner_join(only.models, by = dplyr::join_by(.model)))
  }

  if (is.null(only.models)) {
    all_combinations = unlist(lapply(seq_along(model.names), function(x) {
      utils::combn(model.names, x, paste, collapse = "")
    }))
    combo.model.names = setdiff(all_combinations, model.names)
  } else {
    all_combinations  = unique(only.models$.model)
    combo.model.names = setdiff(all_combinations, model.names)
  }

  if (length(combo.model.names) == 0) combo.model.names = only.models$.model

  split_combination <- function(combination, original_values) {
    pattern = paste(original_values, collapse = "|")
    strsplit(combination, split = paste0("(?<=", pattern, ")"), perl = TRUE)[[1]]
  }

  join_vars = setdiff(
    c(tsibble::index_var(primary.forecasts), tsibble::key_vars(primary.forecasts)),
    ".model"
  )

  for (x in seq_along(combo.model.names)) {
    indiv.model.names = split_combination(combo.model.names[x], model.names)
    nmodels = length(indiv.model.names)  # nchar() breaks with multi-char model names (p1, p4, p8)

    joined_fables = purrr::reduce(split_f[indiv.model.names], dplyr::left_join, by = join_vars)
    if (!"older.x" %in% names(joined_fables)) joined_fables$older.x = 0

    sample.cols = colnames(joined_fables)[grep("sample", colnames(joined_fables))]

    cf = joined_fables %>%
      dplyr::mutate(
        samples = purrr::pmap(dplyr::pick(dplyr::all_of(sample.cols)),
                              ~ purrr::reduce(list(...), `+`) / nmodels),
        older   = older.x
      ) %>%
      dplyr::mutate(.model = combo.model.names[x]) %>%
      dplyr::group_by(.model) %>%
      dplyr::mutate(
        .mean = purrr::map_dbl(samples, mean),
        .sd   = purrr::map_dbl(samples, stats::sd),
        var   = distributional::dist_normal(mean = .mean, sd = .sd)
      ) %>%
      dplyr::select(
        tsibble::key_vars(primary.forecasts),
        tsibble::index_var(primary.forecasts),
        var, .mean, older, samples
      )

    dimnames(cf$var) = "var"
    allf = dplyr::bind_rows(allf, cf)
  }

  if (!is.null(only.models)) {
    allf = allf %>% dplyr::inner_join(only.models, by = dplyr::join_by(.model))
  }
  return(allf)
}


#' Rank Forecast Models by Accuracy Metric
#'
#' Returns accuracy rows ranked by a chosen metric, with percent difference
#' from the best model and cross-replicate mean appended.
#'
#' @param fables_accuracy A data frame of model accuracy metrics (e.g. SWAPE).
#' @param metric Character. Column name of the accuracy metric to rank by.
#' @param top Integer. Maximum number of rows to return per group.
#' @param grouping Logical. Rank within grouping variable.
#' @param groups Character. Grouping variable column name.
#'
#' @return A data frame ranked by `metric`.
#' @noRd
best_fables_accuracy = function(
  fables_accuracy,
  metric   = "MAPE",
  top      = 1000,
  grouping = FALSE,
  groups   = "Intervention"
) {
  .metric = rlang::sym(metric)

  mean_model_metric = fables_accuracy %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.model) %>%
    dplyr::summarise(mean = mean({{ .metric }}))

  out = fables_accuracy %>%
    { if (grouping) dplyr::group_by(., !!rlang::sym(groups)) else . } %>%
    dplyr::arrange({{ .metric }}) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), \(x) round(x, digits = 3)),
      `Intervention Rank` = dplyr::row_number(),
      percent_diff = round(100 * (({{ .metric }} - dplyr::first({{ .metric }})) /
                                    dplyr::first({{ .metric }})), digits = 3)
    ) %>%
    dplyr::filter(`Intervention Rank` <= top) %>%
    dplyr::left_join(mean_model_metric, by = ".model")

  return(out)
}


#' Fit Forecast Models and Generate Predictions
#'
#' Fits ARIMA, ETS, NNETAR, TSLM, and Prophet models to training data and
#' produces sample-based forecast paths.  Optionally includes ensemble
#' combination models.
#'
#' @param train_data A tsibble of pre-intervention training data.
#' @param test_data A tsibble of hold-out test data (same keys as `train_data`).
#' @param n_forecasts Integer. Number of sample paths to generate (default 2000).
#' @param .var Character. Name of the variable to model (default `"total"`).
#' @param numberForecastMonths Integer. Forecast horizon in months.
#' @param type Character or NA. One of `"transform and covariate"`,
#'   `"transform"`, `"covariate"`, or `NA` for plain models.
#' @param covariate Character. Covariate column name (when `type` includes it).
#' @param ensemble Logical. Also return ensemble combination forecasts.
#' @param msg Logical. Print progress messages.
#' @param .set.seed Logical. Set random seed for reproducibility.
#'
#' @param base_models Character vector of base-model short names to fit
#'   (e.g. `c("a", "e")` for ARIMA + ETS only).  `NULL` (default) fits all.
#'   Valid names: `"a"` ARIMA, `"e"` ETS, `"n"` NNETAR, `"t"` TSLM,
#'   `"p1"`, `"p4"`, `"p8"` Prophet variants.
#' @return A fable object of forecasts (primary + ensemble if `ensemble = TRUE`).
#' @export
tsmodels = function(
  train_data,
  test_data,
  n_forecasts          = 2000,
  .var                 = "total",
  numberForecastMonths = 12,
  type                 = NA,
  covariate            = NULL,
  ensemble             = TRUE,
  msg                  = TRUE,
  .set.seed            = TRUE,
  base_models          = NULL
) {
  valid_types = c("transform and covariate", "transform", "covariate")
  if (!is.na(type) && !type %in% valid_types) {
    warning("tsmodels(): `type` must be one of: ", paste(valid_types, collapse = ", "), call. = FALSE)
    return(NULL)
  }

  .var_sym    = rlang::sym(.var)
  train_data  = train_data %>% dplyr::mutate(var = !!.var_sym)
  test_data   = test_data  %>%
    dplyr::mutate(var = !!.var_sym) %>%
    dplyr::select(-!!.var_sym, -var)

  if (!is.null(covariate)) covariate = rlang::sym(covariate)

  # Prophet models require fable.prophet (heavy rstan/Stan dependency — optional).
  # Skip prophet when training data is too short (< 24 months) to avoid OOM crashes
  # on memory-constrained servers (Connect Cloud). Fitting prophet with ~14 months of
  # training data reliably kills the R process via SIGKILL, producing an empty error.
  #
  # When prophet IS used, fit all models in ONE fabletools::model() call to keep the
  # mable structure intact. Splitting into two calls and merging via [[<- corrupts the
  # mable's tsibble attributes, breaking downstream dplyr::select() calls.
  # A tryCatch falls back to base-only if prophet errors (non-OOM failures).
  .has_prophet   <- requireNamespace("fable.prophet", quietly = TRUE)
  n_train_months <- length(unique(train_data[[tsibble::index_var(train_data)]]))
  .use_prophet   <- .has_prophet && n_train_months >= 24

  if (msg && !.has_prophet)
    cat("\n - tsmodels: fable.prophet not installed; prophet models skipped")
  if (msg && .has_prophet && !.use_prophet)
    cat("\n - tsmodels: prophet skipped (only", n_train_months,
        "training months; need >= 24)")

  # fabletools::model() detects any active future plan and tries to use
  # future.apply for internal parallelism. On Connect Cloud, future.apply is
  # not installed, causing an error even for base models. Since tsmodels() is
  # already called inside a future() worker, nested parallelism adds no value —
  # force sequential so fabletools runs single-threaded.
  .old_plan <- future::plan(future::sequential)
  on.exit(.mg2_restore_plan(.old_plan), add = TRUE)

  if (msg) cat("\n - tsmodels: Fitting models")
  .t_total <- Sys.time()
  .t_stage <- Sys.time()

  if (is.na(type)) {
    if (.use_prophet) {
      if (msg) cat(" (base + prophet)")
      primary_models <- tryCatch(
        train_data %>% fabletools::model(
          a  = fable::ARIMA(var),
          e  = fable::ETS(var),
          n  = fable::NNETAR(var),
          t  = fable::TSLM(var),
          p1 = fable.prophet::prophet(var ~ season("year", order = 1, type = "multiplicative")),
          p4 = fable.prophet::prophet(var ~ season("year", 4,     type = "multiplicative")),
          p8 = fable.prophet::prophet(var ~ season("year", 8,     type = "multiplicative")),
          .safely = TRUE
        ),
        error = function(e) {
          warning("tsmodels(): Prophet failed (", conditionMessage(e), "); using base models only", call. = FALSE)
          train_data %>% fabletools::model(
            a = fable::ARIMA(var), e = fable::ETS(var),
            n = fable::NNETAR(var), t = fable::TSLM(var), .safely = TRUE
          )
        }
      )
    } else {
      if (msg) cat(" (base only)")
      primary_models = train_data %>% fabletools::model(
        a = fable::ARIMA(var), e = fable::ETS(var),
        n = fable::NNETAR(var), t = fable::TSLM(var), .safely = TRUE
      )
    }
  }

  if (!is.na(type) && type == "transform") {
    if (.use_prophet) {
      if (msg) cat(" (base + prophet)")
      primary_models <- tryCatch(
        train_data %>% fabletools::model(
          a  = fable::ARIMA(log(var + 1)),
          e  = fable::ETS(log(var + 1)),
          n  = fable::NNETAR(log(var + 1)),
          t  = fable::TSLM(log(var) ~ trend() + season()),
          p1 = fable.prophet::prophet(log(var + 1) ~ season("year", 1, type = "multiplicative")),
          p4 = fable.prophet::prophet(log(var + 1) ~ season("year", 4, type = "multiplicative")),
          p8 = fable.prophet::prophet(log(var + 1) ~ season("year", 8, type = "multiplicative")),
          .safely = TRUE
        ),
        error = function(e) {
          warning("tsmodels(): Prophet failed (", conditionMessage(e), "); using base models only", call. = FALSE)
          train_data %>% fabletools::model(
            a = fable::ARIMA(log(var + 1)), e = fable::ETS(log(var + 1)),
            n = fable::NNETAR(log(var + 1)), t = fable::TSLM(log(var) ~ trend() + season()),
            .safely = TRUE
          )
        }
      )
    } else {
      if (msg) cat(" (base only)")
      primary_models = train_data %>% fabletools::model(
        a = fable::ARIMA(log(var + 1)), e = fable::ETS(log(var + 1)),
        n = fable::NNETAR(log(var + 1)), t = fable::TSLM(log(var) ~ trend() + season()),
        .safely = TRUE
      )
    }
  }

  # Covariate types need rlang::inject() to substitute the covariate symbol (!!covariate)
  # into the model formulas — unavoidable since the column name is runtime-determined.
  if (!is.na(type) && type == "covariate") {
    if (.set.seed) set.seed(1432)
    if (.use_prophet) {
      if (msg) cat(" (base + prophet)")
      primary_models <- tryCatch(
        rlang::inject(train_data %>% fabletools::model(
          a  = fable::ARIMA(var ~ log(!!covariate)),
          e  = fable::ETS(var),
          n  = fable::NNETAR(var ~ !!covariate),
          t  = fable::TSLM(var ~ trend() + season() + !!covariate),
          p1 = fable.prophet::prophet(var ~ !!covariate + season("year", 1, type = "multiplicative")),
          p4 = fable.prophet::prophet(var ~ !!covariate + season("year", 4, type = "multiplicative")),
          p8 = fable.prophet::prophet(var ~ !!covariate + season("year", 8, type = "multiplicative")),
          .safely = TRUE
        )),
        error = function(e) {
          warning("tsmodels(): Prophet failed (", conditionMessage(e), "); using base models only", call. = FALSE)
          rlang::inject(train_data %>% fabletools::model(
            a = fable::ARIMA(var ~ log(!!covariate)), e = fable::ETS(var),
            n = fable::NNETAR(var ~ !!covariate),
            t = fable::TSLM(var ~ trend() + season() + !!covariate), .safely = TRUE
          ))
        }
      )
    } else {
      if (msg) cat(" (base only)")
      primary_models = rlang::inject(train_data %>% fabletools::model(
        a = fable::ARIMA(var ~ log(!!covariate)), e = fable::ETS(var),
        n = fable::NNETAR(var ~ !!covariate),
        t = fable::TSLM(var ~ trend() + season() + !!covariate), .safely = TRUE
      ))
    }
  }

  if (!is.na(type) && type == "transform and covariate") {
    if (.use_prophet) {
      if (msg) cat(" (base + prophet)")
      primary_models <- tryCatch(
        rlang::inject(train_data %>% fabletools::model(
          a  = fable::ARIMA(log(var) ~ log(!!covariate)),
          e  = fable::ETS(log(var)),
          n  = fable::NNETAR(log(var) ~ !!covariate),
          t  = fable::TSLM(log(var) ~ trend() + season() + !!covariate),
          p1 = fable.prophet::prophet(log(var) ~ !!covariate + season("year", 1, type = "multiplicative")),
          p4 = fable.prophet::prophet(log(var) ~ !!covariate + season("year", 4, type = "multiplicative")),
          p8 = fable.prophet::prophet(log(var) ~ !!covariate + season("year", 8, type = "multiplicative")),
          .safely = TRUE
        )),
        error = function(e) {
          warning("tsmodels(): Prophet failed (", conditionMessage(e), "); using base models only", call. = FALSE)
          rlang::inject(train_data %>% fabletools::model(
            a = fable::ARIMA(log(var) ~ log(!!covariate)), e = fable::ETS(log(var)),
            n = fable::NNETAR(log(var) ~ !!covariate),
            t = fable::TSLM(log(var) ~ trend() + season() + !!covariate), .safely = TRUE
          ))
        }
      )
    } else {
      if (msg) cat(" (base only)")
      primary_models = rlang::inject(train_data %>% fabletools::model(
        a = fable::ARIMA(log(var) ~ log(!!covariate)), e = fable::ETS(log(var)),
        n = fable::NNETAR(log(var) ~ !!covariate),
        t = fable::TSLM(log(var) ~ trend() + season() + !!covariate), .safely = TRUE
      ))
    }
  }

  if (msg) cat("\n - tsmodels: All models fitted")

  # When base_models is specified, drop unneeded model columns before forecasting.
  # Fitting all models is fast; generating n_forecasts sample paths is the bottleneck.
  if (!is.null(base_models) && length(base_models) > 0) {
    keep <- intersect(names(primary_models), base_models)
    if (length(keep) > 0) {
      primary_models <- dplyr::select(primary_models, dplyr::all_of(keep))
    } else {
      if (msg) cat("\n - tsmodels: base_models had no matches; keeping all models")
    }
  }

  if (msg) cat("\n - tsmodels: Primary models finished.", .mg2_elapsed(.t_stage),
               "\n - tsmodels: Preparing primary forecasts...")

  .t_stage <- Sys.time()
  if (.set.seed) set.seed(1432)

  primary_forecasts = primary_models %>%
    fabletools::forecast(h = numberForecastMonths, times = n_forecasts) %>%
    dplyr::mutate(samples = fabletools::generate(var, n_forecasts))

  if (msg) cat("\n - tsmodels: Primary forecasts finished.", .mg2_elapsed(.t_stage))

  if (ensemble) {
    if (msg) cat("\n - tsmodels: Preparing ensemble forecasts...")
    .t_stage <- Sys.time()
    if (.set.seed) set.seed(1432)
    combo_forecasts = combination_forecasts(primary_forecasts)
    if (msg) cat("\n - tsmodels: Ensemble forecasts finished.", .mg2_elapsed(.t_stage))
    if (msg) cat("\n - tsmodels: Total time.", .mg2_elapsed(.t_total))
    return(combo_forecasts)
  } else {
    if (msg) cat("\n - tsmodels: Total time.", .mg2_elapsed(.t_total))
    return(primary_forecasts)
  }
}


#' Compute Out-of-Sample Accuracy (SWAPE) for Forecast Models
#'
#' Joins test-set forecasts with actuals and computes SWAPE per model per
#' replicate, then averages across replicates.
#'
#' @param test.forecasts Fable object of test-period forecasts.
#' @param test.data Tsibble of held-out actuals.
#' @param msg Logical. Print progress messages.
#' @param .var Character. Variable name (default `"total"`).
#' @param grouping Logical. Compute within grouping variable.
#' @param groups Character. Grouping variable name.
#'
#' @return A data frame of mean SWAPE per model.
#' @noRd
model_metrics = function(
  test.forecasts,
  test.data,
  msg      = TRUE,
  .var     = "total",
  grouping = FALSE,
  groups   = "agegrp"
) {
  if (msg) cat("\n * model_metrics: Calculating monthly SWAPE")

  var = rlang::sym(.var)

  if (grouping) {
    selectVars    = c(groups, .var, "Month")
    byVars        = c(groups, "Month")
    group_by_cols1 = c(groups, ".model", "Month")
    group_by_cols2 = c(groups, ".model", ".id")
    group_by_cols3 = c(groups, ".model")
  } else {
    selectVars    = c(.var, "Month")
    byVars        = "Month"
    group_by_cols1 = c(".model", "Month")
    group_by_cols2 = c(".model", ".id")
    group_by_cols3 = ".model"
  }

  accuracy_by_rep = test.forecasts %>%
    dplyr::inner_join(
      test.data %>%
        dplyr::select(dplyr::all_of(selectVars)) %>%
        dplyr::rename(actual = !!var),
      by = byVars
    ) %>%
    tidyr::unnest(samples) %>%
    dplyr::group_by(!!!rlang::syms(group_by_cols1)) %>%
    dplyr::mutate(.id = dplyr::row_number(), ae = abs(actual - samples)) %>%
    dplyr::group_by(!!!rlang::syms(group_by_cols2)) %>%
    dplyr::summarise(
      swape = 200 * sum(ae) / (sum(abs(actual)) + sum(abs(samples))),
      .groups = "drop"
    )

  accuracy_by_rep %>%
    dplyr::group_by(!!!rlang::syms(group_by_cols3)) %>%
    dplyr::summarise(swape = mean(swape), .groups = "drop")
}


#' Select the Best Forecast Model
#'
#' Given a table of model accuracy metrics, returns the model name(s) that
#' minimise the metric or that all series agree on (synchronize).
#'
#' @param modelMetrics Data frame output of `model_metrics()`.
#' @param type Character. `"synchronize"` picks the model with the lowest mean;
#'   `"optimize"` picks the single best row.
#' @param table Logical. Print a formatted flextable (default `FALSE`).
#' @param grouping Logical. Select within grouping variable.
#' @param groups Character. Grouping variable name.
#'
#' @return A data frame with a `.model` column.
#' @noRd
modelSelection = function(
  modelMetrics,
  type     = c("synchronize", "optimize"),
  table    = FALSE,
  grouping = FALSE,
  groups   = "agegrp"
) {
  type = match.arg(type)
  selectVars = if (grouping) c(groups, ".model") else ".model"

  ranks = best_fables_accuracy(fables_accuracy = modelMetrics, metric = "swape")

  if (type == "synchronize") {
    return(ranks %>% dplyr::filter(mean == min(ranks$mean, na.rm = TRUE)) %>%
             dplyr::select(dplyr::all_of(selectVars)))
  }
  if (type == "optimize") {
    return(ranks %>% dplyr::filter(dplyr::row_number() == 1) %>%
             dplyr::select(dplyr::all_of(selectVars)))
  }
}


#' Compute Weighted Percent Error Between Actual and Forecast Samples
#'
#' For each replicate, sums `actual - samples` over the post-intervention
#' period and divides by the sum of samples to give a WPE (%).
#'
#' @param actual Tsibble of actual post-intervention values.
#' @param predicted Fable object of post-intervention forecasts.
#' @param .var Character. Variable name (default `"total"`).
#' @param grouping Logical. Compute within grouping variable.
#' @param groups Character or character vector. Grouping variable name(s).
#' @param ... Ignored.
#'
#' @return A data frame with columns `.model`, `.rep`, `WPE`.
#' @noRd
forecast_diff = function(
  actual,
  predicted,
  .var     = "total",
  grouping = TRUE,
  groups   = "Intervention",
  ...
) {
  var = rlang::sym(.var)

  if (grouping) {
    group_cols = c(groups, ".model")
    selectVars = c(groups, "Month", .var)
  } else {
    group_cols = ".model"
    selectVars = c("Month", .var)
  }

  predicted %>%
    dplyr::inner_join(
      dplyr::bind_rows(actual) %>%
        dplyr::select(dplyr::all_of(selectVars)) %>%
        dplyr::rename(actual = !!var),
      by = setdiff(selectVars, .var)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Month")))) %>%
    tidyr::unnest(samples) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Month")))) %>%
    dplyr::mutate(.rep = dplyr::row_number(), e = actual - samples) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, ".rep")))) %>%
    dplyr::summarise(WPE = sum(e) / sum(samples) * 100, .groups = "drop")
}


#' Summarise Weighted Percent Error Across Replicates
#'
#' Computes mean, SD, median, and HDI of WPE from `forecast_diff()`.
#'
#' @param actual Tsibble of actual post-intervention values.
#' @param predicted Fable object of post-intervention forecasts.
#' @param .var Character. Variable name (default `"total"`).
#' @param grouping Logical. Compute within grouping variable.
#' @param groups Character. Grouping variable name.
#' @param ... Passed to `forecast_diff()`.
#'
#' @return A data frame with WPE summary statistics per model.
#' @export
wpe_summary = function(
  actual,
  predicted,
  .var     = "total",
  grouping = FALSE,
  groups   = "Intervention",
  ...
) {
  if (grouping) {
    grp_cols = c("Intervention", ".model")
  } else {
    grp_cols = ".model"
  }

  forecast_diff(actual, predicted, .var, grouping = grouping, groups = groups) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
    dplyr::summarise(
      n      = dplyr::n(),
      mean   = mean(WPE),
      sd     = stats::sd(WPE),
      median = stats::median(WPE),
      .groups = "drop"
    )
}


#' Histogram of Weighted Percent Error Across Forecast Replicates
#'
#' Plots the distribution of WPE values from `forecast_diff()`, with the
#' median bin highlighted in blue.
#'
#' @param actual Tsibble of actual post-intervention values.
#' @param predicted Fable object of post-intervention forecasts.
#' @param xlimits Numeric vector of length 2. x-axis limits (default `c(NA, NA)`).
#' @param ... Passed to `forecast_diff()` and [wpe_summary()].
#'
#' @return A ggplot object.
#' @noRd
diffHistogram = function(actual, predicted, xlimits = c(NA, NA), ...) {
  diffPredictedActual = forecast_diff(actual, predicted, ...)
  n_forecasts = max(diffPredictedActual$.rep)
  diffPredictedActual.summary = wpe_summary(actual, predicted, ...)

  d = diffPredictedActual %>%
    dplyr::inner_join(diffPredictedActual.summary, by = dplyr::join_by(.model)) %>%
    dplyr::mutate(.model = paste0("tsmodel = ", .model))

  ggplot2::ggplot() +
    ggplot2::geom_histogram(data = d, ggplot2::aes(WPE),
                            color = "white", binwidth = 1) +
    ggplot2::geom_histogram(
      data = d %>% dplyr::filter(round(WPE) == round(median)),
      ggplot2::aes(WPE), binwidth = 1, fill = "blue", alpha = 0.5
    ) +
    ggplot2::scale_x_continuous(limits = xlimits) +
    ggplot2::facet_grid(Intervention ~ .model, scales = "fixed") +
    ggplot2::labs(
      title    = "WPE Posterior Distribution",
      subtitle = paste0(
        "Each observation represents estimated difference from an individual forecast (n=",
        n_forecasts, ")"
      ),
      caption = "Blue bar represents median value",
      x = "Weighted Percent Change"
    )
}










# key.mpe = function( forecastData = tsForecast , actualData = td ,
#                       .period = 'Month' ,
#                       horizon = 12 ,
#                       agg_level = NULL ,
#                       levelNames = NULL ,
#                       split = 'None' ,
#                       .cat = FALSE ){
#
#         if ( .cat ) cat('\n* evaluation_widget key.mpe()')
#
#         predicted = forecastData %>%
#           rename( pred = .mean )
#
#         actual =  actualData %>%
#           rename( actual = total )
#
#         keyvars = key_vars( actual )
#         if ( .cat ) cat('\n - keyvars' , keyvars )
#
#         truth = predicted %>%
#            inner_join( actual , by = c( .period, keyvars  ) )
#
#         # print( 'truth'); #print( truth )
#
#         mid_point = round( as.integer( horizon ) /2  )
#
#         if (is.null( levelNames)) levelNames = getLevelNames( orgUnits = orgUnits )
#
#         if ( is.null( agg_level ) ) agg_level = levelNames[1]
#
#         e = truth %>%
#           group_by_key() %>%
#           index_by( 1 ) %>%
#           summarise(
#                 mpe = ifelse( mean( pred , na.rm = T ) > 0 ,
#                              mean( actual - pred  , na.rm = T ) /
#                              mean( pred , na.rm = T ) ,
#                              NA ) ,
#                 !! .period   := nth( !! rlang::sym( .period )  , mid_point ) ,
#                  actual = ifelse( mpe<=0 , max( actual, na.rm = TRUE ),
#                                  min( actual, na.rm = TRUE  )
#                                  #nth( actual , mid_point )
#                 ) ,
#                 pred = ifelse( mpe<=0 , max( pred, na.rm = TRUE ),
#                                  min( pred, na.rm = TRUE  )
#                                  #nth( actual , mid_point )
#                 ) ,
#                 just = ifelse( mpe >= 0 , 1, -1 )
#                 ) %>%
#         as_tibble()  %>%
#             mutate( !! agg_level :=
#                       as.character( !! rlang::sym( agg_level  ) ) )
#
#       if ( !split %in% 'None' ){
#            if ( .cat ) cat( '\n - key.mape grouping_var' , split )
#            e = e %>%
#              mutate(
#                grouping_var = as.character( !! rlang::sym( split ) )
#              )
#        } else {
#              e = e %>%
#                mutate(  grouping_var = 'Total' )
#            }
#
#         if ( .cat ) cat( "\n - mpe"  ); #glimpse(e )
#         return( e )
#       }

