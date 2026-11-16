# Moved from R/data_Functions.R on 2026-11-16: unreachable from the
# package API, the Shiny app, vignettes or tests (call-graph check).
# Not part of the package build (dev/ is .Rbuildignore'd).

#' Prepare trend data for modelling and forecasting
#' @param .d tsibble; the HTS-aggregated dataset
#' @param reportingSelectedOUs character vector; org units to include
#' @param period character; time index column name ("Month" or "Week")
#' @param startingMonth yearmonth; start of the analysis window
#' @param endingMonth yearmonth; end of the analysis window
#' @param selected.only logical; restrict to selected (consistently reporting) facilities
#' @param num_facilities integer; number of facilities in the dataset
#' @param num_datasets integer; number of datasets in the dataset
#' @param levelNames character vector; administrative level names
#' @param agg_level character; aggregation level name
#' @param split character; split variable ("None" or a column name)
#' @param covariates character vector; covariate column names to retain
#' @param remove.aggregate logical; remove aggregate (non-leaf) rows
#' @param scale logical; allow y-axis to be free (do not force zero baseline)
#' @param testing logical; save intermediate objects for debugging
#' @param .cat logical; print progress messages to the console
#' @noRd
trendData = function(
  .d = data.hts,
  reportingSelectedOUs = NULL,
  period = "Month",
  startingMonth = NULL,
  endingMonth = NULL,
  selected.only = TRUE,
  num_facilities = NULL,
  num_datasets = 1,
  levelNames = NULL,
  agg_level = NULL,
  split = 'None',
  covariates = NULL,
  remove.aggregate = TRUE,
  scale = FALSE,
  testing = FALSE,
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R: trendData(): ')
  }

  # Testing
  if (testing) {
    saveRDS(.d, "trendData.d.rds")
  }

  if (is.null(period)) {
    period = dataPeriod(.d)
  }

  .d. = .d

  # num_facilities = length( unique( .d.$orgUnit ))

  if (.cat) {
    cat('\n - period: ', period)
  }
  if (.cat) {
    cat('\n - num_facilities: ', num_facilities)
  }
  if (.cat) {
    cat('\n - selected.only: ', selected.only)
  }

  if (selected.only & num_facilities > 1) {
    if (.cat) {
      cat('\n - Show Selected (mostFrequeltylReporting) only')
    }

    .d. = .d %>% filter(Selected == 'Reporting Each Period')

    # make tibbles of covariate values to add back , if needed
    # Selected.d = .d. %>%
    #   as_tibble() %>%
    #   select_at( c( setdiff( key_vars( .d )  , "Selected"  ), "Month" , covariates ) ) %>%
    #   as_tsibble( key =  setdiff( key_vars( .d )  , "Selected"  ) , index = "Month" )
    #
    # notSelected.d = .d %>% anti_join( .d. , by =c( key_vars( .d ) , "Month" ) ) %>%
    #   as_tibble() %>%
    #   select_at( c( setdiff( key_vars( .d )  , "Selected"  ), "Month" , covariates ) ) %>%
    #   as_tsibble( key =  setdiff( key_vars( .d )  , "Selected"  ) , index = "Month" )

    if (!is.null(covariates)) {
      covariate.data = .d %>%
        as_tibble() %>%
        # filter(  Selected ==  'Reporting Each Period' ) %>%
        group_by_at(c(
          setdiff(key_vars(.d), c("Selected", "agegrp")),
          "Month"
        )) %>%
        summarise_at(covariates, median, na.rm = TRUE)
    }
  }

  if (period %in% 'Month' & !is.null(startingMonth) & !is.null(endingMonth)) {
    if (!"yearmonth" %in% class(startingMonth)) {
      startingMonth = as.yearmonth(startingMonth)
    }
    if (!"yearmonth" %in% class(endingMonth)) {
      endingMonth = as.yearmonth(endingMonth)
    }

    .d. = .d. %>%
      filter(
        Month >= startingMonth,
        Month <= endingMonth
      )
  }

  if (period %in% 'Week' & !is.null(startingMonth) & !is.null(endingMonth)) {
    .d. = .d. %>%
      filter(
        Week >= yearweek(startingMonth),
        Week <= yearweek(endingMonth)
      )
  }

  if (!is.null(agg_level)) {
    if (.cat) {
      cat("\n - input$agg_level:", agg_level)
    }

    sub_agg = levelNames[which(agg_level == levelNames) + 1]
    if (.cat) {
      cat("\n - sub agg level", sub_agg)
    }

    .d. = .d. %>%
      filter(
        !.is_empty(!!rlang::sym(agg_level)),
        !is.na(!!rlang::sym(agg_level))
        # next line is good for level 0
        # ,  ! is_aggregated(  !! rlang::sym( agg_level   ) )
      )
  }

  # if ( !.is_empty( sub_agg ) ){
  #   if ( .cat) cat( '\n - filtering by sub_agg' )
  #   .d = .d %>% filter(
  #         is_aggregated( !! rlang::sym( sub_agg  ) )
  #   )
  # }

  # preserve tsibble key and index,
  indexVar = index_var(.d.)
  keyVars = key_vars(.d.)

  .d. = .d. %>%
    mutate(
      grouping_var = 'Total'
    ) %>%
    # ensure tsibble before using fill_gaps
    as_tsibble(key = all_of(keyVars), index = indexVar) %>%
    fill_gaps(.full = TRUE)

  # once gaps are filled with NA, need to add back values of covariates
  if (!is.null(covariates)) {
    .d. = .d. %>%
      select_at(setdiff(names(.d.), covariates)) %>%
      left_join(
        covariate.data,
        by = c(setdiff(names(covariate.data), covariates))
      )
  }

  # glimpse( .d.. )
  # .d..  %>% filter( Cluster %in% "Chiyendausiku", Month == march ) %>% View()

  if (.cat) {
    cat('\n - .d in trendData')
  } # glimpse(.d)

  # Testing:
  # saveRDS(.d ,  "trendData.d.rds")

  # num_datasets = length( unique( .d$dataSet ))
  if (num_datasets > 1) {
    if (.cat) {
      cat("\n - num_datasets:", num_datasets)
    }

    .d. = .d. %>%
      filter(!is_aggregated(dataSet)) %>%
      mutate(
        dataSet = as.character(dataSet) %>%
          str_remove_all("<aggregated>"),
        grouping_var = dataSet
      )
  }

  if (num_facilities > 1) {
    if (.cat) {
      cat("\n - num_facilities:", num_facilities)
    }

    .d. = .d. %>%
      filter(!is_aggregated(Selected)) %>%
      mutate(
        Selected = as.character(Selected) %>%
          str_remove_all("<aggregated>")
      )

    if (.cat) cat('\n- Facilities:', unique(.d$Selected))
  }

  # if split, remove aggregate grouping
  if (.cat) {
    cat('\n - split:', split)
  }

  if (!'None' %in% split && length(split) == 1) {
    if (.cat) {
      cat('\n - input split:', split)
    }

    .d. = .d. %>%
      filter(!is_aggregated(!!rlang::sym(split))) %>%
      mutate(
        grouping_var = as.character(
          !!rlang::sym(split)
        )
      )

    if (remove.aggregate) {
      .d = .d %>%
        filter(!is_aggregated(!!rlang::sym(split)))
    }
  }

  # if ( .cat ) cat( '\n- nrow(.d)' , nrow(.d))

  # if ( !split() %in% 'None' & !input$filter_data %in% 'All' ){
  #     print( 'filter_data is not null' )
  #     .d = .d %>%
  #       filter( .data[[ split() ]] %in% input$filter_data )
  # }

  if (scale) {
    if (.cat) {
      cat('\n - scale:')
    }

    .d. = .d. %>%
      ungroup() %>%
      group_by(grouping_var) %>%
      mutate(
        total = scale(total) + 1
      )
  }

  # ensure tsibble before using fill_gaps
  if (.cat) {
    cat('\n - as_tsibble:')
  }

  .d. = .d. %>% as_tsibble(key = all_of(keyVars), index = indexVar)

  if (.cat) {
    cat('\n - end trend data():')
  } # print( glimpse( .d ) ); # print(.d)

  # Testing
  # saveRDS( .d , 'trendData.rds' )

  return(.d.)
}

#' Build model formula string for time-series fitting
#' @param model character; model type (e.g. "ARIMA", "ETS", "TSLM (trend+season)")
#' @param modelSpecs list; optional model specification overrides
#' @param transform logical; apply Box-Cox transformation in the formula
#' @param period character; time index column name ("Month" or "Week")
#' @param covariates character vector; covariate names to add as regressors
#' @param .cat logical; print progress messages to the console
#' @noRd
model_formula = function(
  model = "ARIMA",
  modelSpecs = NULL,
  transform = FALSE,
  period = "Month",
  covariates = NULL,
  .cat = FALSE
) {
  if (.cat) {
    cat("\n* data Functions.R model_formula:")
  }

  if (model %in% 'TSLM (trend+season)') {
    if (.cat) {
      cat("\n - model = TSLM")
    }

    formula.string = "total ~ trend() + season()"

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend() + season()'
    }
  }

  if (model %in% 'TSLM (trend)') {
    if (.cat) {
      cat("\n - model = TSLM (trend)")
    }

    formula.string = "total ~ trend()"

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend()'
    }
  }

  if (model %in% 'ARIMA') {
    if (.cat) {
      cat("\n - model = ARIMA")
    }

    # formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
    #                         ' pdq() ' )

    formula.string = ' total ~  pdq() '

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() '
    }

    if (period %in% "Month") {
      formula.string = paste0(formula.string, '+ PDQ( period = "1 year" )')
    }

    if (period %in% "Week") {
      formula.string = paste0(formula.string, '+ PDQ( period = 52 )')
    }

    if (!.is_empty(covariates) && any(nchar(covariates) > 0)) {
      formula.string =
        paste(
          formula.string,
          '+ xreg(',
          paste(covariates, collapse = " + "),
          ' ) '
        )
    }
  }

  if (model %in% 'BSTS') {
    if (.cat) {
      cat("\n - model = BSTS")
    }

    formula.string = 'total ~ season("year")'

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season()'
    }
  }

  if (model %in% 'ETS') {
    if (.cat) {
      cat("\n - model = ETS")
    }

    formula.string = 'total ~ error() + trend() + season()'

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ error() + trend() + season()'
    }
  }

  if (model %in% 'NNETAR') {
    if (.cat) {
      cat("\n - model = NNETAR")
    }

    formula.string = 'total'

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  )'
    }
  }

  if (model %in% 'SNAIVE') {
    if (.cat) {
      cat("\n - model = SNAIVE")
    }

    formula.string = 'total ~ lag("year")'

    if (transform) {
      formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ lag("year")'
    }
  }

  if (.cat) {
    cat("\n - formula:", formula.string)
  }
  return(formula.string)
}

#' Fit pre-intervention time-series models
#' @param trend.data tsibble; the prepared trend dataset
#' @param evaluation_month yearmonth; start of the evaluation (post-intervention) window
#' @param model character; model type to fit (e.g. "ARIMA", "ETS")
#' @param formula.string character; model formula string (NULL to auto-build)
#' @param period character; time index column name ("Month" or "Week")
#' @param .cat logical; print progress messages to the console
#' @param ... additional arguments passed to `model_formula()`
#' @noRd
tsPreModel = function(
  trend.data,
  evaluation_month,
  model = "ARIMA",
  formula.string = NULL,
  period = "Month",
  .cat = TRUE,
  ...
) {
  if (.cat) {
    cat('\n* data Function.R tsPreModel():', model)
  }

  if (period %in% "Month") {
    time_period = as.yearmonth(evaluation_month) - 12
  }
  if (period %in% "Week") {
    time_period = as.yearweek(evaluation_month) - 52
  }

  if (.cat) {
    cat("\n - evaluation begins:", as.character(evaluation_month))
  }
  if (.cat) {
    cat("\n - training data ends:", as.character(time_period))
  }

  # fit.data  = trend.data %>%
  #   filter_index( ~ as.character( time_period ) ,
  #                 .preserve = TRUE )

  # if ( .cat ) cat("\n - nrow(trendData()):" , nrow( trend.data )  )
  # if ( .cat ) cat("\n - nrow(fit.data:" , nrow( fit.data )  )

  fit.data = trend.data %>%
    filter_index(~ as.character(time_period), .preserve = TRUE)

  if (.cat) {
    cat("\n - fit.data ends:", as.character(max(fit.data$Month)))
  }

  # Testing:
  # saveRDS( trendData() , 'trendData.rds' )
  # saveRDS( fit.data , 'fit.data.rds' )

  if (is.null(formula.string)) {
    formula.string = model_formula(model, ...)
  }
  if (.cat) {
    cat("\n - formula.string:", formula.string)
  }

  if (grepl("~", formula.string, fixed = TRUE)) {
    formula.string = as.formula(formula.string)
  }

  if (model %in% 'TSLM (trend)') {
    fit = fit.data %>% model(l = TSLM(formula.string))

    if (.cat) {
      cat('\n - end tsPreModel() TSLM(trend):')
    }
    return(fit)
  }

  if (model %in% 'TSLM (trend+season)') {
    fit = fit.data %>% model(l = TSLM(formula.string))

    if (.cat) {
      cat('\n - end tsPreModel() TSLM(trend + season):')
    }
    return(fit)
  }

  if (model %in% 'ARIMA') {
    # if ( .cat ) cat( '\n - fit.data names:' , paste( names(fit.data), collapse = ', ') )

    fit = fit.data %>%
      model(
        arima = ARIMA(formula.string)
      )
    # if ( input$reconcile ) fit = fit %>%
    #       reconcile(
    #         mint = min_trace(a, method = "mint_shrink")
    #         )

    if (.cat) {
      cat('\n - end tsPreModel(): arima fit')
    }
    # glimpse( fit )
    # testing model fit for forecasts

    # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

    return(fit)
  }

  if (model %in% 'NNETAR') {
    fit = fit.data %>%
      model(
        nnetar = NNETAR(total)
      )

    if (transform) {
      fit = fit.data %>%
        model(nnetar = NNETAR(fabletools::box_cox(total, lambda = .5)))
    }

    if (.cat) {
      cat('\n - end tsModel():')
    }
    return(fit)
  }

  if (model %in% 'BSTS') {
    fit = fit.data %>%
      model(
        # b = BSTS( model_formula() )
        bsts = BSTS(formula.string)
      )

    if (.cat) {
      cat('\n - end tsPreModel() BSTS:')
    }
    return(fit)
  }

  if (model %in% 'ETS') {
    fit = fit.data %>% model(ets = ETS(!!formula.string))

    # if ( input$reconcile ) fit = fit %>%
    #       reconcile(
    #         mint = min_trace(a, method = "mint_shrink")
    #         )
    if (.cat) {
      cat('\n - end tsModel():')
    }
    return(fit)
  }

  if (model %in% 'SNAIVE') {
    fit = fit.data %>% model(ets = SNAIVE(formula.string))

    if (.cat) {
      cat('\n - end tsModel():')
    }
    return(fit)
  }

  if (model %in% 'Prophet') {
    if (transform) {
      fit = fit.data %>%
        model(
          prophet = prophet(
            fabletools::box_cox(total, lambda = .5) ~
              growth(
                type = 'linear',
                changepoint_range = 1,
                changepoint_prior_scale = 1,
                # capacity = 1e5 ,
                # floor = 0
              ) +
                season(period = 12, order = 4, type = 'multiplicative'),
            seed = TRUE
          )
        )
    } else {
      fit = fit.data %>%
        model(
          prophet = prophet(
            total ~
              growth(
                type = 'linear',
                changepoint_range = 1,
                changepoint_prior_scale = 1,
                # capacity = 1e5 ,
                # floor = 0
              ) +
                season(period = 12, order = 4, type = 'multiplicative'),
            seed = TRUE
          )
        )
    }

    if (.cat) {
      cat('\n - end tsPreModel() Prophet:')
    }
    return(fit)
  }
}

#' Generate pre-intervention forecasts from fitted models
#' @param trend.data tsibble; the full trend dataset (used to build new_data)
#' @param preModel mable; fitted model object from `tsPreModel()`
#' @param horizon integer; number of periods to forecast
#' @param evaluation_month yearmonth; centre of the evaluation window
#' @param period character; time index column name ("Month" or "Week")
#' @param covariates character vector; covariate column names required by the model
#' @param split character; split variable name (or NULL)
#' @param agg_level character; aggregation level name
#' @param prob logical; return probabilistic (distribution) forecasts
#' @param pi_levels numeric; prediction interval level(s) (e.g. 0.89)
#' @param .cat logical; print progress messages to the console
#' @noRd
tsPreForecast = function(
  trend.data,
  preModel,
  horizon = 12,
  evaluation_month = NULL,
  period = "Month",
  covariates = NULL,
  split = NULL,
  agg_level = NULL,
  prob = FALSE,
  pi_levels = .89,
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R tsPreForecast')
  }

  if (period %in% "Month") {
    time_start = as.yearmonth(evaluation_month) - horizon + 1
    time_end = as.yearmonth(evaluation_month)
  }

  if (period %in% "Week") {
    time_period = yearweek(evaluation_month)
    horizon = 52
  }

  if (.cat) {
    cat(
      "\n - evaluation period:",
      as.character(time_start),
      "-",
      as.character(time_end)
    )
  }

  test.data = trend.data %>%
    select(-total) %>%
    filter(Month >= time_start, Month <= time_end)

  # filter_index(  format(time_start,  format = "%Y-%m")  ~  format(time_end,  format = "%Y-%m")  ,
  #             .preserve = TRUE )

  if (.cat) {
    cat(
      "\n - test.data period:",
      as.character(min(test.data$Month)),
      "-",
      as.character(max(test.data$Month))
    )
  }

  # fcast= getForecast( test_data = test.data , model = tsPreModel() ,
  #          bootstrap = FALSE , Reps = 1000 )

  # if ( period() %in% 'Month' ) fcast = tsPreModel() %>% forecast( h = 12 , level = pi_levels() )
  # if ( period() %in% 'Week' ) fcast = tsPreModel() %>% forecast( h = 52  )

  if (prob) {
    fcast = preModel %>%
      fabletools::forecast(new_data = test.data, level = pi_levels)
  } else {
    fcast = preModel %>% fabletools::forecast(new_data = test.data)
  }

  # preserve tsibble key and index,
  indexVar = index_var(fcast)
  keyVars = key_vars(fcast)

  if (!is.null(agg_level)) {
    if (.cat) {
      cat('\n - tsPreForecast done.  Adding agg_level')
    }

    fcast = fcast %>%
      mutate(!!agg_level := as.character(!!rlang::sym(agg_level)))
  }

  if (.cat) {
    cat('\n - tsPreForecast grouping_var', split)
  }
  if (any(!split %in% 'None') && length(split) == 1) {
    fcast = fcast %>%
      mutate(
        grouping_var = as.character(!!rlang::sym(split))
      )
  } else {
    fcast = fcast %>%
      mutate(grouping_var = 'Total')
  }
  if (.cat) {
    cat('\n - tsPreForecast grouping_var values:', unique(fcast$grouping_var))
  }

  # Ensure result is tsibble
  # fcast = fcast %>%
  #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
  #        fill_gaps( .full = TRUE  )

  if (.cat) {
    cat('\n - tsPreForecast done.')
  }
  # print( names( fcast ) )

  # Testing:
  # saveRDS( fcast , 'tsPreForecast.rds' )

  return(fcast)
}

#' Compute key MAPE values for forecast accuracy summary
#' @param preForecast fable or data frame; forecast output with a `.mean` column
#' @param trend.data tsibble; actual observed values
#' @param period character; time index column name ("Month" or "Week")
#' @param split character; split variable for grouping results ("None" or a column name)
#' @param agg_level character; aggregation level name
#' @param horizon integer; number of forecast periods
#' @param .cat logical; print progress messages to the console
#' @noRd
key.mape = function(
  preForecast,
  trend.data,
  period = "Month",
  split = 'None',
  agg_level = NULL,
  horizon = 12,
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R  key.mape()')
  }

  predicted = preForecast %>%
    rename(pred = .mean)

  actual = trend.data %>%
    rename(actual = total)

  keyvars = key_vars(actual)
  if (.cat) {
    cat('\n - keyvars', keyvars)
  }

  truth = predicted %>%
    # select( -dataCol, -grouping_var ) %>%
    inner_join(actual, by = c(period, keyvars))

  if (.cat) {
    cat('\n - truth')
  } #print( truth )

  mid_point = round(as.integer(horizon) / 2)

  # summarise
  if ('orgUnit' %in% keyvars) {
    group_no_orgUnit = setdiff(keyvars, 'orgUnit')

    truth = truth %>% as_tibble() %>% group_by_at(group_no_orgUnit)
  } else {
    truth = truth %>%
      group_by_key() %>%
      index_by(1)
  }

  e = truth %>%
    summarise(
      mape = ifelse(
        mean(pred, na.rm = T) > 0,
        mean(abs(actual - pred), na.rm = T) /
          mean(pred, na.rm = T),
        NA
      ),
      !!rlang::sym(period) := nth(!!rlang::sym(period), mid_point),
      actual = ifelse(
        mape >= 0,
        max(actual, na.rm = TRUE),
        min(actual, na.rm = TRUE)
        #nth( actual , mid_point )
      ),
      just = ifelse(mape >= 0, 2, -2)
    ) %>%
    as_tibble()

  if (!is.null(agg_level)) {
    e = e %>%
      mutate(!!agg_level := as.character(!!rlang::sym(agg_level)))
  }

  if (any(!split %in% 'None') && length(split) == 1) {
    if (.cat) {
      cat('\n - key.mape grouping_var', split)
    }
    e = e %>%
      mutate(
        grouping_var = as.character(!!rlang::sym(split))
      )
  } else {
    e = e %>%
      mutate(grouping_var = 'Total')
  }

  # print( "end key.mape"); #glimpse(e )
  return(e)
}

#' Fit pre-intervention impact models
#' @noRd
pre_impact_fit = function(
  ml.data = ml.data,
  startingMonth = "Jan 2015",
  endingMonth = NULL,
  selected.only = FALSE,
  missing_reports = 0,
  split = NULL,
  evaluation_month = "Dec 2018",
  error = "seasonal3",
  hts = TRUE,
  aggregate = TRUE,
  levelNames = NULL,
  agg_level = NULL, # levelNames[2] # if NULL, while ignore admin levels
  pi_levels = 89, # for plot trends...
  model = "ARIMA",
  modelSpecs = NULL,
  transform = FALSE,
  covariates = NULL,
  simulate = FALSE,
  bootstrap = FALSE,
  times = 100,
  remove.aggregate = TRUE,
  .cat = TRUE
) {
  cat("\n* data Functions.R pre_impact_fit:")
  period = dataPeriod(ml.data)

  # prepare data with error and selected facilities
  d = ml.data %>%
    error_factor %>%
    cleanedData(., error = error, .cat = .cat) %>%

    selectedData(
      startingMonth = startingMonth,
      endingMonth = endingMonth,
      missing_reports = missing_reports,
      .cat = .cat
    ) %>%
    as_tibble()

  # d %>% group_by( Include , RTSS,  Selected ) %>%
  #   summarise( facilities = n_distinct( orgUnit ) ,
  #              errorFlags = sum( value & is.na( dataCol ) ) ,
  #              `%` = percent( errorFlags / sum( value ) , 0.1 )
  #   )

  group_by_cols = groupByCols(
    period = dataPeriod(d),
    hts = hts,
    levelNames = levelNames,
    split = split,
    .cat = .cat
  )

  num_facilities = mostFrequentReportingOUs(
    ml.data,
    startingMonth = startingMonth,
    endingMonth = endingMonth,
    missing_reports = missing_reports,
    .cat = .cat
  ) %>%
    length()

  dataSets = unique(mvip.ml$dataSet)
  num_datasets = length(dataSets)

  data.total = dataTotal(
    data = d,
    group_by_cols = group_by_cols,
    dataSets = dataSets,
    covariates = covariates,
    .cat = .cat
  )

  if (hts) {
    hts_formula = htsFormula(
      hts = hts,
      levelNames = levelNames,
      agg_level = agg_level,
      all.levels = FALSE,
      num_facilities = num_facilities,
      num_datasets = num_datasets,
      split = split,
      .cat = .cat
    )

    data.agg.ts = htsData(
      data = data.total,
      hts = hts,
      hts_formula = hts_formula,
      covariates = covariates,
      group_by_cols = group_by_cols,
      .cat = .cat
    )
  } else {
    data.agg.ts = aggData(
      data.total = data.total,
      covariates = covariates,
      group_by_cols = group_by_cols,
      .cat = .cat
    )
  }

  trend.data = trendData(
    .d = data.agg.ts,
    levelNames = levelNames,
    startingMonth = startingMonth,
    endingMonth = endingMonth,
    selected.only = selected.only,
    num_facilities = num_facilities,
    num_datasets = num_datasets,
    split = split,
    agg_level = agg_level,
    remove.aggregate = remove.aggregate,
    .cat = .cat
  )

  formula.string = model_formula(
    model = model,
    modelSpecs = modelSpecs,
    transform = transform,
    period = period,
    covariates = covariates,
    .cat = .cat
  )

  .old_opts <- options(future.rng.onMisuse = "ignore")
  on.exit(options(.old_opts), add = TRUE)

  # cat( "\n *** saving pre_model_diagnostics.rda")
  # save( data.total, data.hts, group_by_cols, dataSets , covariates,
  #       trend.data, evaluation_month, model, formula.string, transform,
  #       file =  "pre_model_diagnostics.rda")
  # cat( "\n *** -done")

  .t0 <- Sys.time()
  preModel = tsPreModel(
    trend.data,
    evaluation_month = evaluation_month,
    model = model,
    formula.string = formula.string,
    transform = transform,
    .cat = .cat
  )

  cat("\n\n")
  if (.cat) cat(.mg2_elapsed(.t0), "\n")

  # testing
  # cat("\n - saving preModel")
  # saveRDS( preModel , "preModel.rds" )

  # data.orgUnit = data.tota %>% as_tsibble( index = "Month", key = c('orgUnit') )
  #
  # preModel.orgUnit = tsPreModel( dt ,
  #                                evaluation_month = evaluation_month ,
  #                        model = model  , transform = TRUE )

  # Remove any NULL models
  null_models = grepl(
    fixed("NULL"),
    preModel %>% select(ncol(preModel)) %>% pull
  )

  if (all(null_models)) {
    m = tibble(
      mape = NA %>% as.numeric(),
      evaluation_month = evaluation_month,
      startingMonth = startingMonth,
      missing_reports = missing_reports,
      # num_facilities = num_facilities ,
      agg_level = agg_level,
      error = error,
      model = model,
      transform = transform,
      covariates = paste(covariates, collapse = " + ")
    )
    return(m)
  }

  preModel = preModel[!null_models, ]

  preForecast = tsPreForecast(
    trend.data,
    preModel,
    horizon = 12,
    evaluation_month = evaluation_month,
    period = "Month",
    covariates = covariates,
    split = split,
    agg_level = agg_level,
    .cat = .cat
  )

  m = key.mape(
    preForecast,
    trend.data,
    split = split,
    agg_level = agg_level,
    .cat = .cat
  ) %>%
    # select( - .model, - `1` , - just , - grouping_var , -Month, -actual   ) %>%
    mutate(
      evaluation_month = evaluation_month,
      startingMonth = startingMonth,
      missing_reports = missing_reports,
      # num_facilities = num_facilities ,
      agg_level = agg_level,
      error = error,
      model = model,
      transform = transform,
      covariates = paste(covariates, collapse = " + ")
    )
}

#' Fit time-series models across replicates
#' @param trend.data tsibble; the prepared trend dataset
#' @param evaluation_month yearmonth; end of the training window
#' @param model character; model type to fit (e.g. "ARIMA", "ETS")
#' @param formula.string character; model formula string (NULL to auto-build)
#' @param period character; time index column name ("Month" or "Week")
#' @param .cat logical; print progress messages to the console
#' @param ... additional arguments passed to `model_formula()`
#' @noRd
tsModel = function(
  trend.data,
  evaluation_month,
  model = "ARIMA",
  formula.string = NULL,
  period = "Month",
  .cat = TRUE,
  ...
) {
  if (.cat) {
    cat('\n* data Function.R tsModel():', model)
  }

  if (period %in% "Month") {
    time_period = as.yearmonth(evaluation_month)
  }
  if (period %in% "Week") {
    time_period = yearweek(evaluation_month)
  }

  if (.cat) {
    cat("\n - training data ends:", as.character(time_period))
  }
  if (.cat) {
    cat("\n - training data keys:", key_vars(trend.data))
  }

  fit.data = trend.data %>%
    filter_index(~ as.character(time_period), .preserve = TRUE)

  # if ( .cat ) cat("\n - fit.data ends:" , as.character( max( fit.data$Month ) ) )

  # Testing:
  # saveRDS( trendData() , 'trendData.rds' )
  # saveRDS( fit.data , 'fit.data.rds' )

  if (is.null(formula.string)) {
    formula.string = model_formula(model, ...)
  }
  if (.cat) {
    cat("\n - formula.string:", formula.string)
  }

  if (grepl("~", formula.string, fixed = TRUE)) {
    formula.string = as.formula(formula.string)
  }

  if (model %in% 'TSLM (trend)') {
    fit = fit.data %>% model(l = TSLM(formula.string))

    if (.cat) {
      cat('\n - end tsPreModel() TSLM(trend):')
    }
    return(fit)
  }

  if (model %in% 'TSLM (trend+season)') {
    fit = fit.data %>% model(l = TSLM(formula.string))

    if (.cat) {
      cat('\n - end tsPreModel() TSLM(trend + season):')
    }
    return(fit)
  }

  if (model %in% 'ARIMA') {
    # if ( .cat ) cat( '\n - fit.data names:' , paste( names(fit.data), collapse = ', ') )

    fit = fit.data %>%
      model(
        arima = ARIMA(formula.string)
      )
    # if ( input$reconcile ) fit = fit %>%
    #       reconcile(
    #         mint = min_trace(a, method = "mint_shrink")
    #         )

    if (.cat) {
      cat('\n - end tsPreModel(): arima fit')
    }
    # glimpse( fit )
    # testing model fit for forecasts

    # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

    return(fit)
  }

  if (model %in% 'NNETAR') {
    fit = fit.data %>%
      model(
        nnetar = NNETAR(total)
      )

    if (transform) {
      fit = fit.data %>%
        model(nnetar = NNETAR(fabletools::box_cox(total, lambda = .5)))
    }

    if (.cat) {
      cat('\n - end tsModel():')
    }
    return(fit)
  }

  if (model %in% 'BSTS') {
    fit = fit.data %>%
      model(
        # b = BSTS( model_formula() )
        bsts = BSTS(formula.string)
      )

    if (.cat) {
      cat('\n - end tsPreModel() BSTS:')
    }
    return(fit)
  }

  if (model %in% 'ETS') {
    fit = fit.data %>% model(ets = ETS(!!formula.string))

    # if ( input$reconcile ) fit = fit %>%
    #       reconcile(
    #         mint = min_trace(a, method = "mint_shrink")
    #         )
    if (.cat) {
      cat('\n - end tsModel():')
    }
    return(fit)
  }

  if (model %in% 'SNAIVE') {
    fit = fit.data %>% model(ets = SNAIVE(formula.string))

    if (.cat) {
      cat('\n - end tsModel():')
    }
    return(fit)
  }

  if (model %in% 'Prophet') {
    if (input$transform) {
      fit = fit.data %>%
        model(
          prophet = prophet(
            fabletools::box_cox(total, lambda = .5) ~
              growth(
                type = 'linear',
                changepoint_range = 1,
                changepoint_prior_scale = 1,
                # capacity = 1e5 ,
                # floor = 0
              ) +
                season(period = 12, order = 4, type = 'multiplicative'),
            seed = TRUE
          )
        )
    } else {
      fit = fit.data %>%
        model(
          prophet = prophet(
            total ~
              growth(
                type = 'linear',
                changepoint_range = 1,
                changepoint_prior_scale = 1,
                # capacity = 1e5 ,
                # floor = 0
              ) +
                season(period = 12, order = 4, type = 'multiplicative'),
            seed = TRUE
          )
        )
    }

    if (.cat) {
      cat('\n - end tsPreModel() Prophet:')
    }
    return(fit)
  }
}

#' Generate time-series forecasts from fitted models
#' @param trend.data tsibble; the full trend dataset (used to build new_data)
#' @param Model mable; fitted model object from `tsModel()`
#' @param horizon integer; number of periods to forecast
#' @param evaluation_month yearmonth; start of the forecast window
#' @param period character; time index column name ("Month" or "Week")
#' @param covariates character vector; covariate column names required by the model
#' @param split character; split variable name (or NULL)
#' @param agg_level character; aggregation level name
#' @param simulate logical; use simulation-based forecasting
#' @param bootstrap logical; use bootstrap residuals for prediction intervals
#' @param times integer; number of simulation/bootstrap replicates
#' @param pi_levels numeric; prediction interval level(s) (e.g. 0.89)
#' @param .cat logical; print progress messages to the console
#' @noRd
tsForecast = function(
  trend.data,
  Model,
  horizon = 12,
  evaluation_month = NULL,
  period = "Month",
  covariates = NULL,
  split = NULL,
  agg_level = NULL,
  simulate = FALSE,
  bootstrap = FALSE,
  times = 100,
  pi_levels = .89,
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R tsForecast')
  }

  if (period %in% "Month") {
    time_start = as.yearmonth(evaluation_month)
    time_end = time_start + horizon - 1
  }

  if (period %in% "Week") {
    time_period = yearweek(evaluation_month)
    horizon = 52
  }

  if (.cat) {
    cat(
      "\n - evaluation period:",
      as.character(time_start),
      "-",
      as.character(time_end),
      "(",
      time_end - time_start,
      "months)"
    )
  }

  test.data = trend.data %>%
    select(-total) %>%
    filter(Month >= (time_start), Month <= time_end)

  # filter_index(  format(time_start,  format = "%Y-%m")  ~  format(time_end,  format = "%Y-%m")  ,
  #             .preserve = TRUE )

  if (.cat) {
    cat(
      "\n - test.data period:",
      as.character(min(test.data$Month)),
      "-",
      as.character(max(test.data$Month))
    )
  }

  # fcast= getForecast( test_data = test.data , model = tsPreModel() ,
  #          bootstrap = FALSE , Reps = 1000 )

  # if ( period() %in% 'Month' ) fcast = tsPreModel() %>% forecast( h = 12 , level = pi_levels() )
  # if ( period() %in% 'Week' ) fcast = tsPreModel() %>% forecast( h = 52  )

  fcast = Model %>%
    fabletools::forecast(
      new_data = test.data,
      simulate = simulate,
      bootstrap = bootstrap,
      times = times
    )

  # preserve tsibble key and index,
  indexVar = index_var(fcast)
  keyVars = key_vars(fcast)

  if (!is.null(agg_level)) {
    if (.cat) {
      cat('\n - Adding agg_level')
    }

    fcast = fcast %>%
      mutate(!!agg_level := as.character(!!rlang::sym(agg_level)))
  }

  if (.cat) {
    cat('\n - grouping_var', split)
  }
  if (any(!split %in% 'None') && length(split) == 1) {
    fcast = fcast %>%
      mutate(
        grouping_var = as.character(!!rlang::sym(split))
      )
  } else {
    fcast = fcast %>%
      mutate(grouping_var = 'Total')
  }
  if (.cat) {
    cat('\n - tsPreForecast grouping_var values:', unique(fcast$grouping_var))
  }

  # Ensure result is tsibble
  # fcast = fcast %>%
  #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
  #        fill_gaps( .full = TRUE  )

  if (.cat) {
    cat('\n - tsPreForecast done.')
  }
  # print( names( fcast ) )

  # Testing:
  # saveRDS( fcast , 'tsPreForecast.rds' )

  return(fcast)
}

#' Fit post-intervention impact models
#' @noRd
impact_fit = function(
  # ml.data = ml.data ,
  # startingMonth = "Jan 2015" ,
  # horizon = 12 ,
  # endingMonth = NULL ,
  # selected.only = FALSE ,
  # missing_reports = 0 ,
  # split = NULL ,
  # evaluation_month = "Dec 2018" ,
  # error = "seasonal3" ,
  # hts = TRUE ,
  # aggregate = TRUE ,
  # levelNames = NULL ,
  # agg_level =  NULL , # levelNames[2] # if NULL, while ignore admin levels
  # prob = FALSE ,
  # pi_levels = 89 , # for plot trends...
  # model = "ARIMA" ,
  # simulate = FALSE ,
  # bootstrap = FALSE ,
  # times = 100 ,
  # distribution = FALSE ,
  # modelSpecs = NULL ,
  # transform = FALSE ,
  # covariates = NULL ,
  # remove.aggregate = TRUE  ,
  # .cat = TRUE ,
  ...
) {
  cat("\n* data Functions.R impact_fit:")
  period = dataPeriod(ml.data)

  # prepare data with error and selected facilities
  d = ml.data %>%
    error_factor %>%
    cleanedData(., error = error, .cat = .cat) %>%

    selectedData(
      startingMonth = startingMonth,
      missing_reports = missing_reports,
      endingMonth = endingMonth,
      .cat = .cat
    ) %>%
    as_tibble()

  # d %>% group_by( Include , RTSS,  Selected ) %>%
  #   summarise( facilities = n_distinct( orgUnit ) ,
  #              errorFlags = sum( value & is.na( dataCol ) ) ,
  #              `%` = percent( errorFlags / sum( value ) , 0.1 )
  #   )

  group_by_cols = groupByCols(
    period = dataPeriod(d),
    hts = hts,
    levelNames = levelNames,
    split = split,
    .cat = .cat
  )

  num_facilities = mostFrequentReportingOUs(
    d,
    startingMonth = startingMonth,
    missing_reports = missing_reports,
    .cat = .cat
  ) %>%
    length()

  dataSets = unique(mvip.ml$dataSet)
  num_datasets = length(dataSets)

  data.total = dataTotal(
    data = d,
    group_by_cols = group_by_cols,
    dataSets = dataSets,
    covariates = covariates,
    .cat = .cat
  )

  if (hts) {
    hts_formula = htsFormula(
      hts = hts,
      levelNames = levelNames,
      agg_level = agg_level,
      all.levels = FALSE,
      num_facilities = num_facilities,
      num_datasets = num_datasets,
      split = split,
      .cat = .cat
    )

    data.agg.ts = htsData(
      data = data.total,
      hts = hts,
      hts_formula = hts_formula,
      covariates = covariates,
      group_by_cols = group_by_cols,
      .cat = .cat
    )
  } else {
    data.agg.ts = aggData(
      data.total = data.total,
      covariates = covariates,
      group_by_cols = group_by_cols,
      .cat = .cat
    )
  }

  trend.data = trendData(
    .d = data.agg.ts,
    levelNames = levelNames,
    startingMonth = startingMonth,
    endingMonth = endingMonth,
    selected.only = selected.only,
    num_facilities = num_facilities,
    num_datasets = num_datasets,
    split = split,
    agg_level = agg_level,
    remove.aggregate = remove.aggregate,
    .cat = .cat
  )

  formula.string = model_formula(
    model = model,
    modelSpecs = modelSpecs,
    transform = transform,
    period = period,
    covariates = covariates,
    .cat = .cat
  )

  .old_opts <- options(future.rng.onMisuse = "ignore")
  on.exit(options(.old_opts), add = TRUE)

  # cat( "\n *** saving pre_model_diagnostics.rda")
  # save( data.total, data.hts, group_by_cols, dataSets , covariates,
  #       trend.data, evaluation_month, model, formula.string, transform,
  #       file =  "pre_model_diagnostics.rda")
  # cat( "\n *** -done")

  .t0 <- Sys.time()
  Model = tsModel(
    trend.data,
    evaluation_month = evaluation_month,
    model = model,
    formula.string = formula.string,
    transform = transform,
    .cat = .cat
  )

  cat("\n\n")
  if (.cat) cat(.mg2_elapsed(.t0), "\n")

  # data.orgUnit = data.tota %>% as_tsibble( index = "Month", key = c('orgUnit') )
  #
  # preModel.orgUnit = tsPreModel( dt ,
  #                                evaluation_month = evaluation_month ,
  #                        model = model  , transform = TRUE )

  # Remove any NULL models
  null_models = grepl(fixed("NULL"), Model %>% select(ncol(Model)) %>% pull)
  if (.cat) {
    cat(
      "\n - Of",
      length(null_models),
      "models, there were",
      sum(null_models),
      "null models"
    )
  }

  Model = Model[!null_models, ]

  if (.cat) {
    cat("\n - evaluation_month", evaluation_month)
  }
  if (.cat) {
    cat("\n - horizon", horizon)
  }

  Forecast = tsForecast(
    trend.data,
    Model,
    horizon = horizon,
    evaluation_month = evaluation_month,
    period = "Month",
    covariates = covariates,
    split = split,
    agg_level = agg_level,
    simulate = simulate,
    bootstrap = bootstrap,
    times = times,
    .cat = .cat
  )

  if (distribution) {
    cat("\n - key.mpe.distribution")
    x = key.mpe.distribution(Forecast, trend.data, times = times, .cat = .cat)
    i = x$e.dist
  } else {
    i = key.mpe(
      Forecast,
      trend.data,
      .split = split,
      agg_level = agg_level,
      .cat = .cat
    )
    x = NULL
  }

  i = i %>%
    # select( - .model, - `1` , - just , - grouping_var , -Month, -actual   ) %>%
    mutate(
      evaluation_month = evaluation_month,
      startingMonth = startingMonth,
      missing_reports = missing_reports,
      # num_facilities = num_facilities ,
      agg_level = agg_level,
      error = error,
      model = model,
      transform = transform,
      covariates = paste(covariates, collapse = " + ")
    )

  return(list(i = i, Forecast = Forecast, x = x))
}

#' Extract forecast values for a specific model and replicate
#' @param forecastData tsibble; prepared trend data to fit and forecast from
#' @param model character; model type to fit (e.g. "ARIMA")
#' @param model.string character; model formula string (NULL to auto-build)
#' @param transform logical; apply Box-Cox transformation
#' @param lambda numeric; Box-Cox lambda parameter
#' @param covariates character vector; covariate column names
#' @param horizon integer; number of periods to forecast
#' @param bootstrap logical; use bootstrap residuals for prediction intervals
#' @param Reps integer; number of bootstrap replicates
#' @param future.seed logical; set a reproducible seed for parallel futures
#' @param split character; split variable ("None" or a column name)
#' @param agg_level character; aggregation level name
#' @param agg_method character; aggregation method ("None" or method name)
#' @param .period character; time index column name ("Month" or "Week")
#' @param eval_date yearmonth; evaluation start date
#' @param .cat logical; print progress messages to the console
#' @noRd
getForecast = function(
  forecastData,
  model = NULL,
  model.string = NULL,
  transform = TRUE,
  lambda = .5,
  covariates = NULL,
  horizon = 12,
  bootstrap = FALSE,
  Reps = 1000,
  future.seed = TRUE,
  split = 'None',
  agg_level = NULL,
  agg_method = "None",
  .period = "Month",
  eval_date = as.yearmonth('Jan 2021'),
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* tsForecast()')
  }

  # if ( bootstrap ){
  #   # remove null models because throws error...
  #   .model = model[ which( !is_null_model( model$arima ) ), ]
  #
  #   fcast = .model %>%
  #     forecast( new_data = test_data ,
  #               # simulate = TRUE ,
  #               bootstrap = TRUE,
  #               times = Reps  )
  #
  # } else {
  #   fcast = .model %>%
  #     forecast( new_data = test_data  )
  # }

  if (.period %in% "Month") {
    time_period = as.yearmonth(eval_date, "%Y %b")
  } # - month(1)
  if (.period %in% "Week") {
    time_period = yearweek(eval_date)
  }

  fit.data = forecastData %>%
    filter_index(~ as.character(time_period), .preserve = TRUE)

  # test.data  = forecastData %>%
  #   filter_index( as.character( time_period ) ~  ,
  #                 .preserve = TRUE )

  if (is.null(model.string)) {
    model.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() + PDQ() '
  }
  # model.string = 'total  ~  pdq() + PDQ()'

  if (transform) {
    model.string = paste(
      'fabletools::box_cox( total , lambda =',
      lambda,
      ') ~  pdq() '
    )
  } else {
    model.string = 'total ~  pdq() '
  }

  if (.period %in% "Month") {
    model.string = paste0(model.string, '+ PDQ( period = "1 year" )')
  }

  if (.period %in% "Week") {
    model.string = paste0(model.string, '+ PDQ( period = 52 )')
  }

  if (!any(is.null(covariates))) {
    model.string =
      paste(model.string, '+ xreg(', covariates, ' ) ')
  }

  if (.cat) {
    cat('\n - model:', model.string)
  }

  fit = fit.data %>%
    model(
      arima = ARIMA(as.formula(model.string))
    )

  if (bootstrap) {
    fcast = fit %>%
      forecast(
        h = as.numeric(horizon),
        bootstrap = TRUE,
        times = as.integer(Reps)
      )
  } else {
    if (!any(is.null(covariates))) {
      forecast.fit.data = forecastData %>%
        select(-total) %>%
        filter_index(as.character(time_period) ~ ., .preserve = TRUE) %>%
        filter(
          Month > time_period,
          Month <= (time_period + horizon)
        )

      fcast = fit %>% forecast(new_data = forecast.fit.data)
    } else {
      if (.cat) {
        cat('\n - forecast horizon', horizon)
      }
      fcast = fit %>% forecast(h = as.numeric(horizon))
    }
  }

  # preserve tsibble key and index,
  # indexVar = index_var( fcast )
  # keyVars = key_vars( fcast )
  #
  #
  # fcast = fcast %>%
  #     mutate( !! agg_level :=
  #               as.character( !! rlang::sym( agg_level  ) ) )
  #
  # if ( !split %in% 'None' ){
  #      cat( '\n - tsForecast grouping_var' , split() )
  #      fcast = fcast %>%
  #        mutate(
  #          grouping_var = as.character( !! rlang::sym( split() ) )
  #        )
  # } else {
  #      fcast = fcast %>%
  #        mutate(  grouping_var = 'Total' )
  # }
  #
  # # Ensure result is tstible
  # fcast = fcast %>%
  #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
  #        fill_gaps( .full = TRUE  )
  #
  # # Reconcile
  # if ( input$agg_method %in% "None" ){
  #   if ( input$agg_method %in% 'Bottom up' ){
  #       fcast = fcast %>%
  #         reconcile( bu = bottom_up(base) )
  #   }
  #   if ( input$agg_method %in% 'MINT(ols)' ){
  #     fcast = fcast %>%
  #         reconcile( ols = min_trace(base, method = "ols") )
  #   }
  #   if ( input$agg_method %in% 'MINT(cov)' ){
  #     fcast = fcast %>%
  #         reconcile( mint = min_trace(base, method = "mint_cov") )
  #   }
  # }

  # saveRDS( fcast , 'tsForecast.rds')
  if (.cat) {
    cat('\n - fcast end:')
  } #glimpse( fcast )

  return(fcast)
}

#' Compute key mean percentage error values for forecast evaluation
#' @param Forecast_data fable or data frame; forecast output with a `.mean` column
#' @param test_data tsibble; actual observed values for the evaluation window
#' @param period character; time index column name ("Month" or "Week")
#' @param .split character; split variable for grouping ("None" or a column name)
#' @param by_month logical; compute MPE separately for each month
#' @param agg_level character; aggregation level name
#' @param horizon integer; number of forecast periods (used for mid-point split)
#' @param var character; name of the forecast point-estimate column (default ".mean")
#' @param .cat logical; print progress messages to the console
#' @noRd
key.mpe = function(
  Forecast_data,
  test_data,
  period = "Month",
  .split = 'None',
  by_month = FALSE,
  agg_level = NULL,
  horizon = NULL,
  var = ".mean",
  .cat = FALSE
) {
  if (.cat) {
    cat('\n* data Functions.R  key.mpe()')
  }

  period = dataPeriod(test_data)

  predicted = Forecast_data %>%
    rename(pred = {{ var }})

  actual = test_data %>%
    rename(actual = total)

  keyvars = key_vars(actual)
  if (.cat) {
    cat('\n - keyvars', keyvars)
  }

  truth = predicted %>%
    # select( -dataCol, -grouping_var ) %>%
    inner_join(
      actual %>% select_at(c(keyvars, period, "actual")),
      by = c(period, keyvars)
    )

  if (.cat) {
    cat('\n - truth')
  } #print( truth )

  if (!is.null(horizon)) {
    mid_point = round(as.integer(horizon) / 2)
  }

  # summarise
  if ('orgUnit' %in% keyvars) {
    group_no_orgUnit = setdiff(keyvars, 'orgUnit')

    truth = truth %>% as_tibble() %>% group_by_at(group_no_orgUnit)
  } else {
    truth = truth %>%
      group_by_key() %>%
      index_by(1)
  }

  if (.split != 'None') {
    .split = c(keyvars, .split)
  }
  if (by_month) {
    .split = c(keyvars, "Month")
  }

  e = truth %>%
    as_tibble %>%
    # mutate( Month = as.character( Month ) ) %>%
    group_by_at(.split) %>%
    summarise(
      # mpe = ifelse( mean( pred , na.rm = T ) > 0 ,
      #                mean( actual - pred  ,
      #                      na.rm = T ) /
      #                  mean( pred , na.rm = T ) ,
      #                NA ) ,
      #
      # !! rlang::sym( period ) := nth( !! rlang::sym( period )  , mid_point ) ,
      #
      # actual = ifelse( mpe>=0 , max( actual, na.rm = TRUE ),
      #                  min( actual, na.rm = TRUE  )
      #                  #nth( actual , mid_point )
      # ) ,

      predicted = sum(pred),
      # pred_hilo80 = `80%` ,
      # pred_hilo95 = `95%` ,
      pred_upper = sum(`80%`$upper),
      pred_lower = sum(`80%`$lower),
      actual = sum(actual),
      diff = actual - predicted,
      abs_diff = abs(diff),
      mpe = ifelse(is.na(predicted), NA, diff / predicted),
      mape = ifelse(is.na(predicted), NA, abs_diff / predicted),
      .model = max(.model),
      just = ifelse(mpe >= 0, 2, -2)
    ) %>%
    as_tibble()

  if (!is.null(agg_level)) {
    e = e %>%
      mutate(!!agg_level := as.character(!!rlang::sym(agg_level)))
  }

  if (any(!.split %in% 'None') && length(.split) == 1) {
    if (.cat) {
      cat('\n - key.mpe grouping_var', .split)
    }
    e = e %>%
      mutate(
        grouping_var = as.character(!!rlang::sym(.split))
      )
  } else {
    e = e %>%
      mutate(grouping_var = 'Total')
  }

  # print( "end key.mpe"); #glimpse(e )
  return(e)
}

#' Plot time-series trends with forecast overlay
#' @param trend.data tsibble; the prepared trend dataset to plot
#' @param scale logical; allow free y-axis (do not force zero baseline)
#' @param legend logical; show the colour legend
#' @param label logical; add text labels at the end of each series
#' @param facet_split logical; split into facets
#' @param facet_vars character vector; variables to use for faceting
#' @param facet_admin logical; facet by administrative level
#' @param agg_level character; aggregation level name for display
#' @param pre_evaluation logical; overlay pre-intervention forecast
#' @param evaluation logical; overlay post-intervention forecast
#' @param horizon integer; number of forecast periods to overlay
#' @param eval_date yearmonth; start date of the evaluation window
#' @param pe logical; show percentage-error annotation
#' @param .cat logical; print progress messages to the console
#' @param ... additional arguments
#' @noRd
plotTrends = function(
  trend.data,
  scale = FALSE,
  legend = FALSE,
  label = FALSE,
  facet_split = FALSE,
  facet_vars = NULL,
  facet_admin = FALSE,
  agg_level = 'National',
  pre_evaluation = FALSE,
  evaluation = FALSE,
  horizon = 12,
  eval_date = as.yearmonth('Jan 2021', "%Y %b"),
  pe = TRUE,
  .cat = FALSE,
  ...
) {
  if (.cat) {
    cat('\n* data Functions.R plotTrends():')
  }

  .limits =
    if (scale) {
      c(NA, NA)
    } else {
      c(0, NA)
    }

  # data.text = paste( unique( plotData$data ), collapse = " + " )

  if (.cat) {
    cat('\n - ploTrends .d:')
  } #glimpse(.d)

  # if ( !input$filter_display %in% 'All' ) .d = .d %>%
  #         filter( .data[[ split() ]] %in%
  #                   input$filter_display )

  .period = dataPeriod(trend.data)

  ## Main plot ####
  g = trend.data %>%
    filter(!is.na(total)) %>%
    # autoplot( total ) +
    ggplot(aes(
      x = !!rlang::sym(.period),
      y = total,
      group = grouping_var, # as.character( !! rlang::sym( input$agg_level  ) )
      color = grouping_var
    )) +
    geom_line() +
    theme_minimal()

  # Testing
  # save(.d, file = 'plot-trend-test-data.rda')

  if (.cat) {
    cat('\n - basic plot done')
  }

  if (!legend) {
    g = g +
      theme(legend.position = "none")
  }

  if (label) {
    g = g +
      geom_label_repel(
        data = .d %>%
          filter(
            !!rlang::sym(.period) == max(.d %>% pull(.period), na.rm = T)
          ),
        aes(label = grouping_var, group = grouping_var)
      )
  }

  # Determine number of agg levels available
  # If only one, do not facet (causes error, perhaps because of autoplot?)

  if (!is.null(agg_level)) {
    num_agg_levels = count(
      trend.data %>% as_tibble,
      !!rlang::sym(agg_level)
    ) %>%
      nrow()
  } else {
    num_agg_levels = 1
  }

  # if ( input$agg_level != levelNames()[1] & input$facet_admin ){

  if (num_agg_levels > 1 & facet_admin) {
    if (.cat) {
      cat('\n -  admin facets')
    }

    if (facet_split) {
      if (.cat) {
        cat('\n -  facet admin - split')
      }

      g = g +
        facet_grid(
          rows = vars(as.character(!!rlang::sym(agg_level))),
          cols = grouping_var,
          scales = "free_y"
        )
    } else {
      g = g +
        facet_wrap(
          vars(as.character(!!rlang::sym(agg_level))),
          scales = "free_y"
        )
    }
  } else {
    if (facet_split) {
      if (.cat) {
        cat('\n - facet_split')
      }
      g = g +
        # facet_wrap( ~ grouping_var   ,
        # facet_grid( vars(  RTSS , pbo )   , scales = "free_y" )
        facet_grid(vars(!!rlang::sym(facet_vars)), scales = "free_y")
    }
  }

  # Time scale
  if (.cat) {
    cat('\n - Evaluation: setting x axis time scale', .period)
  }
  if (.period %in% 'Month') {
    g = g +
      scale_x_yearmonth("", date_labels = "%b\n%Y", date_breaks = "1 year")
  }
  # Default for weeks seems ok - 6 months
  # if ( .period %in% 'Week') g = g + scale_x_yearweek("", date_breaks = "1 year" )

  g = g +
    scale_y_continuous(label = comma, limits = .limits) +
    scale_color_discrete(drop = TRUE) +
    labs(
      y = "",
      x = ""
      # title = str_wrap( indicator , 200 ) ,
      # subtitle = str_wrap( data.text , 200 )
      # caption =  str_wrap( caption.text() , 200 )
    )
  if (.cat) {
    cat('\n - axis scales and labs done')
  }

  # Eval Date
  #     cat( '\n - evaluation date' , input$evaluation_month )
  #     if ( .period %in% 'Month' ) eval_date =   yearmonth( input$evaluation_month  )
  #     if ( .period %in% 'Week' ) eval_date =   yearweek( input$evaluation_month  )
  #     cat( '\n - eval_date:' , eval_date )

  # ## Pre-Evaluation trend line #####
  if (pre_evaluation) {
    if (.cat) {
      cat('\n - pre-evaluation line.  ')
    }
    if (.cat) {
      cat('\n - pi_levels:', pi_levels())
    }

    if (.cat) {
      cat('\n - pre-evaluation date')
    }
    if (.period %in% 'Month') {
      pre_eval_date = as.yearmonth(input$evaluation_month, "%Y %b") - 12
    }
    if (.period %in% 'Week') {
      pre_eval_date = yearweek(input$evaluation_month) - 25
    }
    if (.cat) {
      cat('\n - pre_eval_date:', pre_eval_date)
    }

    g = g +
      forecast::autolayer(
        tsPreForecast(),
        # , level = c(80,90) # ci_levels()
        PI = TRUE,
        color = 'black',
        linetype = 'dotted',
        size = 2,
        alpha = .75
      ) +
      # geom_line( data = tsPreForecast(), aes(  y = .mean )
      #   # ,   color = 'light blue'
      #   , alpha = .75
      #   , linetype = 'dotted'  , size = 2
      # ) +
      # geom_vline( xintercept = as.Date( pre_eval_date ) ,
      #             color = 'brown' ,
      #             alpha = .25 ) +
      geom_vline(xintercept = as.Date(eval_date), color = 'black', alpha = 1)

    # if ( input$pe ) g = g +
    #   geom_label_repel( data =  key.mape() ,
    #            aes(  x = !! rlang::sym( period() ) , y = actual ,
    #            label = paste( "MAPE:" , percent( mape, accuracy = 1.0 ) ) ,
    #            hjust = just ) ,
    #            # force_pull = 0 ,
    #            segment.colour = NA
    #            )

    if (.cat) cat('\n - pre-evaluation line done')
  }

  ## Evaluation Trend Line ####
  if (evaluation) {
    if (.cat) {
      cat('\n - evaluation with horizon - ', horizon)
    }
    tsForecast = getForecast(plotData, horizon = horizon, ...)

    if (.cat) {
      cat('\n - evaluation line.  ')
    }
    # cat( '\n - evaluation line.  ' , 'pi_levels:' , pi_levels() )

    g = g +
      forecast::autolayer(
        tsForecast,
        # , level = pi_levels()
        color = 'black',
        # , linetype = 'dashed'
        size = 1,
        alpha = .5
      ) +
      # geom_line( data = tsForecast() , aes( y = .mean )
      #   # ,   color = 'light blue'
      #   , alpha = .75
      #   , linetype = 'dotted'  , size = 2
      # ) +

      geom_vline(xintercept = as.Date(eval_date), color = 'blue', alpha = 1)

    # annotate( "text" ,
    #           x = as.Date( eval_date ) ,
    #           y = Inf ,
    #           hjust = 0 , vjust = 1 ,
    #           label = paste( "MPE:\n" )
    #           ) +

    mpeData = key.mpe(Forecast_data = tsForecast, test_data = plotData)

    if (pe) {
      g = g +
        geom_label_repel(
          data = mpeData,
          aes(
            x = !!rlang::sym(.period),
            y = pred,
            label = paste("MPE:", percent(mpe, accuracy = 1.0)),
            hjust = just
          ),
          # force_pull = 0 ,
          segment.colour = NA
        )
    }
  }

  if (.cat) {
    cat('\n - evaluation line done')
  }

  ## End ####
  if (.cat) {
    cat('\n - end plotTrends():')
  }

  # saveRDS( g, 'plotTrends.rds')
  return(g)
}

