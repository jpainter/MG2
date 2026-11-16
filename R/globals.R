# Column names used unquoted inside dplyr verbs, ggplot2::aes() and
# data.table `[` expressions, plus names those expressions create (`:=`,
# `i.` join prefixes, `.()` / the magrittr `.` placeholder). R CMD check cannot
# see that these are evaluated against data, so it reports them as undefined
# globals; declaring them here clears that NOTE.
#
# Every name was reviewed against its use site (2026-11-16). Several flagged
# names turned out to be real bugs rather than columns (undefined variables
# in selectedData(), htsFormula(), api_data(), mg2_demo_setup()) and were
# fixed instead of being listed. Before adding a name here, check it is
# really a column -- this list silences the check for every function.

utils::globalVariables(c(
  "% Passed", "%N", "%Total Value", ".", "..keep_cols", ".base_elem",
  ".chunk_key", ".data", ".fitted", ".keep_cols", ".max", ".mean", ".model",
  ".Month_int", ".month_int", ".p", ".sd", ".v", ".year", "active", "actual",
  "ae", "Alg", "AllSmall", "annual", "attend_total", "cat_list", "Categories",
  "Category", "categoryCombo", "categoryCombo.id", "categoryOptionCombo",
  "categoryOptionCombo.ids", "champ_mean", "Combined", "consistent", "COUNT",
  "current.count", "current.value", "data", "data.id", "data_id", "dataCol",
  "dataElement", "dataElement.id", "dataSet", "dataSet.id",
  "dataSetElements.id", "dataSetElements.id_dataElement", "denominator",
  "do_split", "e", "effectiveLeaf", "err", "error", "expected", "fill_val",
  "Formatted Change", "i.do_split", "i.expected", "i.mad10", "i.mad15",
  "i.mad5", "i.mg2_bucket", "i.seasonal3", "i.seasonal5", "id", "incomplete",
  "int", "Intervention Rank", "key_entry_error", "label", "Largest Value",
  "latest_month", "leaf", "Level", "level", "levelName", "ls_val", "mad10",
  "mad15", "mad5", "Max", "Mean_MASE", "mean_mm", "mean_rain", "Median",
  "Median Value", "method", "mg2_bucket", "Min", "missing_denominator",
  "missing_numerator", "Month", "month_label", "N", "n.e", "n.t", "n_cats",
  "n_datasets", "n_evaluated", "n_failed", "n_months", "n_passed", "n_periods",
  "n_reporting", "n_total", "name", "not_key_or_over_under", "not_mad",
  "numerator", "older", "older.x", "opt_list", "Org Unit", "orgUnit",
  "orgUnitName", "original", "over_max", "paired", "parent", "parent.id",
  "parentName", "pass", "pct", "pct_passed", "pe", "percent_no_error",
  "percent_value_no_error", "Period", "period", "period_val", "periodType",
  "pn", "population", "pr", "prev.count", "prev.value", "region",
  "region_name", "report_periods", "reported", "reporting", "rs_val",
  "rule_id", "rule_name", "same", "samples", "seasonal3", "seasonal5",
  "Selected", "Smallest Value", "status", "Subset Total", "SUM", "swape",
  "tooltip", "Total", "total", "Total Value", "unit", "val", "val_d", "val_n",
  "value", "value_raw", "var", "Week", "WPE", "Year", "year_col", "years",
  "ymax", "ymax2", "ymin", "ymin2"
))
