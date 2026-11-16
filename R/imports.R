# Functions the package calls by bare name. Declaring them here pins each name
# to one package -- e.g. `filter` is always dplyr::filter, never stats::filter
# -- instead of depending on what happens to be attached, and clears R CMD
# check's "no visible global function definition" NOTE.
#
# New code should prefer pkg::fn(); add a name here only for existing bare
# calls.

#' @importFrom dplyr across all_of arrange as_tibble bind_rows case_when count
#' @importFrom dplyr distinct filter full_join glimpse group_by group_by_at
#' @importFrom dplyr inner_join left_join mutate mutate_all n pull rename
#' @importFrom dplyr select starts_with summarise ungroup vars where
#' @importFrom fabletools aggregate_key
#' @importFrom future future
#' @importFrom purrr is_empty
#' @importFrom scales percent
#' @importFrom sf st_drop_geometry
#' @importFrom shiny incProgress setProgress showNotification withProgress
#' @importFrom stats aggregate fitted median setNames
#' @importFrom stringr fixed str_detect str_ends str_remove str_replace_all
#' @importFrom stringr str_split_1 str_sub str_trim
#' @importFrom tidyr replace_na
#' @importFrom tsibble as_tsibble is_tsibble yearmonth yearweek
#' @importFrom utils file_test flush.console tail
NULL
