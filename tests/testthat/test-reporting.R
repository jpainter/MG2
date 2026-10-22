# facility_reports(), reports_by_period(), reports_by_year(), aggregate_champions(),
# outlier_summary_values()

library(data.table)

ym <- function(x) tsibble::yearmonth(x)

# Two elements: A (categories x, y) and B (category z). Three facilities, Jan-Mar 2024.
#   f1: A_x, A_y, B_z every month          -> passes every rule
#   f2: A_x every month, B_z in Jan only   -> any_selected; all_elements in Jan only
#   f3: B_z in Feb only, A_x NA in Mar     -> any_selected in Feb only
reports_data <- function() {
  months <- ym(c("2024 Jan", "2024 Feb", "2024 Mar"))
  rbind(
    data.table(orgUnit = "f1", Month = rep(months, 3), data = rep(c("A_x", "A_y", "B_z"), each = 3), original = 1),
    data.table(orgUnit = "f2", Month = months, data = "A_x", original = 2),
    data.table(orgUnit = "f2", Month = months[1], data = "B_z", original = 2),
    data.table(orgUnit = "f3", Month = months[2], data = "B_z", original = 3),
    data.table(orgUnit = "f3", Month = months[3], data = "A_x", original = NA_real_)
  )
}
cats <- c("A_x", "A_y", "B_z")

test_that("facility_reports applies each reporting rule", {
  d <- reports_data()
  n <- function(rule, cs = cats) nrow(facility_reports(d, "Month", rule, cs))
  expect_equal(n("any_data"), 3 + 3 + 1)      # f3's NA in March is not a report
  expect_equal(n("any_selected", "A_x"), 3 + 3)
  expect_equal(n("all_elements"), 3 + 1)      # f1 always, f2 in January
  expect_equal(n("all_categories"), 3)        # f1 only
  expect_equal(n("any_selected", character(0)), n("any_data"))
})

test_that("facility_reports keeps the period class", {
  r <- facility_reports(reports_data(), "Month", "any_selected", cats)
  expect_s3_class(r$Month, "yearmonth")
  expect_named(r, c("orgUnit", "Month"))
})

test_that("mostFrequentReportingOUs selects from facility_reports", {
  d <- reports_data()
  champs <- function(rule) sort(mostFrequentReportingOUs(
    d, startingMonth = ym("2024 Jan"), endingMonth = ym("2024 Mar"), period = "Month",
    missing_reports = 0, reporting_rule = rule, data_categories = cats))
  expect_equal(champs("any_selected"), c("f1", "f2"))
  expect_equal(champs("all_categories"), "f1")
  expect_equal(champs("all_elements"), "f1")
})

test_that("reports_by_period and reports_by_year count facilities", {
  r <- facility_reports(reports_data(), "Month", "any_selected", cats)
  bp <- reports_by_period(r, "Month", champions = "f1")
  expect_equal(bp$all, c(2L, 3L, 2L))
  expect_equal(bp$champions, c(1L, 1L, 1L))
  expect_equal(bp$year, rep(2024L, 3))

  by <- reports_by_year(r, "Month", champions = c("f1", "f2"))
  expect_equal(by$n_periods, c(1L, 3L))
  expect_equal(by$all, c(1L, 2L))
  expect_equal(by$champions, c(0L, 2L))
})

test_that("aggregate_champions sums remaining values and drops empty unit-months", {
  months <- ym(c("2024 Jan", "2024 Feb"))
  d <- data.table(orgUnit = c("f1", "f1", "f2", "f2", "f3"),
                  Month = months[c(1, 2, 1, 2, 1)],
                  value = c(10, NA, 5, NA, 100))
  map <- data.frame(orgUnit = c("f1", "f2", "f3"), unit = c("u1", "u1", "u2"))
  out <- aggregate_champions(d, champions = c("f1", "f2"), unit_map = map, value = "value")
  expect_equal(out$unit, "u1")              # u2 has no champion; February is all censored
  expect_equal(out$value, 15)
  expect_s3_class(out$Month, "yearmonth")
})

test_that("outlier.summary.tibble formats outlier_summary_values", {
  d <- data.table(original = c(1, 2, 3, 1000, 5000),
                  mad15 = c(FALSE, FALSE, FALSE, TRUE, TRUE),
                  mad10 = c(FALSE, FALSE, TRUE, TRUE, TRUE))
  v <- outlier_summary_values(d, c("mad15", "mad10"))
  expect_equal(as.character(v$err), c("mad15", "mad10", "No Error Flags"))
  expect_equal(v$n, c(2, 3, 2))
  expect_equal(v$total, c(6000, 6003, 3))
  f <- outlier.summary.tibble(d, c("mad15", "mad10"))
  expect_equal(f$N, scales::comma(v$n))
  expect_equal(f$`Total Value`, scales::comma(v$total))
})
