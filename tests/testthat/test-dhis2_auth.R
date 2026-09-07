# Input validation — no network required.

test_that("loginDHIS2 rejects an empty or missing baseurl", {
  expect_error(loginDHIS2(baseurl = "", username = "admin", password = "district"))
  expect_error(loginDHIS2(baseurl = NA_character_, username = "a", password = "b"))
  expect_error(loginDHIS2(baseurl = character(0), username = "a", password = "b"))
})

test_that("loginDHIS2 rejects a baseurl with no scheme", {
  expect_error(
    loginDHIS2(baseurl = "play.dhis2.org/40/", username = "a", password = "b")
  )
})

test_that("loginDHIS2 warns when connecting over http", {
  # Warns before any request is made, so an unroutable host is fine here.
  expect_warning(
    try(
      loginDHIS2(
        baseurl  = "http://127.0.0.1:1/",
        username = "a", password = "b", timeout = 1
      ),
      silent = TRUE
    ),
    "plain text"
  )
})

# Regression test for the bug this replaced: DHIS2 answers unauthenticated API
# requests with its HTML login form and a 200 status. Checking only the status
# code made a wrong password look like a successful login.
#
# The retired versioned play URLs redirect to exactly that login form, which
# makes them a stable fixture for the HTML-200 case. Note that no public DHIS2
# instance reliably returns a clean 401 for bad credentials, which is why this
# asserts on the HTML-body path rather than on a status code.
test_that("loginDHIS2 returns FALSE when a 200 response is not JSON", {
  skip_if_offline()

  expect_false(
    suppressMessages(
      loginDHIS2(
        baseurl  = "https://play.dhis2.org/40.6.1/",
        username = "admin",
        password = "wrong_password_for_testing"
      )
    )
  )
})
