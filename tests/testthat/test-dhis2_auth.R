test_that("loginDHIS2 returns FALSE for invalid credentials", {
  skip_if_offline()

  # DHIS2 play instance — expect authentication failure with bad password
  result <- loginDHIS2(
    baseurl  = "https://play.dhis2.org/40.6.1/",
    username = "admin",
    password = "wrong_password_for_testing"
  )

  expect_false(result)
})

test_that("loginDHIS2 requires non-empty baseurl", {
  # Should error or fail gracefully on empty URL
  expect_error(
    loginDHIS2(baseurl = "", username = "admin", password = "district")
  )
})
