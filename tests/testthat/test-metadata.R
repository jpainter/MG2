test_that("clean_invalid_characters removes zero-width space", {
  df <- data.frame(
    name  = c("Good\u200Bname", "Normal", NA_character_),
    value = 1:3,
    stringsAsFactors = FALSE
  )

  result <- clean_invalid_characters(df)

  expect_equal(as.character(result$name[1]), "Goodname")
  expect_equal(as.character(result$name[2]), "Normal")
  expect_true(is.na(result$name[3]))
})

test_that("clean_invalid_characters returns input unchanged when no invalid chars", {
  df <- data.frame(x = c("Hello", "World"), stringsAsFactors = FALSE)
  result <- clean_invalid_characters(df)
  expect_equal(as.character(result$x), c("Hello", "World"))
})

test_that("clean_invalid_characters handles data frame with no character columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  result <- clean_invalid_characters(df)
  expect_equal(nrow(result), 3L)
})

test_that("truncate_df_chars truncates strings over the limit", {
  long_str <- paste(rep("word", 200), collapse = " ")  # > 500 chars
  df       <- data.frame(x = long_str, stringsAsFactors = FALSE)

  result <- truncate_df_chars(df, limit = 50L)

  expect_lte(nchar(as.character(result$x)), 60L)  # 50 + suffix
  expect_true(grepl("TRUNC", result$x))
})

test_that("truncate_df_chars leaves short strings unchanged", {
  df     <- data.frame(x = "short string", stringsAsFactors = FALSE)
  result <- truncate_df_chars(df, limit = 500L)
  expect_equal(as.character(result$x), "short string")
})

test_that("truncate_df_chars handles NA values", {
  df     <- data.frame(x = c(NA_character_, "hello"), stringsAsFactors = FALSE)
  result <- truncate_df_chars(df, limit = 500L)
  expect_true(is.na(result$x[1]))
  expect_equal(as.character(result$x[2]), "hello")
})

test_that("truncate_df_chars with NULL suffix omits suffix text", {
  long_str <- paste(rep("a", 100), collapse = "")
  df       <- data.frame(x = long_str, stringsAsFactors = FALSE)

  result <- truncate_df_chars(df, limit = 10L, suffix = NULL)

  expect_equal(nchar(as.character(result$x)), 10L)
})
