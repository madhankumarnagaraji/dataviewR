library(testthat)
library(shiny)
library(dplyr)

test_that("Filter code generation works correctly", {
  # Test the filter code generation logic
  filter_text <- "mpg > 20 & cyl == 4"
  expected <- "filter(mpg > 20 & cyl == 4)"

  # Simulate the reactive
  filter_code <- if (nzchar(stringr::str_trim(filter_text))) {
    paste0("filter(", filter_text, ")")
  } else NULL

  expect_equal(filter_code, expected)
})

test_that("Selected columns code generation works correctly", {
  selected_cols <- c("mpg", "cyl", "hp")

  # Simulate the reactive logic
  needs_quotes <- !grepl("^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9._]*$", selected_cols)
  formatted_cols <- ifelse(needs_quotes,
                           paste0("`", selected_cols, "`"),
                           selected_cols)
  code <- paste0("select(", paste(formatted_cols, collapse = ", "), ")")

  expect_equal(code, "select(mpg, cyl, hp)")
})

test_that("Column names with special characters get backticks", {
  selected_cols <- c("mpg", "cyl-special", "hp")

  needs_quotes <- !grepl("^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9._]*$", selected_cols)
  formatted_cols <- ifelse(needs_quotes,
                           paste0("`", selected_cols, "`"),
                           selected_cols)
  code <- paste0("select(", paste(formatted_cols, collapse = ", "), ")")

  expect_match(code, "`cyl-special`")
})

test_that("Filter expression parsing works", {
  test_data <- mtcars

  # Valid filter
  result <- tryCatch({
    dplyr::filter(test_data, eval(parse(text = "cyl == 4")))
  }, error = function(e) NULL)

  expect_false(is.null(result))
  expect_equal(nrow(result), 11)

  # Invalid filter
  result_invalid <- tryCatch({
    dplyr::filter(test_data, eval(parse(text = "invalid ++ code")))
  }, error = function(e) "error")

  expect_equal(result_invalid, "error")
})

test_that("NA handling in final_df works correctly", {
  test_data <- data.frame(
    x = c(1, 2, NA, 4),
    y = c("a", NA, "c", "d"),
    stringsAsFactors = FALSE
  )

  # Simulate the NA handling logic
  result <- test_data %>%
    mutate(
      across(
        where(is.character) | where(is.factor),
        ~forcats::fct_drop(forcats::fct_na_value_to_level(as.factor(.x), level = "<NA>"))
      )
    )

  expect_true("<NA>" %in% levels(result$y))
  expect_equal(as.character(result$y[2]), "<NA>")
})
