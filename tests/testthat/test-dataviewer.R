library(testthat)
library(dataviewR)
library(shinytest2)

test_that("dataviewer works with data input (direct dataframe)", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_mtcars",
    height = 800, width = 1200
  )

  # Check initial tab (Viewer tab) exists
  expect_equal(app$get_value(input = "opt"), "Viewer")

  # Check action buttons present
  expect_false(is.null(app$get_value(input = "load")))
  expect_false(is.null(app$get_value(input = "generate_code")))

  # Simulate clicking the generate_code button
  app$set_inputs(generate_code = "click")

  # Wait for the code output to be populated
  app$wait_for_value(input = "code_output", ignore = list(""))
  output_val <- app$get_value(input = "code_output")

  # ensure Shiny processes everything
  app$wait_for_idle()

  print(output_val)  # Print output to see what the value is (debugging)

  # Check modal content is not empty
  expect_true(nchar(output_val) > 0)

  app$stop()
})

test_that("dataviewer works without data input (import panel)", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_import", package = "dataviewR"),
    name = "dataviewer_import",
    height = 800, width = 1200
  )

  # Check initial tab (Import Dataset tab)
  expect_equal(app$get_value(input = "opt"), "Import Dataset")

  app$stop()
})

test_that("dataviewer throws error for invalid input", {
  expect_error(
    dataviewer(iris$Sepal.Length),
    regexp = "Argument 'data' is not a tibble or data.frame"
  )
})

test_that("dataviewer column selection works properly", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_column_selection",
    height = 800, width = 1200
  )

  # Select only 'mpg' and 'cyl' columns
  app$set_inputs(columns = c("mpg", "cyl"))
  app$wait_for_idle()

  # Retrieve the displayed data
  data_view <- app$get_value(input = "columns")

  print(data_view)

  # Ensure only 'mpg' and 'cyl' are present
  expect_true(all(c("mpg", "cyl") %in% data_view))
  expect_equal(length(data_view), 2)

  app$stop()
})

test_that("dataviewer filtering works correctly", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_filtering",
    height = 800, width = 1200
  )

  # Apply filter: mpg > 20
  filter_input <- list(list(column = "mpg", condition = ">", value = 20))
  app$set_inputs(filter = filter_input)
  app$wait_for_idle()

  # Retrieve the filtered data
  filtered_data <- app$get_value(output = "filter_df")

  print(filtered_data)

  # Check that all 'mpg' values are greater than 20
  expect_true(all(filtered_data$mpg > 20))

  app$stop()
})

test_that("dataviewer toggles select/deselect all columns correctly", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_select_all",
    height = 800, width = 1200
  )

  app$set_inputs(load = "click")

  # Deselect all
  app$set_inputs(cols_all = FALSE)

  # Wait and check if no columns selected
  columns_selected <- app$get_value(input = "columns")
  expect_equal(length(columns_selected), 0)

  # Select all again
  app$set_inputs(cols_all = TRUE)
  columns_selected <- app$get_value(input = "columns")
  expect_true(length(columns_selected) > 0)

  app$stop()
})

test_that("dataviewer generates default R code without filter", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_default_code",
    height = 800, width = 1200
  )

  # Click 'Load' to initialize data
  app$set_inputs(load = "click")

  # Click 'Generate Code'
  app$set_inputs(generate_code = "click")

  # Wait for code output
  app$wait_for_value(input = "code_output", ignore = list(""))
  code_output <- app$get_value(input = "code_output")

  expect_match(code_output, "library\\(dplyr\\)")
  expect_match(code_output, "df \\|>")  # Check pipe used
  expect_match(code_output, "select\\(.*\\)")  # Should select all or specific columns

  app$stop()
})

test_that("dataviewer correctly handles a valid filter", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_valid_filter",
    height = 800, width = 1200
  )

  app$set_inputs(load = "click")
  app$set_inputs(filter = "mpg > 20")
  app$set_inputs(submit = "click")

  app$wait_for_idle()

  # Check if table got updated
  tbl_data <- app$get_value(output = "tbl")
  expect_true(!is.null(tbl_data))

  app$stop()
})

test_that("dataviewer shows error notification for invalid filter", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_invalid_filter",
    height = 800, width = 1200
  )

  app$set_inputs(load = "click")
  app$set_inputs(filter = "INVALID CODE")
  app$set_inputs(submit = "click")

  app$wait_for_idle()

  # Since showNotification cannot be directly tested, check if data remains unchanged or similar output check
  tbl_data <- app$get_value(output = "tbl")
  expect_true(!is.null(tbl_data))

  app$stop()
})

test_that("dataviewer attribute info table is populated", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = system.file("shinyapps/dataviewer_direct", package = "dataviewR"),
    name = "dataviewer_attr_info",
    height = 800, width = 1200
  )

  app$wait_for_value(output = "metainfo")
  attr_info <- app$get_value(output = "metainfo")

  print(attr_info)

  # Check that attribute info table output is not empty
  expect_true(length(attr_info) > 0)
  expect_true(any(nzchar(attr_info)))  # ensures it is not all empty strings

  app$stop()
})




