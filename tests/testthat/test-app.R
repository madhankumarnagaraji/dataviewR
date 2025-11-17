library(testthat)
library(shinytest2)
library(dataviewR)

# Helper function to create app instance
app_driver_wrapper <- function() {
  dataviewer(mtcars)
}

test_that("App starts and basic elements are present", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Start the app
  app <- AppDriver$new(app_driver_wrapper())

  # Wait for app to fully initialize
  app$wait_for_idle()

  # Check if Import Dataset tab exists
  expect_true(app$get_js("$('#opt-import_tab').length > 0"))

  # The first data tab should be for mtcars
  # Check if mtcars tab exists (will have a dynamic ID like "tab_mtcars_1")
  tabs_html <- app$get_html("#opt")
  expect_true(grepl("mtcars", tabs_html, ignore.case = TRUE))

  # Get the active tab value
  active_tab <- app$get_value(input = "opt")
  expect_true(grepl("^tab_mtcars", active_tab))

  # Build namespaced IDs for the active mtcars tab
  ns_prefix <- active_tab

  # Check for core UI elements
  filter_exists <- app$get_js(sprintf("$('#%s-filter').length > 0", ns_prefix))
  submit_exists <- app$get_js(sprintf("$('#%s-submit').length > 0", ns_prefix))
  table_exists <- app$get_js(sprintf("$('#%s-tbl').length > 0", ns_prefix))

  expect_true(filter_exists)
  expect_true(submit_exists)
  expect_true(table_exists)

  # Check total rows display (mtcars has 32 rows)
  total_rows_output <- app$get_value(output = paste0(ns_prefix, "-totalrows"))
  expect_equal(total_rows_output, "32")

  app$stop()
})

test_that("Filter and Submit button interaction works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- AppDriver$new(app_driver_wrapper())
  app$wait_for_idle()

  # Get active tab ID
  active_tab <- app$get_value(input = "opt")
  ns_prefix <- active_tab

  # Set filter condition
  filter_input <- paste0(ns_prefix, "-filter")
  app$set_inputs(!!rlang::sym(filter_input) := "cyl == 4")

  # Click submit button
  submit_button <- paste0(ns_prefix, "-submit")
  app$click(submit_button)

  # Wait for the table to update
  app$wait_for_idle(duration = 1000)

  # Check that the filter was applied by examining the DataTable
  # After filtering for cyl == 4, we should have 11 rows
  dt_info <- app$get_js(sprintf(
    "$('#%s-tbl').DataTable().page.info().recordsDisplay",
    ns_prefix
  ))
  expect_equal(dt_info, 11)

  app$stop()
})

test_that("Generate R Code button works and displays correct code", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- AppDriver$new(app_driver_wrapper())
  app$wait_for_idle()

  active_tab <- app$get_value(input = "opt")
  ns_prefix <- active_tab

  # Apply a filter
  filter_input <- paste0(ns_prefix, "-filter")
  app$set_inputs(!!rlang::sym(filter_input) := "mpg > 20 & cyl == 4")

  submit_button <- paste0(ns_prefix, "-submit")
  app$click(submit_button)
  app$wait_for_idle()

  # Click Generate R Code button
  generate_code_btn <- paste0(ns_prefix, "-generate_code")
  app$click(generate_code_btn)
  app$wait_for_idle()

  # Get the code from the modal textarea
  code_textarea_id <- paste0(ns_prefix, "-code_output")
  code_content <- app$get_js(sprintf(
    "$('#%s').val()",
    code_textarea_id
  ))

  # Verify the generated code contains expected elements
  expect_true(grepl("library\\(dplyr\\)", code_content))
  expect_true(grepl("mtcars", code_content))
  expect_true(grepl("filter\\(mpg > 20 & cyl == 4\\)", code_content))
  expect_true(grepl("\\|>", code_content))

  app$stop()
})

test_that("Column selection works correctly", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- AppDriver$new(app_driver_wrapper())
  app$wait_for_idle()

  active_tab <- app$get_value(input = "opt")
  ns_prefix <- active_tab

  # Deselect all columns first
  select_all_checkbox <- paste0(ns_prefix, "-cols_all")
  app$set_inputs(!!rlang::sym(select_all_checkbox) := FALSE)
  app$wait_for_idle()

  # Select only specific columns
  columns_input <- paste0(ns_prefix, "-columns")
  app$set_inputs(!!rlang::sym(columns_input) := c("mpg", "cyl", "hp"))
  app$wait_for_idle()

  # Click Generate Code to verify selection
  generate_code_btn <- paste0(ns_prefix, "-generate_code")
  app$click(generate_code_btn)
  app$wait_for_idle()

  # Check generated code includes select statement
  code_textarea_id <- paste0(ns_prefix, "-code_output")
  code_content <- app$get_js(sprintf("$('#%s').val()", code_textarea_id))

  expect_true(grepl("select\\(", code_content))
  expect_true(grepl("mpg", code_content))
  expect_true(grepl("cyl", code_content))
  expect_true(grepl("hp", code_content))

  app$stop()
})

test_that("Clear button resets filter", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- AppDriver$new(app_driver_wrapper())
  app$wait_for_idle()

  active_tab <- app$get_value(input = "opt")
  ns_prefix <- active_tab

  # Set a filter
  filter_input <- paste0(ns_prefix, "-filter")
  app$set_inputs(!!rlang::sym(filter_input) := "cyl == 6")

  submit_button <- paste0(ns_prefix, "-submit")
  app$click(submit_button)
  app$wait_for_idle()

  # Verify filter is applied (should have 7 rows for cyl == 6)
  dt_info_filtered <- app$get_js(sprintf(
    "$('#%s-tbl').DataTable().page.info().recordsDisplay",
    ns_prefix
  ))
  expect_equal(dt_info_filtered, 7)

  # Click clear button
  clear_button <- paste0(ns_prefix, "-clear")
  app$click(clear_button)
  app$wait_for_idle()

  # Verify all 32 rows are back
  dt_info_cleared <- app$get_js(sprintf(
    "$('#%s-tbl').DataTable().page.info().recordsDisplay",
    ns_prefix
  ))
  expect_equal(dt_info_cleared, 32)

  # Verify filter input is empty
  filter_value <- app$get_value(input = filter_input)
  expect_equal(filter_value, "")

  app$stop()
})

test_that("Import Dataset panel works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Start app without data to trigger import panel
  app <- AppDriver$new(dataviewer)
  app$wait_for_idle()

  # Should start on import tab
  active_tab <- app$get_value(input = "opt")
  expect_equal(active_tab, "import_tab")

  # Verify import UI elements exist
  import_ui_exists <- app$get_js("$('#myid-data-select').length > 0")
  expect_true(import_ui_exists)

  app$stop()
})

test_that("Multiple datasets create separate tabs", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Create app with multiple datasets
  app <- AppDriver$new(function() {
    dataviewer(mtcars, iris)
  })
  app$wait_for_idle()

  # Get all tab IDs
  tabs_html <- app$get_html("#opt")

  # Should have tabs for both datasets
  expect_true(grepl("mtcars", tabs_html, ignore.case = TRUE))
  expect_true(grepl("iris", tabs_html, ignore.case = TRUE))

  # Should also have import tab
  expect_true(grepl("import_tab", tabs_html))

  app$stop()
})

test_that("Tab close button works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- AppDriver$new(function() {
    dataviewer(mtcars, iris)
  })
  app$wait_for_idle()

  # Get initial tabs
  initial_tabs <- app$get_html("#opt")
  expect_true(grepl("mtcars", initial_tabs, ignore.case = TRUE))

  # Click close button on mtcars tab
  # The close button has onclick that triggers 'close_tab' input
  mtcars_tab_id <- app$get_value(input = "opt")

  # Trigger the close by simulating the onclick event
  app$run_js(sprintf(
    "Shiny.setInputValue('close_tab', '%s', {priority: 'event'})",
    mtcars_tab_id
  ))
  app$wait_for_idle()

  # Should switch to import tab
  current_tab <- app$get_value(input = "opt")
  expect_equal(current_tab, "import_tab")

  # mtcars tab should be gone
  remaining_tabs <- app$get_html("#opt")
  expect_false(grepl(mtcars_tab_id, remaining_tabs))

  app$stop()
})

test_that("Invalid filter shows error notification", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- AppDriver$new(app_driver_wrapper())
  app$wait_for_idle()

  active_tab <- app$get_value(input = "opt")
  ns_prefix <- active_tab

  # Set invalid filter
  filter_input <- paste0(ns_prefix, "-filter")
  app$set_inputs(!!rlang::sym(filter_input) := "invalid syntax >>>")

  submit_button <- paste0(ns_prefix, "-submit")
  app$click(submit_button)
  app$wait_for_idle()

  # Check that notification appears
  notification_exists <- app$get_js("$('.shiny-notification').length > 0")
  expect_true(notification_exists)

  # Data should remain unchanged (32 rows)
  dt_info <- app$get_js(sprintf(
    "$('#%s-tbl').DataTable().page.info().recordsDisplay",
    ns_prefix
  ))
  expect_equal(dt_info, 32)

  app$stop()
})

test_that("App handles large dataset efficiently", {
  skip_on_cran()

  large_data <- data.frame(
    x = rnorm(10000),
    y = rnorm(10000),
    category = sample(letters, 10000, replace = TRUE)
  )

  app <- AppDriver$new(function() {
    dataviewer(large_data)
  })

  # Should load within reasonable time
  expect_true(app$wait_for_idle(timeout = 5000))

  app$stop()
})
