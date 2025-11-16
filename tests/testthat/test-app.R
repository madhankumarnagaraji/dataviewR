library(testthat)
library(shinytest2)
library(dplyr)
library(mtcars) # Ensure data is available

# Wrap the app code in a helper function to make it testable
app_driver_wrapper <- function() {
  # Since your multidataviewer() calls shinyApp, we call it directly
  app <- multidataviewer(mtcars)
  return(app)
}

test_that("App starts and basic elements are present", {
  # Start the app using the wrapper function
  app <- AppDriver$new(app_driver_wrapper)

  # Check if the initial dataset tab is present and selected
  initial_tab_id <- app$get_active_nav("opt")
  expect_true(stringr::str_detect(initial_tab_id, "tab_mtcars"))

  # Check for core namespaced elements in the active tab (mtcars tab)
  # The input ID will be something like tab_mtcars_1-filter
  filter_input_id <- paste0(initial_tab_id, "-filter")
  submit_button_id <- paste0(initial_tab_id, "-submit")
  table_output_id <- paste0(initial_tab_id, "-tbl")

  expect_visible(app$get_html_id(filter_input_id))
  expect_visible(app$get_html_id(submit_button_id))
  expect_visible(app$get_html_id(table_output_id))

  # Check if the total row count is correct
  total_rows_id <- paste0(initial_tab_id, "-totalrows")
  expect_equal(app$get_value(total_rows_id), "32")

  app$stop()
})

test_that("Filter and Submit button interaction works (UI/JS integration)", {
  app <- AppDriver$new(app_driver_wrapper)

  initial_tab_id <- app$get_active_nav("opt")
  filter_input_id <- paste0(initial_tab_id, "-filter")
  submit_button_id <- paste0(initial_tab_id, "-submit")

  # 1. Enter a filter condition
  app$set_inputs(!!filter_input_id := "cyl == 4")

  # 2. Click submit button
  app$click(submit_button_id)

  # Wait for reactives to settle and DT to update (using a common shiny output)
  app$wait_for_value(output = paste0(initial_tab_id, "-totalrows"))

  # The total rows output shows the full dataset count (32), but we can check the table info
  # We check the generated code as a reliable proxy for the filter being applied.
  code_output_id <- paste0(initial_tab_id, "-generate_code")

  # Open the modal
  app$click(code_output_id)

  # Check the code in the modal textarea (ID is namespaced)
  modal_code_id <- paste0(initial_tab_id, "-code_output")

  # Check if the code includes the filter
  code_content <- app$get_value(modal_code_id, selector = "textarea")
  expect_true(stringr::str_detect(code_content, "filter\\(cyl == 4\\)"))

  app$stop()
})
