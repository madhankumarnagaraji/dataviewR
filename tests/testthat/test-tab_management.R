library(testthat)
library(shinytest2)
library(dataviewR)

test_that("App Integration: Tabs load and close correctly", {
  skip_on_cran()

  # 1. Launch app with two datasets
  app <- dataviewer(mtcars, iris, background = FALSE)

  # Initialize the test driver
  app_driver <- AppDriver$new(app, name = "tab_test", height = 800, width = 1200, timeout = 15000)

  # Wait for the app to settle
  app_driver$wait_for_idle()

  # 2. Verify Tabs Exist
  values <- app_driver$get_values()
  input_names <- names(values$input)

  expect_true(any(grepl("tab_mtcars_.*-filter", input_names)))
  expect_true(any(grepl("tab_iris_.*-filter", input_names)))

  # 3. Test Tab Closing Logic
  # Identify the tab ID for iris
  iris_input_name <- grep("tab_iris_", names(values$input), value = TRUE)[1]
  iris_tab_id <- sub("-.*", "", iris_input_name)

  # Send the close signal
  app_driver$set_inputs(
    close_tab = iris_tab_id,
    priority_ = "event",
    allow_no_input_binding_ = TRUE
  )

  # Wait for the server to switch the active tab to mtcars
  # This confirms the removeTab logic has completed on the server
  app_driver$wait_for_value(
    input = "opt",
    ignore = list(iris_tab_id),
    timeout = 5000
  )

  # 4. Verify Switch Logic
  values_after <- app_driver$get_values()
  current_tab <- values_after$input$opt

  # The app should have switched back to the mtcars tab
  expect_match(current_tab, "tab_mtcars_")

  # 5. Verify UI Removal (The Robust Check)
  # Instead of checking input names (which persist), we check if the DOM element exists.
  # We use a JavaScript snippet to ask: "Is the element with ID 'iris_tab_id' null?"
  # Note: The tab content div usually has the ID matching the value of the tabPanel.

  is_tab_gone <- app_driver$get_js(sprintf("document.getElementById('%s') === null", iris_tab_id))
  expect_true(is_tab_gone)

  app_driver$stop()
})

test_that("App Integration: Import Panel Trigger", {
  skip_on_cran()

  # Launch with NO data -> Should open Import Panel
  app <- dataviewer(background = FALSE)
  app_driver <- AppDriver$new(app)
  app_driver$wait_for_idle()

  # Verify we are on the import tab
  values <- app_driver$get_values()
  expect_equal(values$input$opt, "import_tab")

  app_driver$stop()
})
