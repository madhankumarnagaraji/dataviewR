library(testthat)
library(shinytest2)
library(dataviewR)

test_that("Multiple datasets create separate tabs", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_multiple()
  app$wait_for_idle()

  tabs_html <- app$get_html("#opt")

  # Should have both dataset tabs
  expect_match(tabs_html, "mtcars", ignore.case = TRUE)
  expect_match(tabs_html, "iris", ignore.case = TRUE)
  expect_match(tabs_html, "Import Dataset")

  app$stop()
})

test_that("Can switch between tabs", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_multiple()
  app$wait_for_idle()

  # Get initial tab (should be first data tab)
  initial_tab <- app$get_value(input = "opt")
  expect_match(initial_tab, "^tab_")

  # Switch to import tab
  app$set_inputs(opt = "import_tab")
  app$wait_for_idle()

  current_tab <- app$get_value(input = "opt")
  expect_equal(current_tab, "import_tab")

  app$stop()
})

test_that("Close button removes tab", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_multiple()
  app$wait_for_idle()

  # Get the active tab
  tab_to_close <- app$get_value(input = "opt")

  # Trigger close
  app$run_js(sprintf(
    "Shiny.setInputValue('close_tab', '%s', {priority: 'event'})",
    tab_to_close
  ))
  app$wait_for_idle()

  # Should switch to import tab
  current_tab <- app$get_value(input = "opt")
  expect_equal(current_tab, "import_tab")

  # Closed tab should not exist in HTML
  tabs_html <- app$get_html("#opt")
  expect_false(grepl(tab_to_close, tabs_html))

  app$stop()
})

test_that("Import same dataset switches to existing tab", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  # Get the initial mtcars tab ID
  initial_tab <- app$get_value(input = "opt")

  # Switch to import tab
  app$set_inputs(opt = "import_tab")
  app$wait_for_idle()

  # Try to import mtcars again
  # Select mtcars from the environment
  env_objects <- app$get_value(output = "myid-envobjects")

  if (!is.null(env_objects) && "mtcars" %in% names(env_objects)) {
    app$set_inputs(`myid-data` = "mtcars")
    app$click("myid-confirm")
    app$wait_for_idle()

    # Should switch back to existing mtcars tab
    current_tab <- app$get_value(input = "opt")
    expect_equal(current_tab, initial_tab)

    # Check for notification
    notification_exists <- app$get_js("$('.shiny-notification').length > 0")
    expect_true(notification_exists)
  }

  app$stop()
})
