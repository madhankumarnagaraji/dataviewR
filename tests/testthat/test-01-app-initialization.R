library(testthat)
library(shinytest2)
library(dataviewR)

test_that("App starts with single dataset", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  # Verify mtcars tab exists and is active
  active_tab <- app$get_value(input = "opt")
  expect_match(active_tab, "^tab_mtcars")

  # Verify import tab also exists
  tabs_html <- app$get_html("#opt")
  expect_match(tabs_html, "Import Dataset")

  app$stop()
})

test_that("App starts in import mode when no data provided", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_import()
  app$wait_for_idle()

  # Should start on import tab
  active_tab <- app$get_value(input = "opt")
  expect_equal(active_tab, "import_tab")

  # Verify import UI exists by checking for the stable 'Import Data' button ID
  import_exists <- app$get_js("$('#myid-confirm').length > 0")
  expect_true(import_exists)

  app$stop()
})

test_that("UI elements are present and properly namespaced", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Check all main UI elements exist
  elements <- c("filter", "submit", "clear", "load", "generate_code",
                "cols_all", "columns", "tbl", "totalrows", "metainfo")

  for (elem in elements) {
    elem_id <- paste0(tab_id, "-", elem)
    elem_exists <- app$get_js(sprintf("$('#%s').length > 0", elem_id))
    expect_true(elem_exists, info = paste("Element", elem, "should exist"))
  }

  app$stop()
})

test_that("Total rows displays correctly", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)
  total_rows <- app$get_value(output = paste0(tab_id, "-totalrows"))

  expect_equal(total_rows, "32")

  app$stop()
})
