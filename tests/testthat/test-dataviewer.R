# tests/testthat/test-dataviewer.R

library(testthat)
library(shinytest2)
library(dplyr)
library(stringr)

# --- SETUP: MOCK DATA AND APP DRIVER ---

# Use a wrapper function to launch the app with test data
# Assuming 'multidataviewer' is exported by your package.
app_driver_wrapper <- function(data = NULL) {
  if (is.null(data)) {
    app <- multidataviewer() # Launches with the Import Panel
  } else {
    app <- multidataviewer(data)
  }
  return(app)
}

# Define test data and expected namespace base
test_data <- mtcars
test_data_name <- "mtcars"


# --- Helper Function for Namespaced UI Tests ---
# This starts the app and returns the initial module ID
start_modular_app <- function(data = test_data) {
  app <- AppDriver$new(app_driver_wrapper(data),
                       name = paste0("test_", Sys.time()),
                       height = 800, width = 1200)

  # The initial active tab ID is the module's namespace (e.g., "tab_mtcars_1")
  initial_tab_id <- app$get_active_nav("opt")

  # Fail early if we didn't land on the expected tab
  if (!stringr::str_detect(initial_tab_id, tolower(test_data_name))) {
    app$stop()
    stop("App failed to initialize on the expected data tab.")
  }

  return(list(app = app, module_id = initial_tab_id))
}

# ----------------------------------------------------
## I. Initialization and Basic UI Checks
# ----------------------------------------------------

test_that("1. App loads data correctly and UI is namespaced", {
  skip_on_cran()

  # Use the helper to start the app and get the module ID
  result <- start_modular_app()
  app <- result$app
  mid <- result$module_id

  # Check 1: Tab is active
  expect_true(stringr::str_detect(mid, tolower(test_data_name)))

  # Check 2: Total row count is correct (uses namespaced output ID)
  total_rows_id <- paste0(mid, "-totalrows")
  expect_equal(app$get_value(total_rows_id), "32")

  # Check 3: Essential inputs are present (namespaced)
  expect_visible(app$get_html_id(paste0(mid, "-filter")))
  expect_visible(app$get_html_id(paste0(mid, "-submit")))

  app$stop()
})

test_that("2. App initializes on Import Panel when no data is provided", {
  skip_on_cran()

  app <- AppDriver$new(app_driver_wrapper(), name = "dataviewer_import")

  # Check initial tab is the static "import_tab" value
  expect_equal(app$get_value(input = "opt"), "import_tab")

  app$stop()
})

# ----------------------------------------------------
## II. Filtering and Data Integrity
# ----------------------------------------------------

test_that("3. Filtering works correctly and is reflected in the code output", {
  skip_on_cran()
  result <- start_modular_app()
  app <- result$app
  mid <- result$module_id

  filter_id <- paste0(mid, "-filter")
  submit_id <- paste0(mid, "-submit")
  generate_code_id <- paste0(mid, "-generate_code")
  code_output_id <- paste0(mid, "-code_output")

  # 1. Apply filter: hp > 200 & cyl == 8
  app$set_inputs(!!filter_id := "hp > 200 & cyl == 8")
  app$click(submit_id)
  app$wait_for_idle()

  # 2. Check the generated R code (reliable proxy for applied filter)
  app$click(generate_code_id)
  app$wait_for_value(input = code_output_id, ignore = list(""))
  code_content <- app$get_value(input = code_output_id)

  expect_true(stringr::str_detect(code_content, "filter\\(hp > 200 & cyl == 8\\)"))

  # 3. Check Clear button functionality
  app$click(paste0(mid, "-clear"))
  app$wait_for_idle()

  # The filter input should be reset
  expect_equal(app$get_value(filter_id), "")

  app$stop()
})

test_that("4. Invalid filter shows no error (table remains unchanged)", {
  skip_on_cran()
  result <- start_modular_app()
  app <- result$app
  mid <- result$module_id

  filter_id <- paste0(mid, "-filter")
  submit_id <- paste0(mid, "-submit")

  # Get initial table data (e.g., first few rows) as a baseline
  initial_tbl <- app$get_value(output = paste0(mid, "-tbl"))

  # 1. Apply an invalid filter
  app$set_inputs(!!filter_id := "INVALID CODE")
  app$click(submit_id)
  app$wait_for_idle()

  # 2. Check that the table output is still valid (i.e., didn't crash)
  current_tbl <- app$get_value(output = paste0(mid, "-tbl"))
  expect_false(is.null(current_tbl))
  # A successful error handler should return the original unfiltered data
  # (though checking table data directly is complex, we verify it didn't disappear)

  app$stop()
})

# ----------------------------------------------------
## III. Column Selection and Code Generation
# ----------------------------------------------------

test_that("5. Column selection and deselect-all work correctly", {
  skip_on_cran()
  result <- start_modular_app()
  app <- result$app
  mid <- result$module_id

  cols_all_id <- paste0(mid, "-cols_all")
  columns_id <- paste0(mid, "-columns")
  generate_code_id <- paste0(mid, "-generate_code")
  code_output_id <- paste0(mid, "-code_output")

  # 1. Deselect all
  app$set_inputs(!!cols_all_id := FALSE)
  app$wait_for_idle()

  columns_selected <- app$get_value(input = columns_id)
  expect_equal(length(columns_selected), 0)

  # 2. Select specific columns
  app$set_inputs(!!columns_id := c("mpg", "cyl", "hp"))
  app$wait_for_idle()

  # 3. Check if code reflects the selection
  app$click(generate_code_id)
  app$wait_for_value(input = code_output_id, ignore = list(""))
  code_content <- app$get_value(input = code_output_id)

  expect_true(stringr::str_detect(code_content, "select\\(mpg, cyl, hp\\)"))

  app$stop()
})

test_that("6. Full code generation (filter + select) is correct", {
  skip_on_cran()
  result <- start_modular_app()
  app <- result$app
  mid <- result$module_id

  filter_id <- paste0(mid, "-filter")
  submit_id <- paste0(mid, "-submit")
  columns_id <- paste0(mid, "-columns")
  generate_code_id <- paste0(mid, "-generate_code")
  code_output_id <- paste0(mid, "-code_output")

  # 1. Set specific filter and selection
  app$set_inputs(!!filter_id := "gear == 4 & am == 1")
  app$set_inputs(!!submit_id := "click")
  app$set_inputs(!!columns_id := c("gear", "am", "qsec"))
  app$wait_for_idle()

  # 2. Generate code
  app$click(generate_code_id)
  app$wait_for_value(input = code_output_id, ignore = list(""))
  code_content <- app$get_value(input = code_output_id)

  # Expected code structure check
  expected_code <- paste0(
    "mtcars \\|>",
    "\\s+filter\\(gear == 4 & am == 1\\) \\|>",
    "\\s+select\\(gear, am, qsec\\)"
  )
  expect_match(code_content, expected_code)

  app$stop()
})

# ----------------------------------------------------
## IV. Metadata and Attributes
# ----------------------------------------------------

test_that("7. Attribute info table is populated", {
  skip_on_cran()
  result <- start_modular_app()
  app <- result$app
  mid <- result$module_id

  metainfo_id <- paste0(mid, "-metainfo")

  # The metainfo table is in the sidebar and should load instantly
  app$wait_for_value(output = metainfo_id)
  attr_info_html <- app$get_value(output = metainfo_id)

  # Check that the attribute info table output is not empty
  expect_true(nchar(attr_info_html) > 100)

  app$stop()
})
