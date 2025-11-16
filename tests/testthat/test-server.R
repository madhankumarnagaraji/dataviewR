library(testthat)
library(shiny)
library(dplyr)
library(stringr)

# Load helper function (assuming the server function is available internally)
source("R/dataviewr_tab_server.R")

# --- Setup Test Environment ---
test_data <- mtcars
test_data[1, "mpg"] <- NA # Add NA for filtering tests
test_name <- "mtcars"

# Mock the module server inputs and setup the module
# This helper simulates running the module server
test_module <- function(id, data, name, input_values = list()) {
  shiny::testServer(
    dataviewr_tab_server,
    args = list(
      get_data = reactive(data),
      dataset_name = reactive(name)
    ),
    expr = {
      # Initialize inputs for the first run
      session$setInputs(
        # Default state of the UI when loaded
        columns = names(data),
        filter = "",
        cols_all = TRUE
      )

      # Set specific inputs passed for this test
      for (key in names(input_values)) {
        session$setInputs(!!key := input_values[[key]])
      }

      # Force reactive chain to run (e.g., filter_df)
      session$flush()

      # Return reactive values/outputs needed for testing
      list(
        filter_df = filter_df(),
        final_df = final_df(),
        generated_code = generated_code(),
        columns = input$columns
      )
    }
  )
}

# ----------------------------------------------------
## Test 1: Data Loading and Initialization
# ----------------------------------------------------
test_that("Module loads data and initializes outputs correctly", {
  results <- test_module("test_load", test_data, test_name)

  # Check if data loads
  expect_true(is.data.frame(results$filter_df))
  expect_equal(nrow(results$filter_df), 32)

  # Check if all columns are initially selected
  expect_equal(length(results$columns), 11)
  expect_equal(results$columns, names(test_data))

  # Check initial code generation
  expect_true(stringr::str_detect(results$generated_code, "mtcars"))
  expect_true(stringr::str_detect(results$generated_code, "select\\(mpg, cyl,"))
})

# ----------------------------------------------------
## Test 2: Filtering Logic
# ----------------------------------------------------
test_that("Filtering logic works correctly", {
  # Test with a valid filter condition (cyl > 4 & hp > 100)
  results_filtered <- test_module(
    "test_filter",
    test_data,
    test_name,
    input_values = list(filter = "cyl == 6", submit = 1)
  )
  expect_equal(nrow(results_filtered$filter_df), 7)

  # Test with a clearing filter
  results_cleared <- test_module(
    "test_clear",
    test_data,
    test_name,
    input_values = list(filter = "cyl == 6", submit = 1, clear = 1)
  )
  expect_equal(nrow(results_cleared$filter_df), 32) # Should revert to full size
})

# ----------------------------------------------------
## Test 3: Column Selection Logic
# ----------------------------------------------------
test_that("Column selection and final_df works", {
  # Select only two columns
  results_select <- test_module(
    "test_select",
    test_data,
    test_name,
    input_values = list(columns = c("mpg", "hp"))
  )
  expect_equal(ncol(results_select$final_df), 2)
  expect_equal(names(results_select$final_df), c("mpg", "hp"))
})

# ----------------------------------------------------
## Test 4: Code Generation
# ----------------------------------------------------
test_that("Code generation combines filter and select correctly", {
  results_code <- test_module(
    "test_code",
    test_data,
    test_name,
    input_values = list(
      filter = "gear > 3",
      submit = 1,
      columns = c("gear", "carb")
    )
  )
  expected_code <- paste0(
    "# Generated R Code\n",
    "library(dplyr)\n",
    "mtcars |>\n",
    "  filter(gear > 3) |>\n",
    "  select(gear, carb)"
  )
  expect_equal(results_code$generated_code, expected_code)
})
