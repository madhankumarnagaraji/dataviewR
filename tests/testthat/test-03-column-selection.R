library(testthat)
library(shinytest2)
library(dataviewR)

test_that("Select all checkbox works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # All should be selected initially
  selected <- app$get_value(input = paste0(tab_id, "-columns"))
  expect_equal(length(selected), 11)

  # Deselect all
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-cols_all")) := FALSE)
  app$wait_for_idle()

  selected_after <- app$get_value(input = paste0(tab_id, "-columns"))
  expect_equal(length(selected_after), 0)

  # Re-select all
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-cols_all")) := TRUE)
  app$wait_for_idle()

  selected_final <- app$get_value(input = paste0(tab_id, "-columns"))
  expect_equal(length(selected_final), 11)

  app$stop()
})

test_that("Individual column selection works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Deselect all first
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-cols_all")) := FALSE)
  app$wait_for_idle()

  # Select specific columns
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-columns")) := c("mpg", "cyl", "hp")
  )
  app$wait_for_idle()

  # Verify selection
  selected <- app$get_value(input = paste0(tab_id, "-columns"))
  expect_equal(selected, c("mpg", "cyl", "hp"))

  app$stop()
})

test_that("Column selection affects displayed table", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Select only 2 columns
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-cols_all")) := FALSE)
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-columns")) := c("mpg", "hp")
  )
  app$wait_for_idle(duration = 1000)

  wait_for_datatable(app, tab_id)

  # Check number of columns in DataTable
  col_count <- app$get_js(sprintf(
    "$('#%s-tbl').DataTable().columns().header().length",
    tab_id
  ))

  # Should have 2 columns plus any DT internal columns
  expect_equal(col_count, 2)

  app$stop()
})
