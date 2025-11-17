library(testthat)
library(shinytest2)
library(dataviewR)

test_that("Code generation button opens modal", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Click generate code button
  app$click(paste0(tab_id, "-generate_code"))
  app$wait_for_idle()

  # Check modal appeared
  modal_exists <- app$get_js("$('.modal').is(':visible')")
  expect_true(modal_exists)

  app$stop()
})

test_that("Generated code includes library and dataset name", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  app$click(paste0(tab_id, "-generate_code"))
  app$wait_for_idle()

  # Get code content
  code_content <- app$get_js(sprintf(
    "$('#%s-code_output').val()",
    tab_id
  ))

  expect_match(code_content, "library\\(dplyr\\)")
  expect_match(code_content, "mtcars")
  expect_match(code_content, "\\|>")

  app$stop()
})

test_that("Generated code includes filter when applied", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Apply filter
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-filter")) := "mpg > 20 & cyl == 4"
  )
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle()

  # Generate code
  app$click(paste0(tab_id, "-generate_code"))
  app$wait_for_idle()

  code_content <- app$get_js(sprintf(
    "$('#%s-code_output').val()",
    tab_id
  ))

  expect_match(code_content, "filter\\(mpg > 20 & cyl == 4\\)")

  app$stop()
})

test_that("Generated code includes select when columns chosen", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Select specific columns
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-cols_all")) := FALSE)
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-columns")) := c("mpg", "cyl", "hp")
  )
  app$wait_for_idle()

  # Generate code
  app$click(paste0(tab_id, "-generate_code"))
  app$wait_for_idle()

  code_content <- app$get_js(sprintf(
    "$('#%s-code_output').val()",
    tab_id
  ))

  expect_match(code_content, "select\\(")
  expect_match(code_content, "mpg")
  expect_match(code_content, "cyl")
  expect_match(code_content, "hp")

  app$stop()
})

test_that("Generated code combines filter and select correctly", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Apply both filter and column selection
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-filter")) := "gear == 4"
  )
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle()

  app$set_inputs(!!rlang::sym(paste0(tab_id, "-cols_all")) := FALSE)
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-columns")) := c("gear", "mpg")
  )
  app$wait_for_idle()

  # Generate code
  app$click(paste0(tab_id, "-generate_code"))
  app$wait_for_idle()

  code_content <- app$get_js(sprintf(
    "$('#%s-code_output').val()",
    tab_id
  ))

  # Should have both operations piped
  expect_match(code_content, "filter\\(gear == 4\\).*\\|>.*select\\(")

  app$stop()
})

test_that("Copy button works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Open code modal
  app$click(paste0(tab_id, "-generate_code"))
  app$wait_for_idle()

  # Click copy button (just verify it exists and doesn't error)
  copy_btn_exists <- app$get_js(sprintf(
    "$('#%s-copy_btn').length > 0",
    tab_id
  ))
  expect_true(copy_btn_exists)

  # Note: Actually testing clipboard is difficult in automated tests
  # We just verify the button can be clicked without error
  app$click(paste0(tab_id, "-copy_btn"))
  app$wait_for_idle()

  app$stop()
})
