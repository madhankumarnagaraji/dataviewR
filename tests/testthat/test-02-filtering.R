library(testthat)
library(shinytest2)
library(dataviewR)

test_that("Basic filter works correctly", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Apply filter: cyl == 4
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-filter")) := "cyl == 4")
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle(duration = 2000)

  # 💡 NEW: Aggressive wait using the helper (check for 11 rows)
  # This line assumes the helper function 'wait_for_row_count_to_be' is accessible!
  wait_for_row_count_to_be(app, tab_id, 11)

  # Verify filtered row count (11 cars with 4 cylinders)
  row_count <- get_dt_row_count(app, tab_id)

  # FALLBACK WAIT: If the check is still NULL, wait longer and try again (for extreme timing issues)
  if (is.null(row_count) || row_count == "") {
    Sys.sleep(1) # Wait an additional second
    row_count <- get_dt_row_count(app, tab_id)
  }

  expect_equal(as.numeric(row_count), 11)

  app$stop()
})


test_that("Complex filter with multiple conditions works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Apply complex filter
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-filter")) := "mpg > 20 & cyl == 4"
  )
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle(duration = 2000)

  # 💡 NEW: Wait specifically for 9 rows to appear
  wait_for_row_count_to_be(app, tab_id, 9)

  # Should have 9 rows matching this condition
  row_count <- get_dt_row_count(app, tab_id)
  expect_equal(as.numeric(row_count), 9)

  app$stop()
})

test_that("Clear button resets filter", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Apply filter
  app$set_inputs(!!rlang::sym(paste0(tab_id, "-filter")) := "cyl == 6")
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle(duration = 2000)
  # 💡 NEW: Wait specifically for 7 rows
  wait_for_row_count_to_be(app, tab_id, 7)

  # Verify filtered (7 cars with 6 cylinders)
  expect_equal(as.numeric(get_dt_row_count(app, tab_id)), 7)

  # Click clear
  app$click(paste0(tab_id, "-clear"))
  app$wait_for_idle(duration = 2000)

  # Verify filter input is cleared
  filter_value <- app$get_value(input = paste0(tab_id, "-filter"))
  expect_equal(filter_value, "")

  # 💡 NEW: Wait specifically for the total row count (32) to return
  wait_for_row_count_to_be(app, tab_id, 32)
  # Verify all rows are back
  expect_equal(as.numeric(get_dt_row_count(app, tab_id)), 32)

  app$stop()
})

test_that("Invalid filter shows error notification", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- app_with_mtcars()
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Apply invalid filter
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-filter")) := "invalid ++ syntax"
  )
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle(duration = 2000)

  # Check for notification
  notification_exists <- app$get_js("$('.shiny-notification').length > 0")
  expect_true(notification_exists)

  # Data should remain unchanged (should still be 32)
  wait_for_row_count_to_be(app, tab_id, 32) # Ensure it didn't change
  expect_equal(as.numeric(get_dt_row_count(app, tab_id)), 32)

  app$stop()
})

test_that("Filter with NA values works correctly", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Create data with NAs
  test_data_na <- mtcars
  test_data_na$mpg[1:3] <- NA

  app <- AppDriver$new(app = dataviewer(test_data_na))
  app$wait_for_idle()

  tab_id <- get_data_tab_id(app)

  # Filter to exclude NAs
  app$set_inputs(
    !!rlang::sym(paste0(tab_id, "-filter")) := "!is.na(mpg)"
  )
  app$click(paste0(tab_id, "-submit"))
  app$wait_for_idle(duration = 2000)

  # 💡 NEW: Wait specifically for 29 rows (32 - 3 NAs)
  wait_for_row_count_to_be(app, tab_id, 29)

  # Should have 29 rows (32 - 3 NAs)
  row_count <- get_dt_row_count(app, tab_id)
  expect_equal(as.numeric(row_count), 29)

  app$stop()
})
