library(testthat)
library(dataviewR)

# Only run these tests if callr is available
skip_if_not_installed("callr")

test_that("dataviewer background mode returns process ID", {
  skip_on_cran()

  # Launch in background mode
  proc_id <- dataviewer(mtcars, background = TRUE, port = 4444)

  # Should return a process ID
  expect_true(!is.null(proc_id))
  expect_true(is.character(proc_id))
  expect_match(proc_id, "^dv_\\d+$")

  # Clean up
  Sys.sleep(0.5)
  stop_dataviewer(proc_id)
})

test_that("background process is stored in environment", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Launch in background mode
  proc_id <- dataviewer(mtcars, background = TRUE, port = 4445)
  Sys.sleep(0.5)

  # Check process is stored
  expect_true(proc_id %in% names(dataviewR:::.dataviewer_env$processes))

  proc_info <- dataviewR:::.dataviewer_env$processes[[proc_id]]
  expect_true(!is.null(proc_info$process))
  expect_true(!is.null(proc_info$started))
  expect_equal(proc_info$data_name, "mtcars")
  expect_equal(proc_info$port, 4445)

  # Clean up
  stop_dataviewer(proc_id)
})

test_that("list_dataviewers shows active processes", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Launch two processes
  proc_id1 <- dataviewer(mtcars, background = TRUE, port = 4446)
  proc_id2 <- dataviewer(iris, background = TRUE, port = 4447)
  Sys.sleep(0.5)

  # Capture output of list_dataviewers
  output <- capture.output(list_dataviewers())

  # Should show both processes
  expect_true(any(grepl(proc_id1, output)))
  expect_true(any(grepl(proc_id2, output)))
  expect_true(any(grepl("mtcars", output)))
  expect_true(any(grepl("iris", output)))

  # Clean up
  stop_all_dataviewers()
})

test_that("stop_dataviewer stops specific process", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Launch a process
  proc_id <- dataviewer(mtcars, background = TRUE, port = 4448)
  Sys.sleep(0.5)

  # Verify it's running
  proc_info <- dataviewR:::.dataviewer_env$processes[[proc_id]]
  expect_true(proc_info$process$is_alive())

  # Stop it
  stop_dataviewer(proc_id)

  # Verify it's removed from the list
  expect_false(proc_id %in% names(dataviewR:::.dataviewer_env$processes))
})

test_that("stop_dataviewer stops most recent when no ID specified", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Launch two processes
  proc_id1 <- dataviewer(mtcars, background = TRUE, port = 4449)
  proc_id2 <- dataviewer(iris, background = TRUE, port = 4450)
  Sys.sleep(0.5)

  # Stop without specifying ID (should stop most recent)
  output <- capture.output(stop_dataviewer(), type = "message")

  # Should mention stopping the most recent one
  expect_true(any(grepl("most recent", output)))
  expect_true(any(grepl(proc_id2, output)))

  # proc_id2 should be gone, proc_id1 should remain
  expect_false(proc_id2 %in% names(dataviewR:::.dataviewer_env$processes))
  expect_true(proc_id1 %in% names(dataviewR:::.dataviewer_env$processes))

  # Clean up
  stop_all_dataviewers()
})

test_that("stop_all_dataviewers stops all processes", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Launch multiple processes
  proc_id1 <- dataviewer(mtcars, background = TRUE, port = 4451)
  proc_id2 <- dataviewer(iris, background = TRUE, port = 4452)
  proc_id3 <- dataviewer(ChickWeight, background = TRUE, port = 4453)
  Sys.sleep(0.5)

  # Verify they're all running
  expect_equal(length(dataviewR:::.dataviewer_env$processes), 3)

  # Stop all
  stop_all_dataviewers()

  # Verify all are removed
  expect_equal(length(dataviewR:::.dataviewer_env$processes), 0)
})

test_that("background mode handles import panel mode", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Launch without data (import mode)
  proc_id <- dataviewer(background = TRUE, port = 4454)
  Sys.sleep(0.5)

  # Check process info
  proc_info <- dataviewR:::.dataviewer_env$processes[[proc_id]]
  expect_null(proc_info$data_name)
  expect_equal(proc_info$port, 4454)

  # Clean up
  stop_dataviewer(proc_id)
})

test_that("error when stopping non-existent process", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Try to stop non-existent process
  expect_error(
    stop_dataviewer("fake_id"),
    regexp = "not found"
  )
})

test_that("message when no processes to list", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Try to list when none exist
  output <- capture.output(list_dataviewers(), type = "message")
  expect_true(any(grepl("No background dataviewer processes", output)))
})

test_that("message when no processes to stop", {
  skip_on_cran()

  # Clear any existing processes
  stop_all_dataviewers()

  # Try to stop when none exist
  output <- capture.output(stop_dataviewer(), type = "message")
  expect_true(any(grepl("No background dataviewer processes", output)))
})

test_that("error when callr not installed", {
  skip_on_cran()

  # Mock the requireNamespace function to return FALSE
  with_mocked_bindings(
    {
      expect_error(
        dataviewer(mtcars, background = TRUE),
        regexp = "Package 'callr' is required"
      )
    },
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})
