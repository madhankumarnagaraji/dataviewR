library(testthat)

test_that("Background Process Lifecycle", {
  # Skip this test on CRAN servers (they don't allow background processes usually)
  skip_on_cran()

  # Ensure clean slate
  stop_all_dataviewers()

  # 1. Start a background viewer
  expect_message(
    pid <- dataviewer(mtcars, background = TRUE),
    "Starting dataviewer in background"
  )

  # Give the background process a moment to register
  Sys.sleep(2)

  expect_false(is.null(pid))
  expect_type(pid, "character")

  # 2. Check internal environment via list_dataviewers
  output <- capture.output(list_dataviewers())
  expect_true(any(grepl("Active background dataviewer processes", output)))
  expect_true(any(grepl(pid, output)))
  expect_true(any(grepl("RUNNING", output)))

  # 3. Stop specific viewer
  expect_message(stop_dataviewer(pid), "Stopped dataviewer process")

  # 4. Verify it is stopped
  output_after <- capture.output(list_dataviewers())
  # Should either say "No background..." or just not list the PID
  if(length(output_after) > 0) {
    expect_false(any(grepl(pid, output_after)))
  } else {
    expect_equal(length(output_after), 0)
  }
})

test_that("Stop All Viewers", {
  skip_on_cran()

  stop_all_dataviewers() # Cleanup

  # Start two viewers
  pid1 <- dataviewer(mtcars, background = TRUE)
  pid2 <- dataviewer(iris, background = TRUE)

  Sys.sleep(2)

  # Verify both running
  output <- capture.output(list_dataviewers())
  expect_true(any(grepl(pid1, output)))
  expect_true(any(grepl(pid2, output)))

  # Stop all
  expect_message(stop_all_dataviewers(), "Stopped 2 dataviewer processes")

  # Verify empty
  expect_message(list_dataviewers(), "No background dataviewer processes are running")
})
