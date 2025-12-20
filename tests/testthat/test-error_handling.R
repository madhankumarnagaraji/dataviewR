library(testthat)

test_that("stop_dataviewer handles invalid IDs", {
  # Ensure clean state
  stop_all_dataviewers()

  # 1. Try to stop a specific ID that doesn't exist
  expect_error(
    stop_dataviewer("ghost_process"),
    "Process 'ghost_process' not found"
  )

  # 2. Try to stop "most recent" when NONE are running
  expect_message(
    stop_dataviewer(),
    "No background dataviewer processes are running"
  )
})

test_that("list_dataviewers handles empty state", {
  stop_all_dataviewers()

  # Should just print a message and return invisible NULL
  expect_message(
    list_dataviewers(),
    "No background dataviewer processes are running"
  )

  # Capture output to ensure no table header is printed
  output <- capture.output(list_dataviewers())
  expect_false(any(grepl("Active background dataviewer processes", output)))
})
