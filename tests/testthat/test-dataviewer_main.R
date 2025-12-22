library(testthat)

test_that("Input Validation", {
  # FIX: Must set background = FALSE so the error happens in the main thread
  # Otherwise, the background process crashes silently and the main function returns a PID
  expect_error(
    dataviewer("not a dataframe", background = FALSE),
    "All arguments must be tibbles or data.frames"
  )

  expect_error(
    dataviewer(mtcars, "string", background = FALSE),
    "All arguments must be tibbles or data.frames"
  )
})

test_that("Foreground Mode Returns Shiny App", {
  # When background = FALSE, it should return a shiny.appobj
  app <- dataviewer(mtcars, background = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("Zero arguments handling", {
  # If no arguments, it naturally defaults to foreground mode
  expect_message(
    app <- dataviewer(background = TRUE), # User tries background, but no data forces foreground
    "Background mode cannot access Global Environment"
  )
  expect_s3_class(app, "shiny.appobj")
})
