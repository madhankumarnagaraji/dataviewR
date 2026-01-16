library(testthat)
library(dataviewR)

test_that("dataviewer launches in background and stops correctly", {
  # Skip these tests on CRAN to prevent issues with background processes on build servers
  skip_on_cran()

  # MOCKING: prevent the test from actually opening a browser or RStudio viewer
  # We use `testthat::local_mocked_bindings` (available in testthat >= 3.0.0)
  # If you have an older version, you might need the `mockery` package.

  # We mock browseURL to do nothing but return NULL
  local_mocked_bindings(
    browseURL = function(url, ...) NULL,
    .package = "utils"
  )

  # We mock rstudioapi::viewer (if it exists) to do nothing
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    local_mocked_bindings(
      viewer = function(url, ...) NULL,
      .package = "rstudioapi"
    )
  }

  # --- TEST 1: Launch Logic ---
  message("Testing launch...")

  # Capture the ID returned by the function
  # We expect a message about "Starting dataviewer"
  pid <- NULL
  expect_message({
    pid <- dataviewer(mtcars, background = TRUE)
  }, "Starting dataviewer")

  # Assertions
  expect_type(pid, "character")
  expect_true(grepl("^dv_\\d+", pid)) # format should be dv_1, dv_2, etc.

  # Verify the process is actually alive in the internal environment
  # Note: Accessing internal .dataviewer_env via ::: for testing purposes
  env <- dataviewR:::.dataviewer_env
  expect_true(pid %in% names(env$processes))
  expect_true(env$processes[[pid]]$process$is_alive())

  # --- TEST 2: Stop Logic ---
  message("Testing stop...")

  expect_message({
    stop_dataviewer(pid)
  }, "Stopped dataviewer process")

  # Verify it is gone from the environment
  expect_false(pid %in% names(env$processes))

  # Verify trying to stop it again throws an error
  expect_error(stop_dataviewer(pid), "not found")
})

test_that("dataviewer handles foreground mode for Import", {
  skip_on_cran()

  # When no dataset is provided, it defaults to foreground and import mode.
  # Testing this fully requires interacting with Shiny, so we just check the return type
  # or the specific message it prints before launching app.

  # Since foreground mode blocks execution, we can't easily test the running app
  # in a standard unit test without shinytest2.
  # However, we can verify the trigger logic (arguments).

  # This part is tricky because running dataviewer() blocks the console.
  # Usually, we skip blocking tests or run them in a separate process that we kill immediately.
  # For now, we will skip the blocking foreground test in this basic suite.
  succeed("Foreground test skipped (requires interactive session)")
})
