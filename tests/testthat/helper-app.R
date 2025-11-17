# Shared test utilities
library(shinytest2)
library(dataviewR) # Ensure this is loaded in your helper or test file

#' Create a test app instance with mtcars
app_with_mtcars <- function() {
  shinytest2::AppDriver$new(
    # PASS THE RESULT OF THE FUNCTION CALL DIRECTLY to the 'app' argument
    app = dataviewer(mtcars),
    name = "mtcars_viewer",
    width = 1400,
    height = 900
  )
}

#' Create a test app instance with multiple datasets
app_with_multiple <- function() {
  shinytest2::AppDriver$new(
    # PASS THE RESULT OF THE FUNCTION CALL DIRECTLY
    app = dataviewer(mtcars, iris),
    name = "multi_viewer",
    width = 1400,
    height = 900
  )
}

#' Create a test app instance with no data (import mode)
app_with_import <- function() {
  shinytest2::AppDriver$new(
    # PASS THE RESULT OF THE FUNCTION CALL (no arguments = import mode)
    app = dataviewer(),
    name = "import_viewer",
    width = 1400,
    height = 900
  )
}

#' Get the active data tab ID (excluding import tab)
get_data_tab_id <- function(app) {
  active <- app$get_value(input = "opt")
  if (active == "import_tab") {
    stop("Currently on import tab, not a data tab")
  }
  return(active)
}

#' Wait for DataTable to be fully rendered
wait_for_datatable <- function(app, tab_id, timeout = 5000) {
  app$wait_for_js(
    sprintf(
      "typeof $('#%s-tbl').DataTable === 'function' &&
       $('#%s-tbl').DataTable().page.info() !== undefined",
      tab_id, tab_id
    ),
    timeout = timeout
  )
}

#' Get number of rows in DataTable
get_dt_row_count <- function(app, tab_id) {
  app$get_js(sprintf(
    "$('#%s-tbl').DataTable().page.info().recordsDisplay",
    tab_id
  ))
}

#' Wait for DataTable's displayed row count to reach a specific value
wait_for_row_count_to_be <- function(app, tab_id, expected_count, timeout = 5000) {
  # JavaScript to check the current displayed row count
  js_check <- sprintf(
    "$('#%s-tbl').DataTable().page.info().recordsDisplay == %d",
    tab_id, expected_count
  )
  app$wait_for_js(js_check, timeout = timeout)
}
