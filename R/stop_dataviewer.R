#' Stop a Background Dataviewer Process
#'
#' @param id Character string specifying the process ID to stop. If NULL, stops the most recent background dataviewer.
#'
#' @export
stop_dataviewer <- function(id = NULL) {
  # If no ID specified and no processes running, just message and return
  if (is.null(id) && length(.dataviewer_env$processes) == 0) {
    message("No background dataviewer processes are running.")
    return(invisible(NULL))
  }

  # If no ID specified, use the most recent process
  if (is.null(id)) {
    id <- names(.dataviewer_env$processes)[length(.dataviewer_env$processes)]
    message("Stopping most recent dataviewer: ", id)
  }

  # Check if process exists - throw error if specific ID not found
  if (!id %in% names(.dataviewer_env$processes)) {
    if (length(.dataviewer_env$processes) == 0) {
      stop("Process '", id, "' not found. No background dataviewer processes are running.")
    } else {
      available <- paste(names(.dataviewer_env$processes), collapse = ", ")
      stop("Process '", id, "' not found. Available processes: ", available)
    }
  }

  # Get the process
  proc_info <- .dataviewer_env$processes[[id]]
  proc <- proc_info$process

  # Kill the process
  if (proc$is_alive()) {
    proc$kill()
    message("Stopped dataviewer process: ", id)
  } else {
    message("Process ", id, " was already stopped.")
  }

  # Remove from list
  .dataviewer_env$processes[[id]] <- NULL

  invisible(NULL)
}
