#' Stop All Background Dataviewer Processes
#'
#' @export
stop_all_dataviewers <- function() {
  if (length(.dataviewer_env$processes) == 0) {
    message("No background dataviewer processes are running.")
    return(invisible(NULL))
  }

  count <- 0
  for (id in names(.dataviewer_env$processes)) {
    proc_info <- .dataviewer_env$processes[[id]]
    proc <- proc_info$process

    if (proc$is_alive()) {
      proc$kill()
      count <- count + 1
    }
  }

  # Clear all processes
  .dataviewer_env$processes <- list()

  message("Stopped ", count, " dataviewer process", if (count != 1) "es" else "")
  invisible(NULL)
}
