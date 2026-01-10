#' List Active Background Dataviewer Processes
#'
#' @seealso For more information, please refer to the \href{https://madhankumarnagaraji.github.io/dataviewR/articles/Exporting-and-Reproducibility.html#to-know-the-session-information}{documentation}.
#'
#' @export
list_dataviewers <- function() {
  if (length(.dataviewer_env$processes) == 0) {
    message("No background dataviewer processes are running.")
    return(invisible(NULL))
  }

  cat("Active background dataviewer processes:\n")
  cat("=======================================\n\n")

  for (id in names(.dataviewer_env$processes)) {
    proc_info <- .dataviewer_env$processes[[id]]
    proc <- proc_info$process

    status <- if (proc$is_alive()) "RUNNING" else "STOPPED"
    data_info <- paste0("Data: ", proc_info$data_names)
    port_info <- if (!is.null(proc_info$port)) {
      paste0("Port: ", proc_info$port)
    } else {
      "Port: auto"
    }
    started_info <- paste0("Started: ", format(proc_info$started, "%Y-%m-%d %H:%M:%S"))

    cat("ID: ", id, "\n", sep = "")
    cat("  Status: ", status, "\n", sep = "")
    cat("  ", data_info, "\n", sep = "")
    cat("  ", port_info, "\n", sep = "")
    cat("  ", started_info, "\n", sep = "")
    cat("\n")
  }

  invisible(NULL)
}
