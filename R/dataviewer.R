# Package environment to store background processes
.dataviewer_env <- new.env(parent = emptyenv())
.dataviewer_env$processes <- list()
.dataviewer_env$counter <- 0L

# Define global variables to satisfy R CMD check
utils::globalVariables(c("att", "col_name", "col_type", "colname", "pos", "value", ".data"))

#' Interactive Data Viewer with Filter and Code Generation
#'
#' Launches a Shiny application to explore and filter a `data.frame` or `tibble`.
#' If no data is provided, it opens an import panel to load a dataset from either the global environment or the packages.
#'
#' @param ... One or more `data.frame` or `tibble` objects. If none provided, an import UI is shown to load data interactively.
#' @param background Logical. If `TRUE`, runs the app in a background R process using `callr`. Requires the `callr` package. Default is TRUE when dataset(s) are provided, FALSE when no dataset(s) are provided.
#' @param port Integer. Port number for the Shiny app. If `NULL`, a random available port is used. Default is NULL.
#'
#' @return
#' If `background = TRUE`, returns the process ID (character) invisibly.
#' If `background = FALSE`, returns a Shiny application object.
#'
#' @details
#' This function provides:
#' \itemize{
#'     \item A tab-based interface with data import and viewer options.
#'     \item Support for multiple datasets in separate tabs.
#'     \item A checkbox panel to select/deselect columns.
#'     \item An input for `dplyr`-compatible filter expressions.
#'     \item A dynamically generated `dplyr` code preview.
#'     \item Metadata display for the variables.
#' }
#'
#' The filtering uses `dplyr::filter()` and generates user-friendly code to replicate the steps.
#' It also provides copyable R code that includes column selection and filtering logic.
#'
#' @importFrom shiny fluidPage tabsetPanel tabPanel actionButton textInput updateTextInput checkboxInput checkboxGroupInput updateCheckboxGroupInput sidebarLayout sidebarPanel mainPanel renderTable tableOutput tags showModal modalDialog modalButton observeEvent updateTabsetPanel reactive reactiveVal req br removeTab appendTab NS moduleServer reactiveValues
#' @importFrom DT datatable renderDT dataTableOutput DTOutput
#' @importFrom shinyjs useShinyjs runjs
#' @import dplyr
#' @import stringr
#' @import labelled
#' @import forcats
#' @import datamods
#' @importFrom purrr map imap_dfr map_lgl
#' @importFrom tibble enframe tibble
#' @import htmlwidgets
#' @importFrom stats setNames
#' @importFrom utils globalVariables
#'
#' @examples
#' if (interactive()) {
#'     dataviewer(mtcars) # Opens in RStudio Viewer pane or default web browser
#'     dataviewer(iris, mtcars) # Opens multiple datasets in separate tabs
#'     dataviewer() # Opens the "Import Dataset" tab (foreground mode - console will be busy)
#'
#'     # Run in foreground to enable the "Import Dataset" tab alongside 'mtcars'
#'     dataviewer(mtcars, background = FALSE)
#'
#'     # Stop background process
#'     id <- dataviewer(mtcars, iris)
#'     stop_dataviewer(id)
#' }
#'
#' @export

dataviewer <- function(..., background = NULL, port = NULL) {

  # Capture datasets
  datasets <- list(...)
  dataset_names <- as.character(substitute(list(...)))[-1]

  # RULE: If no datasets provided, force background = FALSE
  if (length(datasets) == 0) {
    if (!is.null(background) && background == TRUE) {
      message("Note: Background mode cannot access Global Environment for import.")
    }
    background <- FALSE
    message("Using foreground mode.")
  } else {
    # If datasets provided and background not specified, default to TRUE
    if (is.null(background)) {
      background <- TRUE
    }
  }

  # Determine if import panel should be shown
  # Show import panel only when: background=FALSE OR no datasets provided
  show_import_panel <- !background

  # If background mode, launch in separate process
  if (background) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop("Package 'callr' is required for background mode. Install with: install.packages('callr')")
    }

    # Generate unique ID for this process
    .dataviewer_env$counter <- .dataviewer_env$counter + 1L
    proc_id <- paste0("dv_", .dataviewer_env$counter)

    # Generate a port if not specified
    if (is.null(port)) {
      port <- sample(3000:8000, 1)
    }

    message("Starting dataviewer in background on port ", port, "...")

    # Launch in background WITHOUT opening browser
    # IMPORTANT: Pass the entire app creation code as a function
    proc <- callr::r_bg(
      func = function(datasets, dataset_names, port, show_import_panel) {
        # Load required packages in background session
        library(dataviewR)

        # Calling the internal functions so the background process can see them
        dataviewer_ui_head    <- utils::getFromNamespace("dataviewer_ui_head", "dataviewR")
        dataviewer_tab_ui     <- utils::getFromNamespace("dataviewer_tab_ui", "dataviewR")
        dataviewer_tab_server <- utils::getFromNamespace("dataviewer_tab_server", "dataviewR")

        # Determine trigger mode
        if (length(datasets) == 0) {
          cat("\033[34mNote: Showing the Import Dataset Panel because no datasets were provided\033[0m\n")
          trigger <- 1
          initial_datasets <- list()
          initial_names <- character()
        } else {
          valid_data <- sapply(datasets, function(d) any(class(d) %in% c("tbl_df", "tbl", "data.frame")))
          if (!all(valid_data)) {
            stop("All arguments must be tibbles or data.frames")
          }
          cat("\033[34mNote:", length(datasets), "dataset(s) provided\033[0m\n")
          trigger <- 2
          initial_datasets <- datasets
          initial_names <- dataset_names
        }

        app <- shiny::shinyApp(
          ui = shiny::fluidPage(
            class = "full-width",
            shinyjs::useShinyjs(),
            dataviewer_ui_head(),
            shiny::tabsetPanel(
              id = "opt",
              # Conditionally show Import Dataset panel
              if (show_import_panel) {
                shiny::tabPanel(
                  "Import Dataset",
                  value = "import_tab",
                  shiny::fluidRow(datamods::import_globalenv_ui("myid"))
                )
              }
            )
          ),
          server = function(input, output, session) {
            dataset_store <- shiny::reactiveValues()
            tab_counter <- shiny::reactiveVal(0)

            create_tab_id <- function(name) {
              paste0("tab_", tolower(gsub("[^a-zA-Z0-9]", "_", name)), "_", tab_counter())
            }

            # FIXED: create_viewer_tab with local() to capture tab_id correctly
            create_viewer_tab <- function(tab_id, dataset_name, dataset, show_close_btn = TRUE) {
              tab_title <- if (show_close_btn) {
                shiny::tagList(
                  tolower(dataset_name),
                  shiny::tags$span(
                    class = "close-tab-btn",
                    onclick = sprintf("Shiny.setInputValue('close_tab', '%s', {priority: 'event'})", tab_id),
                    "x"
                  )
                )
              } else {
                tolower(dataset_name)
              }

              shiny::appendTab(
                inputId = "opt",
                shiny::tabPanel(
                  title = tab_title,
                  value = tab_id,
                  dataviewer_tab_ui(tab_id)
                ),
                select = TRUE
              )

              dataset_store[[tab_id]] <- list(
                data = dataset,
                name = dataset_name
              )

              # FIX: Use local() to capture tab_id correctly
              local({
                current_tab_id <- tab_id
                dataviewer_tab_server(
                  id = current_tab_id,
                  get_data = shiny::reactive(dataset_store[[current_tab_id]]$data),
                  dataset_name = shiny::reactive(dataset_store[[current_tab_id]]$name)
                )
              })
            }

            if (trigger == 2) {
              session$onFlushed(function() {
                for (i in seq_along(initial_datasets)) {
                  shiny::isolate({
                    tab_counter(tab_counter() + 1)
                    tab_id <- create_tab_id(initial_names[i])
                    create_viewer_tab(tab_id, initial_names[i], initial_datasets[[i]], show_close_btn = TRUE)
                  })
                }
              }, once = TRUE)
            }

            # Only set up import handlers if import panel is shown
            if (show_import_panel) {
              imported <- datamods::import_globalenv_server("myid", btn_show_data = FALSE)

              shiny::observeEvent(input$`myid-confirm`, {
                shiny::req(imported$data())
                dataset_name <- imported$name()
                dataset <- imported$data()

                if (length(names(dataset_store)) > 0) {
                  existing_tabs <- sapply(names(dataset_store), function(tid) {
                    if (is.null(dataset_store[[tid]])) return(FALSE)
                    dataset_store[[tid]]$name == dataset_name
                  }, USE.NAMES = FALSE)
                  existing_tabs <- as.logical(existing_tabs)
                } else {
                  existing_tabs <- logical(0)
                }

                if (length(existing_tabs) > 0 && any(existing_tabs, na.rm = TRUE)) {
                  valid_tab_ids <- names(dataset_store)[!sapply(dataset_store, is.null)]
                  existing_tab_id <- valid_tab_ids[which(existing_tabs)[1]]
                  if(!is.null(existing_tab_id) && !is.na(existing_tab_id)) {
                    shiny::updateTabsetPanel(session, "opt", selected = existing_tab_id)
                    shiny::showNotification(paste("Switched to existing tab:", dataset_name), type = "message")
                  }
                } else {
                  shiny::isolate({
                    tab_counter(tab_counter() + 1)
                    tab_id <- create_tab_id(dataset_name)
                    create_viewer_tab(tab_id, dataset_name, dataset, show_close_btn = TRUE)
                  })
                }
              })
            }

            shiny::observeEvent(input$close_tab, {
              tab_id <- input$close_tab
              shiny::removeTab(inputId = "opt", target = tab_id)
              dataset_store[[tab_id]] <- NULL

              # CHANGED: Switch to first tab instead of always import_tab
              all_tabs <- names(dataset_store)[!sapply(dataset_store, is.null)]
              if (length(all_tabs) > 0) {
                # Switch to first dataset tab
                shiny::updateTabsetPanel(session, "opt", selected = all_tabs[1])
              } else if (show_import_panel) {
                # If no dataset tabs left and import panel exists, go there
                shiny::updateTabsetPanel(session, "opt", selected = "import_tab")
              }
            })
          }
        )

        shiny::runApp(app, port = port, launch.browser = FALSE)
      },
      args = list(datasets = datasets, dataset_names = dataset_names, port = port, show_import_panel = show_import_panel),
      supervise = TRUE,
      stdout = "|",
      stderr = "|"
    )

    # Store process
    .dataviewer_env$processes[[proc_id]] <- list(
      process = proc,
      started = Sys.time(),
      data_names = if (length(dataset_names) > 0) paste(dataset_names, collapse = ", ") else "<import mode>",
      port = port
    )

    # Wait a moment for app to initialize
    Sys.sleep(1.5)

    # Check if process is alive
    if (!proc$is_alive()) {
      error_msg <- tryCatch(proc$read_all_error_lines(), error = function(e) "Could not read error")
      output_msg <- tryCatch(proc$read_all_output_lines(), error = function(e) "Could not read output")
      stop("Failed to start dataviewer in background.\n",
           "Exit status: ", proc$get_exit_status(), "\n",
           "Error output:\n", paste(error_msg, collapse = "\n"), "\n",
           "Standard output:\n", paste(output_msg, collapse = "\n"))
    }

    # Open in RStudio Viewer pane from MAIN process
    url <- paste0("http://127.0.0.1:", port)

    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable() &&
        rstudioapi::hasFun("viewer")) {
      rstudioapi::viewer(url)
      message("Opening in RStudio Viewer pane")
    } else {
      utils::browseURL(url)
      message("Opening in browser")
    }

    message("Dataviewer running in background (ID: ", proc_id, ")")
    message("URL: ", url)
    if (length(dataset_names) > 0) {
      message("Datasets: ", paste(dataset_names, collapse = ", "))
    }
    message("Use stop_dataviewer('", proc_id, "') to stop this viewer")
    message("Use list_dataviewers() to see all running viewers")

    return(invisible(proc_id))
  }

  # Original foreground mode

  # Determine trigger mode
  if (length(datasets) == 0) {
    cat("\033[34mNote: Showing the Import Dataset Panel because no datasets were provided\033[0m\n")
    trigger <- 1  # triggers the import dataset panel
    initial_datasets <- list()
    initial_names <- character()
  } else {
    # Validate all inputs are data.frame/tibble
    valid_data <- sapply(datasets, function(d) any(class(d) %in% c("tbl_df", "tbl", "data.frame")))

    if (!all(valid_data)) {
      stop("All arguments must be tibbles or data.frames")
    }

    cat("\033[34mNote:", length(datasets), "dataset(s) provided\033[0m\n")
    trigger <- 2  # Shows passed dataframes
    initial_datasets <- datasets
    initial_names <- dataset_names
  }

  shiny::shinyApp(
    # --- REFACTORED UI ---
    ui = shiny::fluidPage(
      class = "full-width",
      shinyjs::useShinyjs(),
      dataviewer_ui_head(), # Call UI head helper
      shiny::tabsetPanel(
        id = "opt",
        # Conditionally show Import Dataset panel
        if (show_import_panel) {
          shiny::tabPanel(
            "Import Dataset",
            value = "import_tab",
            shiny::fluidRow(datamods::import_globalenv_ui("myid"))
          )
        }
      )
    ),

    # --- REFACTORED SERVER ---
    server = function(input, output, session) {

      # Reactive values to store dataset information
      dataset_store <- shiny::reactiveValues()
      tab_counter <- shiny::reactiveVal(0)

      # Helper function to create unique tab ID
      create_tab_id <- function(name) {
        paste0("tab_", tolower(gsub("[^a-zA-Z0-9]", "_", name)), "_", tab_counter())
      }

      # Helper function to create viewer tab
      # FIXED: Uses local() to properly capture tab_id
      create_viewer_tab <- function(tab_id, dataset_name, dataset, show_close_btn = TRUE) {

        # Create tab title with or without close button
        tab_title <- if (show_close_btn) {
          shiny::tagList(
            tolower(dataset_name),
            shiny::tags$span(
              class = "close-tab-btn",
              onclick = sprintf("Shiny.setInputValue('close_tab', '%s', {priority: 'event'})", tab_id),
              "x"
            )
          )
        } else {
          tolower(dataset_name)
        }

        # Append the new tab using the Module UI
        shiny::appendTab(
          inputId = "opt",
          shiny::tabPanel(
            title = tab_title,
            value = tab_id,
            dataviewer_tab_ui(tab_id) # Call Module UI
          ),
          select = TRUE
        )

        # Store dataset in reactive values
        dataset_store[[tab_id]] <- list(
          data = dataset,
          name = dataset_name
        )

        # FIX: Use local() to capture tab_id correctly
        local({
          # Create a local copy of tab_id that won't be affected by loop iterations
          current_tab_id <- tab_id

          # Call the Module Server with properly scoped reactives
          dataviewer_tab_server(
            id = current_tab_id,
            get_data = shiny::reactive(dataset_store[[current_tab_id]]$data),
            dataset_name = shiny::reactive(dataset_store[[current_tab_id]]$name)
          )
        })
      }

      # Initialize tabs with provided datasets (trigger == 2)
      if (trigger == 2) {
        session$onFlushed(function() {
          for (i in seq_along(initial_datasets)) {
            shiny::isolate({
              tab_counter(tab_counter() + 1)
              tab_id <- create_tab_id(initial_names[i])
              create_viewer_tab(tab_id, initial_names[i], initial_datasets[[i]], show_close_btn = TRUE)
            })
          }
        }, once = TRUE)
      }

      # Only set up import handlers if import panel is shown
      if (show_import_panel) {
        imported <- datamods::import_globalenv_server("myid", btn_show_data = FALSE)

        shiny::observeEvent(input$`myid-confirm`, {
          shiny::req(imported$data())

          dataset_name <- imported$name()
          dataset <- imported$data()

          # Check if dataset already exists
          if (length(names(dataset_store)) > 0) {
            existing_tabs <- sapply(names(dataset_store), function(tid) {
              # Check for NULLs which can happen during tab removal
              if (is.null(dataset_store[[tid]])) return(FALSE)
              dataset_store[[tid]]$name == dataset_name
            }, USE.NAMES = FALSE)

            existing_tabs <- as.logical(existing_tabs)
          } else {
            existing_tabs <- logical(0)
          }

          if (length(existing_tabs) > 0 && any(existing_tabs, na.rm = TRUE)) {
            # Switch to existing tab
            valid_tab_ids <- names(dataset_store)[!sapply(dataset_store, is.null)]
            existing_tab_id <- valid_tab_ids[which(existing_tabs)[1]]

            if(!is.null(existing_tab_id) && !is.na(existing_tab_id)) {
              shiny::updateTabsetPanel(session, "opt", selected = existing_tab_id)
              shiny::showNotification(paste("Switched to existing tab:", dataset_name), type = "message")
            }

          } else {
            # Create new tab (always show close button)
            shiny::isolate({
              tab_counter(tab_counter() + 1)
              tab_id <- create_tab_id(dataset_name)
              create_viewer_tab(tab_id, dataset_name, dataset, show_close_btn = TRUE)
            })
          }
        })
      }

      # Handle tab closing
      shiny::observeEvent(input$close_tab, {
        tab_id <- input$close_tab

        # Remove tab
        shiny::removeTab(inputId = "opt", target = tab_id)

        # Remove from storage
        dataset_store[[tab_id]] <- NULL

        # Switch to first tab when a tab has been closed
        all_tabs <- names(dataset_store)[!sapply(dataset_store, is.null)]
        if (length(all_tabs) > 0) {
          # Switch to first dataset tab
          shiny::updateTabsetPanel(session, "opt", selected = all_tabs[1])
        } else if (show_import_panel) {
          # If no dataset tabs left and import panel exists, go there
          shiny::updateTabsetPanel(session, "opt", selected = "import_tab")
        }
      })
    }
  )
}
