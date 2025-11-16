#' Interactive Data Viewer with Filter and Code Generation
#'
#' Launches a Shiny application to explore and filter a 'data.frame' or `tibble`.
#' If no data is provided, it opens an import panel to load a dataset from either the global environment or the packages.
#'
#' @param ... One or more `data.frame` or `tibble` objects. If none provided, an import UI is shown to load data interactively.
#'
#' @return
#' Launches a Shiny application in the browser. Does not return a value.
#'
#' @details
#' This function provides:
#' \itemize{
#'     \item A tab-based interface with data import and viewer options.
#'     \item Support for multiple datasets in separate tabs.
#'     \item A checkbox panel to select/deselect columns.
#'     \item An input for dplyr-compatible filter expressions.
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
#'     multidataviewer(mtcars)
#'     multidataviewer(iris, mtcars) # Multiple datasets
#'     multidataviewer() # Opens the import panel
#' }
#'
#' @export

# Define global variables to satisfy R CMD check
utils::globalVariables(c("att", "col_name", "col_type", "colname", "pos", "value", ".data"))


multidataviewer <- function(...) {

  # Capture all datasets passed
  datasets <- list(...)
  dataset_names <- as.character(substitute(list(...)))[-1]

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
      dataviewr_ui_head(), # Call UI head helper
      shiny::tabsetPanel(
        id = "opt",
        # Always show Import Dataset panel as first tab
        shiny::tabPanel(
          "Import Dataset",
          value = "import_tab",
          shiny::fluidRow(datamods::import_globalenv_ui("myid"))
        )
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
      # This is now much simpler
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
            dataviewr_tab_ui(tab_id) # Call Module UI
          ),
          select = TRUE
        )

        # Store dataset in reactive values
        dataset_store[[tab_id]] <- list(
          data = dataset,
          name = dataset_name
        )

        # Call the Module Server
        dataviewr_tab_server(
          id = tab_id,
          get_data = shiny::reactive(dataset_store[[tab_id]]$data),
          dataset_name = shiny::reactive(dataset_store[[tab_id]]$name)
        )
      }

      # create_tab_observers() function is NO LONGER NEEDED

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

      # Handle import dataset (always available now)
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

      # Handle tab closing
      shiny::observeEvent(input$close_tab, {
        tab_id <- input$close_tab

        # Remove tab
        shiny::removeTab(inputId = "opt", target = tab_id)

        # Remove from storage
        dataset_store[[tab_id]] <- NULL

        # Always switch to import tab when closing a dataset tab
        shiny::updateTabsetPanel(session, "opt", selected = "import_tab")
      })
    }
  )
}
