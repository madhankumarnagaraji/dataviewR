#' Interactive Data Viewer with Filter and Code Generation
#'
#' Launches a Shiny application to explore and filter a 'data.frame' or 'tibble'.
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
#' @importFrom shiny fluidPage tabsetPanel tabPanel actionButton textInput updateTextInput checkboxInput checkboxGroupInput updateCheckboxGroupInput sidebarLayout sidebarPanel mainPanel renderTable tableOutput tags showModal modalDialog modalButton observeEvent updateTabsetPanel reactive reactiveVal req br removeTab appendTab
#' @importFrom DT datatable renderDT dataTableOutput
#' @importFrom shinyjs useShinyjs runjs
#' @import dplyr
#' @import stringr
#' @import labelled
#' @import forcats
#' @import datamods
#' @importFrom purrr map
#' @importFrom tibble enframe
#' @import htmlwidgets
#'
#' @examples
#' if (interactive()) {
#'     multidataviewer(mtcars)
#'     multidataviewer(iris, mtcars) # Multiple datasets
#'     multidataviewer() # Opens the import panel
#' }
#'
#' @export
utils::globalVariables(c("att", "col_name", "col_type", "colname", "pos", "value"))

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
    ui = shiny::fluidPage(
      shinyjs::useShinyjs(),
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .close-tab-btn {
            margin-left: 10px;
            color: #d9534f;
            cursor: pointer;
            font-weight: bold;
          }
          .close-tab-btn:hover {
            color: #c9302c;
          }
          /* Namespace FixedHeader containers to prevent conflicts */
          .dataTables_wrapper {
            position: relative;
            z-index: 1;
          }
          .dataTables_scrollHead {
            z-index: 2 !important;
          }
        "))
      ),
      shiny::tabsetPanel(
        id = "opt",
        # Always show Import Dataset panel as first tab
        shiny::tabPanel(
          "Import Dataset",
          value = "import_tab",
          shiny::fluidRow(import_globalenv_ui("myid"))
        )
      )
    ),
    server = function(input, output, session) {

      # Reactive values to store dataset information
      dataset_store <- shiny::reactiveValues()
      tab_counter <- shiny::reactiveVal(0)

      # Helper function to create unique tab ID
      create_tab_id <- function(name) {
        paste0("tab_", tolower(gsub("[^a-zA-Z0-9]", "_", name)), "_", tab_counter())
      }

      # Helper function to create viewer tab
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

        shiny::appendTab(
          inputId = "opt",
          shiny::tabPanel(
            title = tab_title,
            value = tab_id,
            shiny::br(),
            shiny::actionButton(paste0("load_", tab_id), "Load the Data"),
            shiny::actionButton(paste0("generate_code_", tab_id), "Generate R Code"),
            shiny::h4(shiny::tags$strong("Filter")),
            shiny::textInput(paste0("filter_", tab_id), NULL, value = "", width = "40%"),
            shiny::actionButton(paste0("clear_", tab_id), "Clear"),
            shiny::actionButton(paste0("submit_", tab_id), "Submit"),
            shiny::tags$head(shiny::tags$style(shiny::HTML("
              .scrollable-checkbox{
                max-height: 400px;
                overflow-y: scroll;
                border: 1px solid #ccc;
                padding: 1px;
                background-color: #ffffff;
              }
              .scrollable-checkbox::-webkit-scrollbar {
                width: 15px;
              }
              .scrollable-checkbox::-webkit-scrollbar-track{
                background-color: #ffffff;
              }
              .scrollable-checkbox::-webkit-scrollbar-thumb{
                background-color: #e1e1e1;
                border-radius: 1px;
              }
              .scrollable-checkbox::-webkit-scrollbar-thumb:hover{
                background-color: #120101;
              }
            "))),
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                shiny::fluidRow(shiny::column(12,
                                              shiny::div(class = "scrollable-checkbox",
                                                         shiny::checkboxInput(paste0("cols_all_", tab_id), "Select/Deselect All", TRUE),
                                                         shiny::checkboxGroupInput(paste0("columns_", tab_id), "")
                                              )
                )),
                shiny::br(),
                shiny::fluidRow(shiny::column(12,
                                              # --- MODIFICATION: Added pop-out link ---
                                              shiny::div(
                                                style = "display: flex; justify-content: space-between; align-items: center; padding: 5px;",
                                                shiny::h4(shiny::tags$strong("Attribute Information:"), style = "margin: 0;"),
                                                shiny::actionLink(paste0("popout_meta_", tab_id), label="", icon = icon("glyphicon glyphicon-new-window",lib = "glyphicon"))
                                              ),
                                              shiny::div(class = "scrollable-checkbox",
                                                         shiny::tableOutput(paste0("metainfo_", tab_id))
                                              )
                                              # --- END MODIFICATION ---
                )),
                width = 2
              ),
              shiny::mainPanel(
                # Wrap DataTable in a unique container for namespace isolation
                shiny::tags$div(
                  id = paste0("container_", tab_id),
                  style = "position: relative;",
                  DT::DTOutput(paste0("tbl_", tab_id))
                )
              )
            )
          ),
          select = TRUE
        )

        # Store dataset in reactive values
        dataset_store[[tab_id]] <- list(
          data = dataset,
          name = dataset_name,
          last_action = shiny::reactiveVal("load")
        )

        # Create observers for this tab
        create_tab_observers(tab_id, dataset_name, session, input, output)
      }

      # Function to create all observers for a tab
      create_tab_observers <- function(tab_id, dataset_name, session, input, output) {

        # Get stored data
        get_data <- shiny::reactive({
          shiny::req(dataset_store[[tab_id]])
          dataset_store[[tab_id]]$data
        })

        get_last_action <- function() {
          dataset_store[[tab_id]]$last_action
        }

        # Update columns checkboxes
        shiny::observe({
          shiny::req(get_data())
          columns <- names(get_data())
          cols_all_input <- paste0("cols_all_", tab_id)
          columns_input <- paste0("columns_", tab_id)

          # Check if the input exists and has a value
          select_all <- isTRUE(input[[cols_all_input]])

          shiny::updateCheckboxGroupInput(
            session, columns_input,
            label = NULL,
            choices = columns,
            selected = if (select_all) columns else NULL
          )
        })

        # Update filter placeholder
        shiny::observe({
          filter_input <- paste0("filter_", tab_id)
          shiny::updateTextInput(
            session, filter_input,
            label = NULL,
            value = "",
            placeholder = "Enter a filter condition e.g., mpg > 20 & cyl == 6"
          )
        })

        # Track last action
        shiny::observeEvent(input[[paste0("submit_", tab_id)]], {
          get_last_action()("submit")
        })

        shiny::observeEvent(input[[paste0("clear_", tab_id)]], {
          shiny::updateTextInput(session, paste0("filter_", tab_id), value = "")
          get_last_action()("clear")
        })

        # Filter dataframe
        filter_df <- shiny::eventReactive(
          c(input[[paste0("load_", tab_id)]],
            input[[paste0("submit_", tab_id)]],
            input[[paste0("clear_", tab_id)]]),
          {
            shiny::req(get_data())
            filter_input <- paste0("filter_", tab_id)

            if (get_last_action()() == "submit" && stringr::str_trim(input[[filter_input]]) != "") {
              tryCatch({
                dplyr::filter(get_data(), eval(parse(text = input[[filter_input]])))
              }, error = function(e) {
                shiny::showNotification("Invalid filter condition.", type = "error")
                get_data()
              })
            } else {
              get_data()
            }
          }
        )

        # Filter code
        filter_code <- shiny::reactive({
          filter_input <- paste0("filter_", tab_id)
          if (stringr::str_trim(input[[filter_input]]) != "") {
            paste0("filter(", input[[filter_input]], ")")
          } else NULL
        })

        # Selected columns code
        selected_cols_code <- shiny::reactive({
          columns_input <- paste0("columns_", tab_id)
          selected_cols <- input[[columns_input]]

          if (length(selected_cols) > 0) {
            # Check if column names follow R naming conventions
            # Valid R names: start with letter or dot (not followed by number),
            # contain only letters, numbers, dots, and underscores
            needs_quotes <- !grepl("^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9._]*$", selected_cols)

            # Use backticks for non-standard column names (R standard)
            formatted_cols <- ifelse(needs_quotes,
                                     paste0("`", selected_cols, "`"),
                                     selected_cols)

            paste0("select(", paste(formatted_cols, collapse = ", "), ")")
          } else {
            NULL  # Return NULL if no columns selected
          }
        })

        # Generated code
        generated_code <- shiny::reactive({
          code_lines <- c(
            "# Generated R Code",
            "library(dplyr)",
            paste0(dataset_name, " |>")
          )

          # Add filter if present
          if (!is.null(filter_code())) {
            code_lines <- c(code_lines, paste0("  ", filter_code()))

            # Add pipe operator only if there's a select statement
            if (!is.null(selected_cols_code())) {
              code_lines[length(code_lines)] <- paste0(code_lines[length(code_lines)], " |>")
            }
          }

          # Add select if present
          if (!is.null(selected_cols_code())) {
            code_lines <- c(code_lines, paste0("  ", selected_cols_code()))
          }

          paste(code_lines, collapse = "\n")
        })

        # Show modal with code
        shiny::observeEvent(input[[paste0("generate_code_", tab_id)]], {
          shiny::showModal(shiny::modalDialog(
            title = "Generated R Code",
            shiny::tags$textarea(
              id = paste0("code_output_", tab_id),
              rows = 10,
              style = "width:100%;",
              generated_code()
            ),
            shiny::tags$br(),
            shiny::actionButton(paste0("copy_btn_", tab_id), "Copy"),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          ))
        })

        # Copy button
        shiny::observeEvent(input[[paste0("copy_btn_", tab_id)]], {
          js_code <- sprintf(
            "var copyText = document.getElementById('code_output_%s'); copyText.select(); document.execCommand('copy');",
            tab_id
          )
          shinyjs::runjs(js_code)
        })

        # Select columns
        cols_df <- shiny::reactive({
          columns_input <- paste0("columns_", tab_id)
          dplyr::select(filter_df(), input[[columns_input]])
        })

        # Final dataframe
        final_df <- shiny::reactive({
          dplyr::mutate(
            cols_df(),
            dplyr::across(
              where(is.character) | where(is.factor),
              ~forcats::fct_drop(forcats::fct_na_value_to_level(as.factor(.x), level = "<NA>"))
            )
          )
        })

        # Metadata
        # --- MODIFICATION: Refactored Metadata Reactives ---
        # Moved these reactives to the top level of create_tab_observers
        # so they can be shared by the sidebar table and the modal table.

        att_cols <- shiny::reactive({
          att_list <- purrr::map(get_data(), attributes)

          if (all(purrr::map_lgl(att_list, is.null))) {
            return(tibble::tibble(colname = character(), att = character(), value = character()))
          }

          purrr::imap_dfr(att_list, function(attr, colname) {
            if (is.null(attr)) return(NULL)
            tibble::tibble(
              colname = colname,
              att = names(attr),
              value = as.character(attr)
            )
          })
        })

        class_df <- shiny::reactive({
          dict <- tryCatch(labelled::generate_dictionary(get_data()), error = function(e) NULL)
          if (is.null(dict) || nrow(dict) == 0) {
            return(tibble::tibble(pos = integer(), colname = character(), col_type = character()))
          }
          dict %>%
            dplyr::mutate(colname = .data$variable) %>%
            dplyr::select(pos, colname, col_type)
        })

        meta_cols <- shiny::reactive({
          dplyr::left_join(class_df(), att_cols(), by = "colname") %>%
            dplyr::mutate(col_name = dplyr::case_when(
              col_type == "int" ~ paste0("\U0001F522", colname),
              col_type == "dbl" ~ paste0("\U0001F522", colname),
              col_type == "chr" ~ paste0("\U0001F524", colname),
              col_type == "date" ~ paste0("\U0001F4C5", colname),
              col_type == "dttm" ~ paste0("\U0001F4C5\U0001F552", colname),
              col_type == "Period" ~ paste0("\U0001F552", colname),
              TRUE ~ paste0("\U0001F524", colname)
            )) %>%
            dplyr::select(pos, col_name, att, value) %>%
            labelled::set_variable_labels(col_name = "Variable Name", att = "Attribute", value = "Value")
        })

        # Original sidebar table renderer (now uses shared reactive)
        shiny::observe({
          output[[paste0("metainfo_", tab_id)]] <- shiny::renderTable({
            meta_cols() %>%
              dplyr::arrange(pos, att) %>%
              dplyr::group_by(col_name) %>%
              dplyr::mutate(col_name = ifelse(dplyr::row_number() == 1, col_name, "")) %>%
              dplyr::ungroup() %>%
              dplyr::select(col_name, att, value) %>%
              stats::setNames(c("Variable Name", "Attribute", "Value"))
          }, bordered = TRUE)
        })

        # --- NEW: Observer for the pop-out modal ---
        shiny::observeEvent(input[[paste0("popout_meta_", tab_id)]], {
          shiny::showModal(shiny::modalDialog(
            title = paste(dataset_name, "- Attribute Information"),
            # Add a scrollable div for large tables
            shiny::div(style = "max-height: 70vh; overflow-y: auto;",
                       shiny::tableOutput(paste0("metainfo_modal_", tab_id))
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          ))
        })

        # --- NEW: Renderer for the modal's table ---
        # It uses the exact same logic as the sidebar table
        output[[paste0("metainfo_modal_", tab_id)]] <- shiny::renderTable({
          meta_cols() %>%
            dplyr::arrange(pos, att) %>%
            dplyr::group_by(col_name) %>%
            dplyr::mutate(col_name = ifelse(dplyr::row_number() == 1, col_name, "")) %>%
            dplyr::ungroup() %>%
            dplyr::select(col_name, att, value) %>%
            stats::setNames(c("Variable Name", "Attribute", "Value"))
        }, bordered = TRUE)
        # --- END MODIFICATION ---


        # Render table - FIXED: Use observer to destroy/recreate FixedHeader on tab changes
        output[[paste0("tbl_", tab_id)]] <- DT::renderDT({
          DT::datatable(
            final_df(),
            extensions = c("Buttons", "FixedHeader", "KeyTable"),
            filter = "top",
            class = "cell-border stripe hover nowrap",
            selection = "none",
            options = list(
              pageLength = 50,
              fixedHeader = list(
                header = TRUE,
                headerOffset = 0
              ),
              autoWidth = TRUE,
              searchHighlight = TRUE,
              keys = TRUE,
              dom = 'Bfrtip',
              buttons = list('copy', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download')),
              initComplete = DT::JS(sprintf("
                function(settings, json) {
                  $('<style>', {
                    text: '.sorting::before, .sorting::after, .sorting_asc::before, .sorting_asc::after, .sorting_desc::before, .sorting_desc::after { color: #1B56FD !important; transform: scale(1.5) !important; }'
                  }).appendTo('head');

                  // Store reference to this table's FixedHeader
                  var tableId = '%s';
                  var api = this.api();

                  // Destroy FixedHeader when tab becomes inactive
                  $('a[data-value]').on('shown.bs.tab', function(e) {
                    var activeTab = $(e.target).attr('data-value');
                    if (activeTab !== tableId) {
                      if (api.fixedHeader) {
                        api.fixedHeader.disable();
                      }
                    } else {
                      if (api.fixedHeader) {
                        api.fixedHeader.enable();
                        api.fixedHeader.adjust();
                      }
                    }
                  });
                }
              ", tab_id))
            )
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
              # Always show close button for initially passed datasets
              create_viewer_tab(tab_id, initial_names[i], initial_datasets[[i]], show_close_btn = TRUE)
            })
          }
        }, once = TRUE)
      }

      # Handle import dataset (always available now)
      imported <- import_globalenv_server("myid", btn_show_data = FALSE)

      shiny::observeEvent(input$`myid-confirm`, {
        shiny::req(imported$data())

        dataset_name <- imported$name()
        dataset <- imported$data()

        # Check if dataset already exists
        if (length(names(dataset_store)) > 0) {
          existing_tabs <- sapply(names(dataset_store), function(tid) {
            dataset_store[[tid]]$name == dataset_name
          }, USE.NAMES = FALSE)

          # Ensure it's a logical vector
          existing_tabs <- as.logical(existing_tabs)
        } else {
          existing_tabs <- logical(0)
        }

        if (length(existing_tabs) > 0 && any(existing_tabs, na.rm = TRUE)) {
          # Switch to existing tab
          existing_tab_id <- names(dataset_store)[which(existing_tabs)[1]]
          shiny::updateTabsetPanel(session, "opt", selected = existing_tab_id)
          shiny::showNotification(paste("Switched to existing tab:", dataset_name), type = "message")
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
