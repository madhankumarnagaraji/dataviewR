#' Interactive Data Viewer with Filter and Code Generation
#'
#' Launches a Shiny application to explore and filter a 'data.frame' or 'tibble'.
#' If no data is provided, it opens an import panel to load a dataset from either the global environment or the packages.
#'
#' @param data A `data.frame` or `tibble`. If `NULL` or missing, an import UI is shown to load data interactively.
#'
#' @return Launches a Shiny application in the browser. Does not return a value.
#'
#' @details
#' This function provides:
#' \itemize{
#'   \item A tab-based interface with data import and viewer options.
#'   \item A checkbox panel to select/deselect columns.
#'   \item An input for dplyr-compatible filter expressions.
#'   \item A dynamically generated `dplyr` code preview.
#'   \item Metadata display for the variables.
#' }
#'
#' The filtering uses `dplyr::filter()` and generates user-friendly code to replicate the steps.
#' It also provides copyable R code that includes column selection and filtering logic.
#'
#' @importFrom shiny fluidPage tabsetPanel tabPanel actionButton textInput updateTextInput checkboxInput checkboxGroupInput updateCheckboxGroupInput sidebarLayout sidebarPanel mainPanel renderTable tableOutput tags showModal modalDialog modalButton observeEvent updateTabsetPanel reactive reactiveVal req br
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
#'   dataviewer(mtcars)
#'   dataviewer() # Opens the import panel
#' }
#'
#' @export
#' @name dataviewer
utils::globalVariables(c("att", "col_name", "col_type", "colname", "pos", "value"))
dataviewer <- function(data = NULL) {

  if (missing(data) || is.null(data)) {
    cat("\033[34mNote: Showing the Import Dataset Panel because argument 'data' is either missing or NULL\033[0m\n")
    trigger <- 1  # triggers the import dataset panel
  } else if (!missing(data) & !any(class(data) %in% c("tbl_df", "tbl", "data.frame"))) {
    stop(stringr::str_c("Argument 'data' is not a tibble or data.frame"))
  } else if ( !missing(data) & any(class(data) %in% c("tbl_df", "tbl", "data.frame"))) {
    cat("\033[34mNote: Argument 'data' is Passed\033[0m\n")
    trigger <- 2  # # Shows the passed dataframe directly in the Viewer
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(

      shinyjs::useShinyjs(),
      shiny::tabsetPanel(
        id = "opt",
        if (trigger == 1) {
          shiny::tabPanel(
            "Import Dataset",
            shiny::fluidRow(import_globalenv_ui("myid"))
          )
        },
        shiny::tabPanel(
          "Viewer",
          shiny::br(),
          shiny::actionButton("load", "Load the Data"),
          shiny::actionButton("generate_code", "Generate R Code"),
          shiny::h4(shiny::tags$strong("Filter")),
          shiny::textInput("filter", NULL, value = "", width = "40%"),
          shiny::actionButton("clear", "Clear"),
          shiny::actionButton("submit", "Submit"),
          shiny::tags$head(shiny::tags$style(shiny::HTML("
                                                          .scrollable-checkbox{
                                                          max-height: 400px;
                                                          overflow-y: scroll;
                                                          border: 1px solid #ccc;
                                                          padding: 1px;
                                                          background-color: #ffffff;
                                                          }

                                                          /* Custom Scrollbar styles */
                                                          .scrollable-checkbox::-webkit-scrollbar {
                                                           width: 15px; /* Width */
                                                          }

                                                          .scrollable-checkbox::-webkit-scrollbar-track{
                                                           background-color: #ffffff;
                                                          }

                                                          .scrollable-checkbox::-webkit-scrollbar-thumb{
                                                           background-color: #e1e1e1;
                                                           border-radius: 1px;
                                                          }
                                                          .scrollable-checkbox::-webkit-scrollbar-thumb:hover{
                                                           background-color: #120101; /* Color when hovering */
                                                          }
                                                         "))),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::fluidRow(shiny::column(12,
                                            shiny::div(class = "scrollable-checkbox",
                                                       shiny::checkboxInput("cols_all", "Select/Deselect All", TRUE),
                                                       shiny::checkboxGroupInput("columns", "")
                                            )
              )),
              shiny::br(),
              shiny::fluidRow(shiny::column(12,
                                            shiny::div(class = "scrollable-checkbox",
                                                       shiny::h4(shiny::tags$strong("Attribute Information:")),
                                                       shiny::tableOutput("metainfo")
                                            )
              )),
              width = 2
            ),
            shiny::mainPanel(DT::DTOutput("tbl"))
          )
        )
      )
    ),
    server = function(input, output, session) {

      if (trigger == 1) {
        shiny::observeEvent(input$`myid-confirm`, {
          shiny::updateTabsetPanel(session, "opt", selected = "Viewer")
        })
        imported <- import_globalenv_server("myid", btn_show_data = FALSE)
        data1 <- shiny::reactive({ imported$data() })
      } else {
        data1 <- shiny::reactive(data)
      }

      data2 <- shiny::reactive({ data1() })

      shiny::observe({
        columns <- names(data2())
        shiny::updateCheckboxGroupInput(session, "columns", label = NULL, choices = columns,
                                        selected = if (input$cols_all) columns)
      })

      shiny::observe({
        shiny::updateTextInput(session, "filter", label = NULL, value = "", placeholder = "Enter a filter condition e.g., mpg > 20 & cyl == 6")
      })

      last_action <- shiny::reactiveVal("load")

      shiny::observeEvent(input$submit, { last_action("submit") })
      shiny::observeEvent(input$clear, {
        shiny::updateTextInput(session, "filter", value = "")
        last_action("clear")
      })

      filter_df <- shiny::eventReactive(c(input$load, input$submit, input$clear), {
        shiny::req(data2())
        if (last_action() == "submit" && stringr::str_trim(input$filter) != "") {
          tryCatch({
            dplyr::filter(data2(), eval(parse(text = input$filter)))
          }, error = function(e) {
            shiny::showNotification("Invalid filter condition.", type = "error")
            data2()
          })
        } else {
          data2()
        }
      }, ignoreInit = (trigger == 1))

      # Reactive expression to create filter condition string
      filter_code <- shiny::reactive({
        if (stringr::str_trim(input$filter) != "") {
          paste0("filter(", input$filter, ")")
        } else NULL
      })

      selected_cols_code <- shiny::reactive({
        if (length(input$columns) > 0) {
          paste0("select(", paste(shQuote(input$columns), collapse = ", "), ")")
        } else {
          "select(everything())"
        }
      })

      # Generate R code
      generated_code <- shiny::reactive({
        code_lines <- c(
          "# Generated R Code to View the dataframe",
          "# Change the name of the dataframe",
          "library(dplyr)",
          "df |>"  # Insert dataframe/tibble name
        )
        if (!is.null(filter_code())) {
          code_lines <- c(code_lines, paste0("  ", filter_code(), " |>"))
        }
        code_lines <- c(code_lines, paste0("  ", selected_cols_code()))
        paste(code_lines, collapse = "\n")
      })


      # Show modal with code when button clicked
      shiny::observeEvent(input$generate_code, {
        shiny::showModal(shiny::modalDialog(
          title = "Generated R Code",
          shiny::tags$textarea(id = "code_output", rows = 10, style = "width:100%;", generated_code()),
          shiny::tags$br(),
          shiny::actionButton("copy_btn", "Copy"),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
      })

      # Copy Button
      shiny::observeEvent(input$copy_btn, {
        shinyjs::runjs("var copyText = document.getElementById('code_output'); copyText.select(); document.execCommand('copy');")
      })

      cols_df <- shiny::reactive({ dplyr::select(filter_df(), input$columns) })

      final_df <- shiny::reactive({ dplyr::mutate(cols_df(), dplyr::across(where(is.character), ~forcats::fct_drop(forcats::fct_na_value_to_level(as.factor(.x), level = "<NA>")))) })

      shiny::observe({

        att_cols <- shiny::reactive({
          att_list <- purrr::map(data2(), attributes)

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
          dict <- tryCatch(labelled::generate_dictionary(data2()), error = function(e) NULL)
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

        output$metainfo <- shiny::renderTable({
          meta_cols() %>%
            dplyr::arrange(pos, att) %>%
            dplyr::group_by(col_name) %>%
            dplyr::mutate(col_name = ifelse(dplyr::row_number() == 1, col_name, "")) %>%
            dplyr::ungroup() %>%
            dplyr::select(col_name, att, value) %>%
            stats::setNames(c("Variable Name", "Attribute", "Value"))
        }, bordered = TRUE)
      })

      output$tbl <- DT::renderDT({
        DT::datatable(final_df(),
                      extensions = c("Buttons", "FixedHeader", "KeyTable"),
                      filter = "top",
                      class = "cell-border stripe hover nowrap",
                      selection = "none",
                      options = list(
                        pageLength = 50,
                        fixedHeader = TRUE,
                        autoWidth = TRUE,
                        searchHighlight = TRUE,
                        keys = TRUE,
                        # scrollX = TRUE,
                        # scrollY = 100,
                        dom = 'Bfrtip',
                        buttons = list('copy', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download')),
                        initComplete = DT::JS("function(settings, json) { $('<style>', { text: '.sorting::before, .sorting::after, .sorting_asc::before, .sorting_asc::after, .sorting_desc::before, .sorting_desc::after { color: #1B56FD !important; transform: scale(1.5) !important; }' }).appendTo('head'); }")
                      )
        )
      })


    }
  )
}
