# ===================================================================
# NEW HELPER 3: Module Server
# Creates the server logic for a single data viewer tab
# ===================================================================

#' Internal function for data viewer tab server logic
#'
#' @param id The module's namespace ID.
#' @param get_data A reactive expression returning the dataset.
#' @param dataset_name A reactive expression returning the dataset's name.
#' @noRd
dataviewr_tab_server <- function(id, get_data, dataset_name) {

  shiny::moduleServer(id, function(input, output, session) {

    # This reactive value is now internal to the module
    last_action <- shiny::reactiveVal("load")

    # Provide total rows output (full dataset row count)
    output$totalrows <- shiny::renderText({
      n <- 0L
      d <- get_data()
      if (!is.null(d)) {
        try(n <- NROW(d), silent = TRUE)
      }
      format(n, big.mark = ",")
    })

    # Update columns checkboxes
    shiny::observe({
      shiny::req(get_data())
      columns <- names(get_data())
      select_all <- isTRUE(input$cols_all)

      shiny::updateCheckboxGroupInput(
        session, "columns",
        label = NULL,
        choices = columns,
        selected = if (select_all) columns else NULL
      )
    })

    # Update filter placeholder
    shiny::observe({
      shiny::updateTextInput(
        session, "filter",
        label = NULL,
        value = "",
        placeholder = "Enter a filter condition e.g., mpg > 20 & cyl == 6"
      )
    })

    # Track last action
    shiny::observeEvent(input$submit, {
      last_action("submit")
    })

    shiny::observeEvent(input$clear, {
      shiny::updateTextInput(session, "filter", value = "")
      last_action("clear")
    })

    # Filter dataframe
    filter_df <- shiny::eventReactive(
      c(input$load, input$submit, input$clear),
      {
        shiny::req(get_data())

        # Check the last action.
        if (identical(last_action(), "clear")) {
          return(get_data())
        }

        if (stringr::str_trim(input$filter) != "") {
          tryCatch({
            dplyr::filter(get_data(), eval(parse(text = input$filter)))
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
      if (stringr::str_trim(input$filter) != "") {
        paste0("filter(", input$filter, ")")
      } else NULL
    })

    # Selected columns code
    selected_cols_code <- shiny::reactive({
      selected_cols <- input$columns

      if (length(selected_cols) > 0) {
        needs_quotes <- !grepl("^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9._]*$", selected_cols)
        formatted_cols <- ifelse(needs_quotes,
                                 paste0("`", selected_cols, "`"),
                                 selected_cols)
        paste0("select(", paste(formatted_cols, collapse = ", "), ")")
      } else {
        NULL
      }
    })

    # Generated code
    generated_code <- shiny::reactive({
      code_lines <- c(
        "# Generated R Code",
        "library(dplyr)",
        paste0(dataset_name(), " |>")
      )

      if (!is.null(filter_code())) {
        code_lines <- c(code_lines, paste0("  ", filter_code()))
        if (!is.null(selected_cols_code())) {
          code_lines[length(code_lines)] <- paste0(code_lines[length(code_lines)], " |>")
        }
      }

      if (!is.null(selected_cols_code())) {
        code_lines <- c(code_lines, paste0("  ", selected_cols_code()))
      }

      paste(code_lines, collapse = "\n")
    })

    # Show modal with code
    shiny::observeEvent(input$generate_code, {
      shiny::showModal(shiny::modalDialog(
        title = "Generated R Code",
        shiny::tags$textarea(
          # *** MODULARIZATION FIX: Use namespaced ID ***
          id = session$ns("code_output"),
          rows = 10,
          style = "width:100%;",
          generated_code()
        ),
        shiny::tags$br(),
        shiny::actionButton(session$ns("copy_btn"), "Copy"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # Copy button
    shiny::observeEvent(input$copy_btn, {
      # *** MODULARIZATION FIX: Use namespaced ID ***
      js_code <- sprintf(
        "var copyText = document.getElementById('%s'); copyText.select(); document.execCommand('copy');",
        session$ns("code_output")
      )
      shinyjs::runjs(js_code)
    })

    # Select columns
    cols_df <- shiny::reactive({
      dplyr::select(filter_df(), input$columns)
    })

    # Final dataframe
    final_df <- shiny::reactive({
      dplyr::mutate(
        cols_df(),
        # 1. Handle character/factor NAs (converting to "<NA>")
        dplyr::across(
          where(is.character) | where(is.factor),
          ~forcats::fct_drop(forcats::fct_na_value_to_level(as.factor(.x), level = "<NA>"))
        )
      )
    })

    # --- Metadata Reactives ---
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

    # Sidebar table renderer
    output$metainfo <- shiny::renderTable({
      meta_cols() %>%
        dplyr::arrange(pos, att) %>%
        dplyr::group_by(col_name) %>%
        dplyr::mutate(col_name = ifelse(dplyr::row_number() == 1, col_name, "")) %>%
        dplyr::ungroup() %>%
        dplyr::select(col_name, att, value) %>%
        stats::setNames(c("Variable Name", "Attribute", "Value"))
    }, bordered = TRUE)

    # Observer for the pop-out modal
    shiny::observeEvent(input$popout_meta, {
      shiny::showModal(shiny::modalDialog(
        title = paste(dataset_name(), "- Attribute Information"),
        shiny::div(style = "max-height: 70vh; overflow-y: auto;",
                   # *** MODULARIZATION FIX: Use namespaced ID ***
                   shiny::tableOutput(session$ns("metainfo_modal"))
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # Renderer for the modal's table
    output$metainfo_modal <- shiny::renderTable({
      meta_cols() %>%
        dplyr::arrange(pos, att) %>%
        dplyr::group_by(col_name) %>%
        dplyr::mutate(col_name = ifelse(dplyr::row_number() == 1, col_name, "")) %>%
        dplyr::ungroup() %>%
        dplyr::select(col_name, att, value) %>%
        stats::setNames(c("Variable Name", "Attribute", "Value"))
    }, bordered = TRUE)

    # Render table
    output$tbl <- DT::renderDT({
      DT::datatable(
        final_df(),
        extensions = c("Buttons", "KeyTable"),
        filter = "top",
        class = "cell-border stripe hover nowrap",
        selection = "none",
        options = list(
          pageLength = 100,
          scroller.rowHeight = "auto",
          scrollCollapse = FALSE,
          autoWidth = FALSE,
          searchHighlight = TRUE,
          keys = TRUE,
          dom = 'Bfrtip',
          nullText = "<NA>",
          buttons = list('copy', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download')),
          # *** MODULARIZATION FIX: Use namespaced ID ***
          drawCallback = DT::JS(sprintf("
            function(settings) {
              var tableId = '%s';
              if (window && window.ensureDTFooterMovedTop) {
                window.ensureDTFooterMovedTop(tableId);
              }
            }
          ", session$ns("tbl"))),
          initComplete = DT::JS(sprintf("
            function(settings, json) {
              var tableId = '%s';
              if (window && window.ensureDTFooterMovedTop) {
                window.ensureDTFooterMovedTop(tableId);
              }
              $(document).on('focus mousedown', '.dataTables_wrapper input[type=\"search\"]', function(e) {
                e.stopPropagation();
              });
            }
          ", session$ns("tbl")))
        )
      )
    })

    # Safeguard observer
    shiny::observe({
      shiny::req(final_df())
      # *** MODULARIZATION FIX: Use namespaced ID ***
      shinyjs::runjs(sprintf("if(window && window.ensureDTFooterMovedTop){ window.ensureDTFooterMovedTop('%s'); }", session$ns("tbl")))
    })

    # Custom row info output
    output$row_info <- shiny::renderText({
      # This output was in the UI but never implemented in the server.
      # You can add logic here if needed, e.g., show filter info.
      # For now, just return an empty string to avoid errors.
      ""
    })

  }) # End moduleServer
}
