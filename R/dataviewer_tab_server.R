# Define global variables for NSE evaluation in dplyr / tidyselect
utils::globalVariables(c(
  ".data", "pos", "colname", "col_type", "col_name", "att", "value"
))

#' Internal function for data viewer tab server logic
#' @param id The module's namespace ID.
#' @param get_data A reactive expression returning the dataset.
#' @param dataset_name A reactive expression returning the dataset's name.
#' @noRd
dataviewer_tab_server <- function(id, get_data, dataset_name) {
  shiny::moduleServer(id, function(input, output, session) {
    # --------------------------------------------------
    # REMOVED: "Atomic Batch" Enter Key Handler
    # Filtering is now triggered exclusively by the Submit button.
    # --------------------------------------------------

    # This reactive value is now internal to the module
    last_action <- shiny::reactiveVal("load")
    # FIX: Add initialization flag
    initialized <- shiny::reactiveVal(FALSE)

    # Store validated filter expressions for code generation
    valid_filter_str <- shiny::reactiveVal("")
    valid_filter_out_str <- shiny::reactiveVal("")

    # Provide total rows output (full dataset row count)
    output$totalrows <- shiny::renderText({
      n <- 0L
      d <- get_data()
      if (!is.null(d)) {
        try(n <- NROW(d), silent = TRUE)
      }
      format(n, big.mark = ",")
    })

    # Provide filtered rows output (current rendered rows in DT)
    output$filteredrows <- shiny::renderText({
      # Use the DT-provided indices for rows currently passing all filters
      n <- length(input$tbl_rows_all)
      format(n, big.mark = ",")
    })

    # Provide total columns output
    output$totalcols <- shiny::renderText({
      n <- 0L
      d <- get_data()
      if (!is.null(d)) {
        try(n <- NCOL(d), silent = TRUE)
      }
      format(n, big.mark = ",")
    })

    # Provide selected columns output
    output$selectedcols <- shiny::renderText({
      n <- length(input$columns)
      format(n, big.mark = ",")
    })

    # FIX: Update columns checkboxes with priority and proper selection logic
    shiny::observe(
      {
        shiny::req(get_data())
        # Get columns and current selection state
        columns <- names(get_data())
        select_all <- isTRUE(input$cols_all)

        # FIX: Properly respect the checkbox state
        shiny::updateCheckboxGroupInput(
          session, "columns",
          label = NULL,
          choices = columns,
          selected = if (select_all) columns else NULL
        )
        # Mark as initialized after first update
        if (!initialized()) {
          initialized(TRUE)
        }
      },
      priority = 100
    ) # High priority to ensure it runs first

    # Update filter placeholder
    shiny::observe({
      shiny::updateTextAreaInput(
        session, "filter",
        label = NULL,
        value = "",
        placeholder = "Enter a filter condition e.g., mpg > 20 & cyl == 6"
      )
      shiny::updateTextAreaInput(
        session, "filter_out",
        label = NULL,
        value = "",
        placeholder = "Enter a filter_out condition e.g., gear == 3"
      )
    })

    # Track last action - Submit Button
    shiny::observeEvent(input$submit, {
      last_action("submit")
    })

    shiny::observeEvent(input$clear,
      {
        shiny::updateTextAreaInput(session, "filter", value = "")
        shiny::updateTextAreaInput(session, "filter_out", value = "")
        last_action("clear")
      },
      priority = 100
    )

    validate_filter_expression <- function(expr) {
      # Basic validation: check for dangerous patterns
      dangerous_patterns <- c(
        "system\\(", "shell\\(", "eval\\(",
        "source\\(", ":::", "assign\\("
      )
      if (any(sapply(dangerous_patterns, grepl, x = expr))) {
        stop("Potentially unsafe expression detected")
      }
      # Check if it's a valid R expression
      tryCatch(
        {
          parse(text = expr)
          TRUE
        },
        error = function(e) {
          stop("Invalid R syntax: ", e$message)
        }
      )
    }

    # Filter dataframe
    filter_df <- shiny::eventReactive(
      c(input$load, input$submit, input$clear), # Removed input$enter_trigger
      {
        shiny::req(get_data())

        # Check the last action.
        if (identical(last_action(), "clear")) {
          # Good practice: Ensure error is gone if user clicks Clear
          shiny::removeNotification(id = "filter_error")

          # Reset valid strings for generated code on clear
          valid_filter_str("")
          valid_filter_out_str("")

          return(get_data())
        }

        has_filter <- stringr::str_trim(input$filter) != ""
        has_filter_out <- stringr::str_trim(input$filter_out) != ""

        if (has_filter || has_filter_out) {
          tryCatch(
            {
              # --- FIX : Clear any previous error notification on success ---
              shiny::removeNotification(id = "filter_error")
              # ---------------------------------------------------------------

              df_res <- get_data()

              # --- PRE-EVALUATION STRICT TYPE CHECK ---
              # We evaluate the expression in base R first using custom operator
              # to catch type mismatches before dplyr bypasses them.
              env <- new.env(parent = globalenv())
              get_type_group <- function(x) {
                if (is.null(x)) {
                  return("null")
                }
                if (is.numeric(x)) {
                  return("numeric")
                }
                if (is.character(x) || is.factor(x) || is.ordered(x)) {
                  return("character/factor")
                }
                if (is.logical(x)) {
                  return("logical")
                }
                if (inherits(x, c("Date", "POSIXt", "POSIXct"))) {
                  return("Date/POSIXct")
                }
                class(x)[1]
              }

              make_strict_op <- function(base_op) {
                function(e1, e2) {
                  if (length(e1) == 0 || length(e2) == 0 ||
                    all(is.na(e1)) || all(is.na(e2))) { # nolint
                    return(base_op(e1, e2))
                  }
                  tg1 <- get_type_group(e1)
                  tg2 <- get_type_group(e2)
                  if (tg1 != tg2) {
                    if (length(e1) >= length(e2)) {
                      msg <- sprintf(
                        paste0(
                          "Type mismatch: you're passing ",
                          "a %s value to a %s variable."
                        ),
                        tg2, tg1
                      )
                    } else {
                      msg <- sprintf(
                        paste0(
                          "Type mismatch: you're passing ",
                          "a %s value to a %s variable."
                        ),
                        tg1, tg2
                      )
                    }
                    stop(msg, call. = FALSE)
                  }
                  base_op(e1, e2)
                }
              }

              env$`==` <- make_strict_op(base::`==`)
              env$`!=` <- make_strict_op(base::`!=`)
              env$`<` <- make_strict_op(base::`<`)
              env$`>` <- make_strict_op(base::`>`)
              env$`<=` <- make_strict_op(base::`<=`)
              env$`>=` <- make_strict_op(base::`>=`)
              env$`%in%` <- function(x, table) {
                if (length(x) == 0 || length(table) == 0 ||
                  all(is.na(x)) || all(is.na(table))) { # nolint
                  return(base::`%in%`(x, table))
                }
                tg1 <- get_type_group(x)
                tg2 <- get_type_group(table)
                if (tg1 != tg2) {
                  msg <- sprintf(
                    paste0(
                      "Type mismatch: you're passing ",
                      "a %s value to a %s variable."
                    ),
                    tg2, tg1
                  )
                  stop(msg, call. = FALSE)
                }
                base::`%in%`(x, table)
              }

              if (has_filter) {
                validate_filter_expression(input$filter)
                # Execute pre-check and ONLY intercept our custom Type mismatch errors
                tryCatch(
                  {
                    parsed <- parse(text = input$filter)
                    for (e in parsed) {
                      base::eval(e, envir = df_res, enclos = env)
                    }
                  },
                  error = function(e) {
                    if (grepl("^Type mismatch:", e$message)) {
                      stop(e$message, call. = FALSE)
                    }
                  }
                )
                df_res <- dplyr::filter(
                  df_res,
                  eval(parse(text = input$filter))
                )
              }

              if (has_filter_out) {
                validate_filter_expression(input$filter_out)
                # Execute pre-check and ONLY intercept our custom Type mismatch errors
                tryCatch(
                  {
                    parsed <- parse(text = input$filter_out)
                    for (e in parsed) {
                      base::eval(e, envir = df_res, enclos = env)
                    }
                  },
                  error = function(e) {
                    if (grepl("^Type mismatch:", e$message)) {
                      stop(e$message, call. = FALSE)
                    }
                  }
                )
                # Now using the actual filter_out function from dplyr
                df_res <- dplyr::filter_out(
                  df_res,
                  eval(parse(text = input$filter_out))
                )
              }

              # If everything evaluated without errors, update valid states for code generation
              valid_filter_str(input$filter)
              valid_filter_out_str(input$filter_out)

              df_res
            },
            error = function(e) {
              shiny::showNotification(
                paste0("Invalid condition: ", e$message),
                type = "error",
                duration = 5,
                # Giving a name so we can remove the error notification later
                id = "filter_error"
              )
              # Do not update valid filter strings here so generated code doesn't break
              get_data()
            }
          )
        } else {
          # Also remove error if input is empty
          shiny::removeNotification(id = "filter_error")
          valid_filter_str("")
          valid_filter_out_str("")
          get_data()
        }
      }
    )

    # Filter code
    filter_code <- shiny::reactive({
      if (stringr::str_trim(valid_filter_str()) != "") {
        cleaned_filter <- stringr::str_squish(valid_filter_str())
        paste0("filter(", cleaned_filter, ")")
      } else {
        NULL
      }
    })

    # Filter out code
    filter_out_code <- shiny::reactive({
      if (stringr::str_trim(valid_filter_out_str()) != "") {
        cleaned_filter_out <- stringr::str_squish(valid_filter_out_str())
        paste0("filter_out(", cleaned_filter_out, ")")
      } else {
        NULL
      }
    })

    # Selected columns code
    selected_cols_code <- shiny::reactive({
      selected_cols <- input$columns
      all_cols <- names(get_data())

      if (length(selected_cols) > 0 &&
        length(selected_cols) < length(all_cols)) { # nolint
        needs_quotes <- !grepl(
          "^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9._]*$", selected_cols
        )
        formatted_cols <- ifelse(
          needs_quotes,
          paste0("`", selected_cols, "`"),
          selected_cols
        )
        paste0("select(", paste(formatted_cols, collapse = ", "), ")")
      } else {
        NULL
      }
    })

    # Generated code - FIX: Don't add pipe when no operations
    generated_code <- shiny::reactive({
      has_filter <- !is.null(filter_code())
      has_filter_out <- !is.null(filter_out_code())
      has_select <- !is.null(selected_cols_code())

      # If neither filter nor select, just return the dataset name
      if (!has_filter && !has_filter_out && !has_select) {
        return(paste0(
          "# Generated R Code\n",
          "library(dplyr)\n",
          dataset_name()
        ))
      }

      code_lines <- c(
        "# Generated R Code",
        "library(dplyr)",
        paste0(dataset_name(), " |>")
      )

      if (has_filter) {
        code_lines <- c(code_lines, paste0("  ", filter_code()))
        if (has_filter_out || has_select) {
          last_line_idx <- length(code_lines)
          code_lines[last_line_idx] <- paste0(code_lines[last_line_idx], " |>")
        }
      }

      if (has_filter_out) {
        code_lines <- c(code_lines, paste0("  ", filter_out_code()))
        if (has_select) {
          last_line_idx <- length(code_lines)
          code_lines[last_line_idx] <- paste0(code_lines[last_line_idx], " |>")
        }
      }

      if (has_select) {
        code_lines <- c(code_lines, paste0("  ", selected_cols_code()))
      }

      paste(code_lines, collapse = "\n")
    })

    # Show modal with code
    shiny::observeEvent(input$generate_code, {
      shiny::showModal(shiny::modalDialog(
        title = "Generated R Code",
        shiny::tags$textarea(
          id = session$ns("code_output"),
          rows = 10,
          style = "width:100%;",
          readonly = "readonly", # FIX: Makes the textarea non-editable
          generated_code()
        ),
        shiny::br(),
        shiny::actionButton(session$ns("copy_btn"), "Copy"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # Copy button
    shiny::observeEvent(input$copy_btn, {
      js_code <- sprintf(
        "var copyText = document.getElementById('%s'); copyText.select(); %s",
        session$ns("code_output"),
        "document.execCommand('copy');"
      )
      shinyjs::runjs(js_code)
    })

    # Select columns - FIX: Require at least one column
    cols_df <- shiny::reactive({
      shiny::req(length(input$columns) > 0)
      dplyr::select(filter_df(), dplyr::all_of(input$columns))
    })

    # Final dataframe - FIX: Return NULL when no columns selected
    final_df <- shiny::reactive({
      if (length(input$columns) == 0) {
        return(NULL)
      }
      dplyr::mutate(
        cols_df(),
        # 1. Handle character/factor NAs (converting to "<NA>")
        # to show it in the quick filter box
        dplyr::across(
          tidyselect::where(is.character) | tidyselect::where(is.factor),
          ~ forcats::fct_drop(
            forcats::fct_na_value_to_level(as.factor(.x), level = "<NA>")
          )
        ),
        # 2. Handling the lowercase issue of logical columns in DT
        # by converting to uppercase for R consistency (in the quick filter box)
        dplyr::across(
          tidyselect::where(is.logical),
          ~ forcats::fct_drop(forcats::fct_na_value_to_level(as.factor(.x)))
        )
      )
    })

    # --- Custom Download Handlers ---
    # Downloads the current rendered dataset (after filter + column selection),
    # not just the visible rows shown in the DT viewer.

    # CSV download handler
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0(
          dataset_name(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"
        )
      },
      content = function(file) {
        df <- final_df()
        if (is.null(df)) {
          df <- data.frame()
        }
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # Excel download handler
    output$download_excel <- shiny::downloadHandler(
      filename = function() {
        paste0(
          dataset_name(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"
        )
      },
      content = function(file) {
        df <- final_df()
        if (is.null(df)) {
          df <- data.frame()
        }
        writexl::write_xlsx(df, file)
      }
    )

    # --- Metadata Reactives ---
    att_cols <- shiny::reactive({
      shiny::req(get_data())
      att_list <- purrr::map(get_data(), attributes)

      if (all(purrr::map_lgl(att_list, is.null))) {
        return(tibble::tibble(
          colname = character(), att = character(), value = character()
        ))
      }

      purrr::imap_dfr(att_list, function(attr, colname) {
        if (is.null(attr)) {
          return(NULL)
        }
        tibble::tibble(
          colname = colname,
          att = names(attr),
          value = as.character(attr)
        )
      })
    })

    class_df <- shiny::reactive({
      shiny::req(get_data())
      dict <- tryCatch(
        labelled::generate_dictionary(get_data()),
        error = function(e) NULL
      )
      if (is.null(dict) || nrow(dict) == 0) {
        return(tibble::tibble(
          pos = integer(), colname = character(), col_type = character()
        ))
      }
      dict |>
        dplyr::mutate(colname = .data$variable) |>
        dplyr::select(pos, colname, col_type)
    })

    meta_cols <- shiny::reactive({
      shiny::req(get_data())
      dplyr::left_join(class_df(), att_cols(), by = "colname") |>
        dplyr::mutate(col_name = dplyr::case_when(
          col_type == "int" ~ paste0(
            "<span style='font-size:18px'>", "\u0023\uFE0F\u20E3",
            "</span> ", colname
          ),
          col_type == "dbl" ~ paste0(
            "<span style='font-size:18px'>", "\u0023\uFE0F\u20E3",
            "</span> ", colname
          ),
          col_type == "chr" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F520", "</span> ", colname
          ),
          col_type == "fct" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F520", "</span> ", colname
          ),
          col_type == "lgl" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F501", "</span> ", colname
          ),
          col_type == "date" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F4C5", "</span> ", colname
          ),
          col_type == "dttm" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F4C5\U0001F552",
            "</span> ", colname
          ),
          col_type == "Period" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F552", "</span> ", colname
          ),
          col_type == "time" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F552", "</span> ", colname
          ),
          col_type == "drtn" ~ paste0(
            "<span style='font-size:18px'>", "\U0001F552", "</span> ", colname
          ),
          TRUE ~ paste0(
            "<span style='font-size:18px'>", "\U0001F520", "</span> ", colname
          )
        )) |>
        dplyr::select(pos, col_name, att, value) |>
        labelled::set_variable_labels(
          col_name = "Variable Name", att = "Attribute", value = "Value"
        )
    })

    # Sidebar table renderer
    output$metainfo <- shiny::renderTable(
      {
        shiny::req(get_data())
        meta_cols() |>
          dplyr::arrange(pos, att) |>
          dplyr::group_by(col_name) |>
          dplyr::mutate(
            col_name = ifelse(dplyr::row_number() == 1, col_name, "")
          ) |>
          dplyr::ungroup() |>
          dplyr::select(col_name, att, value) |>
          stats::setNames(c("Variable Name", "Attribute", "Value"))
      },
      bordered = TRUE,
      # it solves the HTML tag escaping issue
      sanitize.text.function = identity
    )

    # Observer for the pop-out modal
    shiny::observeEvent(input$popout_meta, {
      shiny::showModal(shiny::modalDialog(
        title = paste(dataset_name(), "- Attribute Info"),
        shiny::div(
          style = "max-height: 70vh; overflow-y: auto;",
          shiny::tableOutput(session$ns("metainfo_modal"))
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # Renderer for the modal's table
    output$metainfo_modal <- shiny::renderTable(
      {
        meta_cols() |>
          dplyr::arrange(pos, att) |>
          dplyr::group_by(col_name) |>
          dplyr::mutate(
            col_name = ifelse(dplyr::row_number() == 1, col_name, "")
          ) |>
          dplyr::ungroup() |>
          dplyr::select(col_name, att, value) |>
          stats::setNames(c("Variable Name", "Attribute", "Value"))
      },
      bordered = TRUE,
      # it solves the HTML tag escaping issue
      sanitize.text.function = identity
    )

    # Render table - FIX: Handle empty column selection
    output$tbl <- DT::renderDT({
      df <- final_df()

      if (is.null(df)) {
        # Return empty data frame with message
        return(DT::datatable(
          data.frame(
            Message = "No columns selected. Please select at least one column."
          ),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE
        ))
      }

      # Define the JavaScript callback for NA styling and Logical Uppercasing
      row_callback_js <- c(
        "function(row, data){",
        "  for(var i=0; i<data.length; i++){",
        "    // Handle Logical values (true/false to TRUE/FALSE)",
        "    if(typeof data[i] === 'boolean'){",
        "      $('td:eq('+i+')', row).html(data[i] ? 'TRUE' : 'FALSE');",
        "    }",
        "    // Existing logic: Handle the missing values as NA",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('NA')",
        "        .css({'color': 'black', 'font-style': 'normal'});",
        "    }",
        "  }",
        "}"
      )

      DT::datatable(
        df,
        extensions = c("Buttons", "KeyTable"),
        filter = "top",
        class = "cell-border stripe hover nowrap",
        selection = "none",
        options = list(
          pageLength = 500,
          scroller.rowHeight = "auto",
          scrollCollapse = FALSE,
          autoWidth = FALSE,
          searchHighlight = TRUE,
          keys = TRUE,
          # Download removed from DT: it only exported visible/filtered rows
          # Custom Shiny downloadHandler (CSV & Excel) are now used instead,
          # which export the rendered dataset (after filter + column selection).
          dom = "Bfrtip",
          rowCallback = DT::JS(row_callback_js), # Handles missing values as NA
          buttons = list("copy"),
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
              $(document).on(
                'focus mousedown',
                '.dataTables_wrapper input[type=\"search\"]',
                function(e) { e.stopPropagation(); }
              );
            }
          ", session$ns("tbl")))
        )
      )
    })

    # Safeguard observer
    shiny::observe({
      shiny::req(final_df())
      shinyjs::runjs(sprintf(
        "if(window && window.ensureDTFooterMovedTop){ %s }",
        sprintf("window.ensureDTFooterMovedTop('%s');", session$ns("tbl"))
      ))
    })

    # Custom row info output
    output$row_info <- shiny::renderText({
      ""
    })
  }) # End moduleServer
}
