# Define global variables for NSE evaluation in dplyr / tidyselect
utils::globalVariables(c(
  ".data", "pos", "colname", "col_type", "col_name", "att", "value"
))

#' Internal function for data viewer tab server logic
#'
#' @param id The module's namespace ID.
#' @param get_data A reactive expression returning the dataset.
#' @param dataset_name A reactive expression returning the dataset's name.
#'
#' @noRd
dataviewer_tab_server <- function(id, get_data, dataset_name) {
  shiny::moduleServer(id, function(input, output, session) {
    # --------------------------------------------------
    # REMOVED: "Atomic Batch" Enter Key Handler
    # Filtering is now triggered exclusively by the Submit button.
    # Enter key is now handled in the UI via onkeypress on the textInput,
    # which fires input$enter_trigger. No server-side JS listener needed.
    # --------------------------------------------------

    # Null-coalescing helper (base R < 4.4 doesn't have %||%)
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # This reactive value is now internal to the module
    last_action <- shiny::reactiveVal("load")
    # FIX: Add initialization flag
    initialized <- shiny::reactiveVal(FALSE)

    # Stores the inter filter condition that was last submitted via the modal.
    applied_inter_condition <- shiny::reactiveVal("")

    # Persists the picker selection across modal open/close cycles.
    # input$inter_vars becomes NULL when the modal is destroyed on close.
    last_inter_vars <- shiny::reactiveVal(character(0))

    # Remembers last widget value per variable so datamods can seed them
    # via `defaults` instead of resetting to "everything selected" on rebuild.
    filter_defaults_store <- shiny::reactiveValues()

    # Wider drop_ids thresholds than datamods' defaults (p=0.9, n=50) to
    # avoid hiding legitimate multi-level factor variables.
    drop_ids_cfg <- list(p = 0.95, n = 200)

    # Remembers the "Include NA" switch state per variable (default: OFF).
    # datamods has no per-variable default for this switch, so we track it
    # ourselves and use it in build_na_aware_condition() at Submit time.
    na_defaults_store <- shiny::reactiveValues()

    # Flag set before a datamods widget rebuild so the NA-mirror observer
    # discards the browser's spurious "remount echo" for that variable.
    na_pending_echo <- shiny::reactiveValues()
    mark_na_pending_echo <- function(vars) {
      for (v in vars) {
        na_pending_echo[[v]] <- TRUE
        local({
          var <- v
          later::later(function() {
            shiny::withReactiveDomain(session, {
              na_pending_echo[[var]] <- FALSE
            })
          }, delay = 2)
        })
      }
    }

    # Build the datamods-internal input id for a variable's NA toggle switch.
    hex_var_id <- function(var) paste(as.character(charToRaw(var)), collapse = "")
    na_toggle_input_id <- function(var) paste0("dm_filter-na_filter_", hex_var_id(var))

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
    shiny::observe({
      shiny::req(get_data())
      # Get columns and current selection state
      columns    <- names(get_data())
      select_all <- isTRUE(input$cols_all)

      # FIX: Properly respect the checkbox state
      shiny::updateCheckboxGroupInput(
        session, "columns",
        label    = NULL,
        choices  = columns,
        selected = if (select_all) columns else NULL  # FIXED: Now respects deselect
      )

      # Mark as initialized after first update
      if (!initialized()) {
        initialized(TRUE)
      }
    }, priority = 100)  # High priority to ensure it runs first

    # Update filter placeholder
    shiny::observe({
      shiny::updateTextInput(
        session, "filter",
        label       = NULL,
        value       = "",
        placeholder = "Enter a filter condition e.g., mpg > 20 & cyl == 6"
      )
    })

    # Track last action - Submit Button
    shiny::observeEvent(input$submit, {
      last_action("submit")
    })

    shiny::observeEvent(input$clear,
                        {
                          shiny::updateTextInput(session, "filter", value = "")
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

    # ------------------------------------------------------------------
    # Text filter condition
    # active_text_condition stores last validated text.
    # "" = no text filter active.
    # ------------------------------------------------------------------
    active_text_condition <- shiny::reactiveVal("")

    # Backspace to empty — clear immediately without Submit
    # Use identical() to safely handle NULL input$filter on startup
    shiny::observe({
      if (identical(trimws(input$filter %||% ""), "")) {
        shiny::removeNotification(id = "filter_error")
        active_text_condition("")
      }
    })

    # Submit / Enter key / Load / Clear
    shiny::observeEvent(
      c(input$submit, input$enter_trigger, input$load, input$clear),
      {
        if (identical(last_action(), "clear") || identical(trimws(input$filter %||% ""), "")) {
          # Good practice: Ensure error is gone if user clicks Clear
          shiny::removeNotification(id = "filter_error")
          active_text_condition("")
          return()
        }

        tryCatch({
          # Clear any previous error notification on success
          shiny::removeNotification(id = "filter_error")
          validate_filter_expression(input$filter)

          # --- PRE-EVALUATION STRICT TYPE CHECK ---
          # Runs a dry-run with custom operators to catch type mismatches
          # (e.g. cyl == "6" when cyl is numeric) BEFORE storing the condition.
          # Only intercepts our custom "Type mismatch:" errors; all others pass through.
          env <- new.env(parent = globalenv())

          get_type_group <- function(x) {
            if (is.null(x))                               return("null")
            if (is.numeric(x))                            return("numeric")
            if (is.character(x) || is.factor(x) || is.ordered(x)) return("character/factor")
            if (is.logical(x))                            return("logical")
            if (inherits(x, c("Date", "POSIXt", "POSIXct"))) return("Date/POSIXct")
            class(x)[1]
          }

          make_strict_op <- function(base_op) {
            function(e1, e2) {
              if (length(e1) == 0 || length(e2) == 0 ||
                  all(is.na(e1)) || all(is.na(e2))) {
                return(base_op(e1, e2))
              }
              tg1 <- get_type_group(e1)
              tg2 <- get_type_group(e2)
              if (tg1 != tg2) {
                msg <- if (length(e1) >= length(e2)) {
                  sprintf("Type mismatch: you're passing a %s value to a %s variable.", tg2, tg1)
                } else {
                  sprintf("Type mismatch: you're passing a %s value to a %s variable.", tg1, tg2)
                }
                stop(msg, call. = FALSE)
              }
              base_op(e1, e2)
            }
          }

          env$`==` <- make_strict_op(base::`==`)
          env$`!=` <- make_strict_op(base::`!=`)
          env$`<`  <- make_strict_op(base::`<`)
          env$`>`  <- make_strict_op(base::`>`)
          env$`<=` <- make_strict_op(base::`<=`)
          env$`>=` <- make_strict_op(base::`>=`)

          env$`%in%` <- function(x, table) {
            if (length(x) == 0 || length(table) == 0 ||
                all(is.na(x)) || all(is.na(table))) {
              return(base::`%in%`(x, table))
            }
            tg1 <- get_type_group(x)
            tg2 <- get_type_group(table)
            if (tg1 != tg2) {
              stop(sprintf(
                "Type mismatch: you're passing a %s value to a %s variable.", tg2, tg1
              ), call. = FALSE)
            }
            base::`%in%`(x, table)
          }

          tryCatch(
            {
              parsed <- parse(text = input$filter)
              for (e in parsed) {
                base::eval(e, envir = get_data(), enclos = env)
              }
            },
            error = function(e) {
              if (grepl("^Type mismatch:", e$message)) stop(e$message, call. = FALSE)
            }
          )
          # ----------------------------------------

          # Type check passed — store condition for combined_filter_df
          active_text_condition(trimws(input$filter))

        }, error = function(e) {
          shiny::showNotification(
            paste0("Invalid filter condition: ", e$message),
            type     = "error",
            duration = 5,
            id       = "filter_error"
          )
        })
      },
      ignoreInit = TRUE
    )

    # ==================================================================
    # NEW BLOCK 1 — Interactive Filtering Modal
    # ==================================================================

    # -- Render the modal UI dynamically so pickerInput + filter_data_ui
    #    live inside the module namespace correctly --
    output$inter_modal_ui <- shiny::renderUI({ NULL })  # placeholder only

    # filter_data_ui must be rendered ONCE and persisted — not recreated
    # inside showModal every time, as that resets datamods internal state.
    output$dm_filter_ui <- shiny::renderUI({
      datamods::filter_data_ui(
        id         = session$ns("dm_filter"),
        show_nrow  = TRUE,
        max_height = "350px"
      )
    })

    # Persist picker selection; ignoreNULL = TRUE prevents firing on modal close
    # when input$inter_vars becomes NULL. Seeds new categorical vars with nothing
    # selected (blank slate) and defaults NA switch to OFF for new vars.
    shiny::observeEvent(input$inter_vars, {
      new_vars <- input$inter_vars
      old_vars <- last_inter_vars()

      added <- setdiff(new_vars, old_vars)
      data  <- get_data()
      if (length(added) > 0 && !is.null(data)) {
        for (v in added) {
          if (v %in% names(data) && is.null(filter_defaults_store[[v]])) {
            col <- data[[v]]
            if (is.character(col) || is.factor(col) || is.logical(col)) {
              filter_defaults_store[[v]] <- character(0)
            }
          }
          if (is.null(na_defaults_store[[v]])) {
            na_defaults_store[[v]] <- FALSE
          }
        }
      }

      last_inter_vars(new_vars)

      # Picker change triggers a full datamods widget rebuild — mark all vars
      # so the NA mirror observer discards the resulting rebuild echoes.
      mark_na_pending_echo(new_vars)
    }, ignoreNULL = TRUE)

    # ==================================================================
    # NEW BLOCK 2 — Step 2: datamods interactive filter
    # Always receives raw get_data() as its data input.
    # vars is driven by what user selects inside the modal picker.
    # ==================================================================
    dm_filter_res <- datamods::filter_data_server(
      id   = "dm_filter",
      data = shiny::reactive(get_data()),
      vars = shiny::reactive({
        # Use last_inter_vars() — input$inter_vars becomes NULL on modal close.
        # Date/POSIXct excluded: their range sliders can't be reliably dragged.
        vars_chosen <- last_inter_vars()
        if (length(vars_chosen) == 0) return(character(0))

        data <- get_data()
        if (is.null(data)) return(vars_chosen)
        is_date_col <- function(v) v %in% names(data) && inherits(data[[v]], c("Date", "POSIXct"))
        vars_chosen[!vapply(vars_chosen, is_date_col, logical(1))]
      }),
      name      = dataset_name,
      drop_ids  = drop_ids_cfg,
      value_na  = FALSE,
      defaults  = shiny::reactive({
        vals <- shiny::reactiveValuesToList(filter_defaults_store)
        vals <- vals[!vapply(vals, is.null, logical(1))]

        # Drop any default whose shape doesn't match the widget type —
        # a stray character(0) for a numeric slider hard-errors in datamods.
        data <- get_data()
        if (length(vals) > 0 && !is.null(data)) {
          keep <- vapply(names(vals), function(nm) {
            if (!(nm %in% names(data))) return(FALSE)
            col <- data[[nm]]
            v   <- vals[[nm]]
            if (is.numeric(col) || inherits(col, c("Date", "POSIXct"))) {
              length(v) == 2 && !anyNA(suppressWarnings(as.numeric(v)))
            } else {
              TRUE  # categorical widgets accept any length, including 0
            }
          }, logical(1))
          vals <- vals[keep]
        }

        if (length(vals) == 0) NULL else vals
      })
    )

    # Debounced mirror for filter_defaults_store only — prevents slider drags
    # from triggering a datamods full rebuild on every pixel of movement.
    # Submit still reads dm_filter_res$values() directly (undebounced).
    dm_filter_values_debounced <- shiny::debounce(
      shiny::reactive(dm_filter_res$values()),
      millis = 700
    )

    # Mirror settled widget values into filter_defaults_store so `defaults`
    # can restore them on rebuild. For NULL values, only store character(0)
    # for categorical vars — numeric/date NULL just means not yet mounted.
    shiny::observe({
      vals <- tryCatch(dm_filter_values_debounced(), error = function(e) NULL)
      if (is.null(vals) || length(vals) == 0) return()
      data <- get_data()
      for (nm in names(vals)) {
        v <- vals[[nm]]
        if (is.null(v)) {
          is_categorical <- !is.null(data) && nm %in% names(data) &&
            (is.character(data[[nm]]) || is.factor(data[[nm]]) || is.logical(data[[nm]]))
          if (is_categorical) filter_defaults_store[[nm]] <- character(0)
        } else {
          filter_defaults_store[[nm]] <- v
        }
      }
      # Value change triggers a full widget rebuild — mark NA echo guards.
      mark_na_pending_echo(names(vals))
    })

    # Mirror the "Include NA" switch state into na_defaults_store.
    # na_pending_echo is read via isolate() so setting the flag elsewhere
    # doesn't re-trigger this observer before the actual rebuild echo arrives.
    shiny::observe({
      vars_now <- last_inter_vars()
      if (length(vars_now) == 0) return()
      for (v in vars_now) {
        na_val <- input[[na_toggle_input_id(v)]]
        if (is.null(na_val)) next
        if (isTRUE(shiny::isolate(na_pending_echo[[v]]))) {
          na_pending_echo[[v]] <- FALSE
          next
        }
        na_defaults_store[[v]] <- na_val
      }
    })

    # NOTE: we do NOT push na_defaults_store back onto the switch — doing so
    # creates a feedback loop (push → mirror → push) that made the filter
    # behavior random at Submit time. build_na_aware_condition() reads
    # na_defaults_store directly so the on-screen switch state doesn't matter.

    # Mirrors datamods' internal drop_id() heuristic to detect which vars
    # get no filter widget (high-cardinality, single-value, Date/POSIXct).
    is_var_skipped <- function(data, v, cfg) {
      if (!(v %in% names(data))) return(TRUE)
      col <- data[[v]]
      if (all(is.na(col))) return(TRUE)
      if (inherits(col, c("Date", "POSIXct"))) return(TRUE)
      if (!inherits(col, c("character", "factor"))) return(FALSE)
      vals <- unique(as.character(col))
      vals <- tryCatch(vals[trimws(vals) != ""], error = function(e) vals)
      if (length(vals) <= 1) return(TRUE)
      if (length(vals) >= (length(col) * cfg$p)) return(TRUE)
      if (length(vals) >= cfg$n) return(TRUE)
      FALSE
    }

    output$skipped_vars_msg <- shiny::renderUI({
      selected <- last_inter_vars()
      data     <- get_data()
      if (length(selected) == 0 || is.null(data)) return(NULL)

      skipped <- selected[vapply(selected, is_var_skipped, logical(1), data = data, cfg = drop_ids_cfg)]

      if (length(skipped) == 0) return(NULL)

      shiny::div(
        style = paste(
          "margin-top: 10px; padding: 8px 12px;",
          "background-color: #fff8e1; border-left: 4px solid #f9a825;",
          "border-radius: 3px; font-size: 13px; color: #555;"
        ),
        shiny::icon("info-circle"),
        shiny::tags$strong(" No filter widget available for: "),
        shiny::tags$em(paste(skipped, collapse = ", ")),
        shiny::tags$span(
          style = "display:block; margin-top:4px;",
          "These variables are not available for interactive filtering.",
          shiny::tags$br(),
          "Use the 'Filter' text box above the table instead,",
          " e.g. ",
          shiny::tags$code(paste0(skipped[1], ' == "value"'))
        )
      )
    })

    # Observer for the interactive filtering modal.
    # Mark NA echo guards before opening — modal remount triggers spurious
    # switch reports that the mirror observer must discard.
    shiny::observeEvent(input$open_inter_modal, {
      mark_na_pending_echo(last_inter_vars())

      shiny::showModal(shiny::modalDialog(
        title = "Interactive Filtering",
        size  = "l",
        easyClose = FALSE,

        # Step 1: column picker — restore from last_inter_vars() since
        # input$inter_vars is NULL after modal close.
        shinyWidgets::pickerInput(
          inputId  = session$ns("inter_vars"),
          label    = shiny::tags$strong("Select variables to filter:"),
          choices  = names(get_data()),
          selected = last_inter_vars(),
          multiple = TRUE,
          width    = "100%",
          options  = shinyWidgets::pickerOptions(
            liveSearch            = TRUE,
            liveSearchPlaceholder = "Search variable...",
            actionsBox            = TRUE,
            selectedTextFormat    = "count > 3",
            noneSelectedText      = "— pick variables to filter —",
            countSelectedText     = "{0} variables selected"
          )
        ),

        shiny::br(),

        # Step 2: datamods widgets — referenced via uiOutput so the widget
        # is NOT recreated on every modal open (which would reset datamods state)
        shiny::uiOutput(session$ns("dm_filter_ui")),

        # Message for high-cardinality variables datamods skipped
        shiny::uiOutput(session$ns("skipped_vars_msg")),

        footer = shiny::tagList(
          # Clear All — resets picker + clears applied condition
          shiny::actionButton(
            session$ns("inter_clear_all"),
            "Clear All",
            icon  = shiny::icon("times"),
            class = "btn-danger"
          ),
          # Submit — stores the current datamods condition
          shiny::actionButton(
            session$ns("inter_submit"),
            "Submit",
            icon  = shiny::icon("check"),
            class = "btn-primary"
          ),
          shiny::modalButton("Close")
        )
      ))
    })

    # Build the filter condition from dm_filter_res$values() + na_defaults_store,
    # rather than dm_filter_res$expr()/code() which reads the live NA switch
    # and is unreliable when widgets have been rebuilt (switch resets to default).
    build_na_aware_condition <- function(data, values_list, na_choices) {
      quote_vec <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
      parts <- character(0)

      for (var in names(values_list)) {
        if (!(var %in% names(data))) next
        raw_val <- values_list[[var]]

        col    <- data[[var]]
        any_na <- anyNA(col)
        nas    <- isTRUE(na_choices[[var]] %||% FALSE)  # our default is "exclude NA"

        if (is.numeric(col) || inherits(col, c("Date", "POSIXct"))) {
          if (is.null(raw_val)) next  # not yet rendered — skip
          val <- raw_val
          full_range <- suppressWarnings(range(col, na.rm = TRUE))
          is_full <- length(val) == 2 &&
            isTRUE(all.equal(as.numeric(val), as.numeric(full_range)))

          if (!is_full) {
            bound_expr <- if (inherits(col, "Date")) {
              sprintf('%s >= as.Date("%s") & %s <= as.Date("%s")', var, format(val[1]), var, format(val[2]))
            } else if (inherits(col, "POSIXct")) {
              sprintf('%s >= as.POSIXct("%s") & %s <= as.POSIXct("%s")', var, format(val[1]), var, format(val[2]))
            } else {
              sprintf("%s >= %s & %s <= %s", var, val[1], var, val[2])
            }
            if (any_na && nas) bound_expr <- sprintf("(%s | is.na(%s))", bound_expr, var)
            parts <- c(parts, bound_expr)
          } else if (any_na && !nas) {
            parts <- c(parts, sprintf("!is.na(%s)", var))
          }
        } else {
          # Categorical: NULL means nothing selected (treat same as character(0)).
          data_vals <- unique(as.character(col))
          data_vals <- data_vals[!is.na(data_vals)]
          val_chr   <- if (is.null(raw_val)) character(0) else as.character(raw_val)
          is_full   <- length(val_chr) > 0 && setequal(val_chr, data_vals)

          if (length(val_chr) == 0) {
            parts <- c(parts, if (nas) {
              sprintf("is.na(%s)", var)
            } else {
              sprintf("!(%s %%in%% c(%s)) & !is.na(%s)", var, paste(quote_vec(data_vals), collapse = ", "), var)
            })
          } else if (!is_full) {
            vals_code <- if (length(val_chr) == 1) {
              quote_vec(val_chr)
            } else {
              paste0("c(", paste(quote_vec(val_chr), collapse = ", "), ")")
            }
            base_expr <- sprintf("%s %%in%% %s", var, vals_code)  # %in% excludes NA by default
            if (any_na && nas) base_expr <- sprintf("(%s | is.na(%s))", base_expr, var)
            parts <- c(parts, base_expr)
          } else if (any_na && !nas) {
            parts <- c(parts, sprintf("!is.na(%s)", var))
          }
        }
      }

      if (length(parts) == 0) "" else paste(parts, collapse = " & ")
    }

    # -- Modal Submit: build the condition and store it --
    shiny::observeEvent(input$inter_submit, {
      values_list <- tryCatch(dm_filter_res$values(), error = function(e) list())
      na_state    <- shiny::reactiveValuesToList(na_defaults_store)

      condition <- tryCatch(
        build_na_aware_condition(get_data(), values_list, na_state),
        error = function(e) ""
      )

      applied_inter_condition(condition)

      # Update button: blue + checkmark when active, default when not
      is_active <- nchar(condition) > 0
      shinyjs::runjs(sprintf(
        "var btn = document.getElementById('%s');
         if (btn) {
           btn.innerHTML = '%s';
           btn.style.backgroundColor = '%s';
           btn.style.color           = '%s';
           btn.style.borderColor     = '%s';
         }",
        session$ns("open_inter_modal"),
        if (is_active) '<i class=\"fa fa-filter\"></i> Interactive Filtering ✓'
        else           '<i class=\"fa fa-filter\"></i> Interactive Filtering',
        if (is_active) "#2196F3" else "",
        if (is_active) "white"   else "",
        if (is_active) "#1976D2" else ""
      ))

      shiny::showNotification(
        if (is_active) "✅ Interactive filter applied" else "ℹ️ No filter active",
        type     = "message",
        duration = 2
      )
    })

    # -- Modal Clear All: reset picker + clear all stored state --
    shiny::observeEvent(input$inter_clear_all, {
      last_inter_vars(character(0))

      stored_vars <- names(shiny::reactiveValuesToList(filter_defaults_store))
      for (nm in stored_vars) filter_defaults_store[[nm]] <- NULL

      na_stored_vars <- names(shiny::reactiveValuesToList(na_defaults_store))
      for (nm in na_stored_vars) na_defaults_store[[nm]] <- NULL

      shinyWidgets::updatePickerInput(
        session  = session,
        inputId  = "inter_vars",
        selected = character(0)
      )
      applied_inter_condition("")

      # Reset button style to default
      shinyjs::runjs(sprintf(
        "var btn = document.getElementById('%s');
         if (btn) {
           btn.innerHTML     = '<i class=\"fa fa-filter\"></i> Interactive Filtering';
           btn.style.backgroundColor = '';
           btn.style.color           = '';
           btn.style.borderColor     = '';
         }",
        session$ns("open_inter_modal")
      ))

      shiny::showNotification("ℹ️ Interactive filter cleared", type = "message", duration = 2)
    })

    # ==================================================================
    # NEW BLOCK 3 — Combined filter
    # Both filters applied independently to raw get_data(), combined with &
    # ==================================================================
    combined_filter_df <- shiny::reactive({
      shiny::req(get_data())

      text_cond  <- active_text_condition()
      inter_cond <- applied_inter_condition()

      has_text  <- nchar(trimws(text_cond))  > 0
      has_inter <- nchar(trimws(inter_cond)) > 0

      # Neither active — return raw data
      if (!has_text && !has_inter) return(get_data())

      combined_cond <- if (has_text && has_inter) {
        paste0("(", text_cond, ") & (", inter_cond, ")")
      } else if (has_text) {
        text_cond
      } else {
        inter_cond
      }

      tryCatch({
        dplyr::filter(get_data(), eval(parse(text = combined_cond)))
      }, error = function(e) {
        shiny::showNotification(
          paste0("Filter error: ", e$message),
          type = "error", duration = 5, id = "combined_filter_error"
        )
        get_data()
      })
    })

    # ==================================================================
    # (UNCHANGED structure) Filter code — text box part (Step 1)
    # ==================================================================
    filter_code <- shiny::reactive({
      cond <- active_text_condition()
      if (nchar(trimws(cond)) > 0) paste0("filter(", cond, ")") else NULL
    })

    # ==================================================================
    # NEW BLOCK 4 — Inter filter code (Step 2)
    # Extracts the stored condition only when inter is active
    # ==================================================================
    inter_filter_code <- shiny::reactive({
      cond <- applied_inter_condition()
      if (nchar(trimws(cond)) > 0) paste0("filter(", cond, ")") else NULL
    })

    # Selected columns code
    selected_cols_code <- shiny::reactive({
      selected_cols <- input$columns
      all_cols      <- names(get_data())

      if (length(selected_cols) > 0 && length(selected_cols) < length(all_cols)) {
        needs_quotes   <- !grepl("^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9._]*$", selected_cols)
        formatted_cols <- ifelse(needs_quotes,
                                 paste0("`", selected_cols, "`"),
                                 selected_cols)
        paste0("select(", paste(formatted_cols, collapse = ", "), ")")
      } else {
        NULL
      }
    })

    # Generated code - FIX: Don't add pipe when no operations
    # Includes Step 1 (text filter), Step 2 (inter filter), and select() as needed.
    generated_code <- shiny::reactive({
      has_filter       <- !is.null(filter_code())
      has_inter_filter <- !is.null(inter_filter_code())
      has_select       <- !is.null(selected_cols_code())

      # If neither filter nor select, just return the dataset name
      if (!has_filter && !has_inter_filter && !has_select) {
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
        line <- paste0("  ", filter_code())
        if (has_inter_filter || has_select) line <- paste0(line, " |>")
        code_lines <- c(code_lines, line)
      }

      if (has_inter_filter) {
        line <- paste0("  ", inter_filter_code())
        if (has_select) line <- paste0(line, " |>")
        code_lines <- c(code_lines, line)
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
          id       = session$ns("code_output"),
          rows     = 10,
          style    = "width:100%;",
          readonly = "readonly", # FIX: Makes the textarea non-editable for the user
          generated_code()
        ),
        shiny::br(),
        shiny::actionButton(session$ns("copy_btn"), "Copy"),
        easyClose = TRUE,
        footer    = shiny::modalButton("Close")
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
    # UPDATED: now consumes combined_filter_df() instead of filter_df(),
    # so inter results flow through to the table
    cols_df <- shiny::reactive({
      shiny::req(length(input$columns) > 0)
      dplyr::select(combined_filter_df(), dplyr::all_of(input$columns))
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
          att     = names(attr),
          value   = as.character(attr)
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
          col_type == "int"    ~ paste0("<span style='font-size:18px'>", "#️⃣", "</span> ", colname),
          col_type == "dbl"    ~ paste0("<span style='font-size:18px'>", "#️⃣", "</span> ", colname),
          col_type == "chr"    ~ paste0("<span style='font-size:18px'>", "\U0001F520", "</span> ", colname),
          col_type == "fct"    ~ paste0("<span style='font-size:18px'>", "\U0001F520", "</span> ", colname),
          col_type == "lgl"    ~ paste0("<span style='font-size:18px'>", "\U0001F501", "</span> ", colname),
          col_type == "date"   ~ paste0("<span style='font-size:18px'>", "\U0001F4C5", "</span> ", colname),
          col_type == "dttm"   ~ paste0("<span style='font-size:18px'>", "\U0001F4C5\U0001F552", "</span> ", colname),
          col_type == "Period" ~ paste0("<span style='font-size:18px'>", "\U0001F552", "</span> ", colname),
          col_type == "time"   ~ paste0("<span style='font-size:18px'>", "\U0001F552", "</span> ", colname),
          col_type == "drtn"   ~ paste0("<span style='font-size:18px'>", "\U0001F552", "</span> ", colname),
          TRUE                 ~ paste0("<span style='font-size:18px'>", "\U0001F520", "</span> ", colname)
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
        footer    = shiny::modalButton("Close")
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
        filter     = "top",
        class      = "cell-border stripe hover nowrap",
        selection  = "none",
        options    = list(
          pageLength      = 500,
          scroller.rowHeight = "auto",
          scrollCollapse  = FALSE,
          autoWidth       = FALSE,
          searchHighlight = TRUE,
          keys            = TRUE,
          # Download button removed from DT buttons: it only exported visible/filtered rows.
          # Custom Shiny downloadHandler buttons (CSV & Excel) are now used instead,
          # which export the full current rendered dataset (after filter + column selection).
          dom             = 'Bfrtip',
          rowCallback     = DT::JS(row_callback_js),
          buttons         = list('copy'),
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
