#' Internal function for data viewer tab UI
#' @param id The module's namespace ID.
#' @noRd
dataviewer_tab_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::br(),
    shiny::actionButton(ns("load"), "Refresh the Data"),
    shiny::actionButton(ns("generate_code"), "Generate R Code"),
    shiny::h4(shiny::tags$strong("Filters")),

    # --- Custom CSS for Filter UI Elements ---
    shiny::tags$style(shiny::HTML("
      .filter-input-container {
        display: flex;
        flex-direction: column;
        gap: 5px; /* Reduced gap between the two filter boxes */
        width: 40%;
        margin-bottom: 15px;
      }
      .filter-row {
        display: flex;
        align-items: center;
      }
      .filter-label {
        padding: 6px 12px;
        font-size: 13px;
        font-weight: bold;
        color: black;
        background-color: transparent; /* Removed background colors */
        border: 1px solid #ccc;
        border-right: none; /* Remove right border to attach to text box */
        border-radius: 4px 0 0 4px; /* Round only the left corners */
        height: 34px; /* Match standard Shiny input height */
        display: flex;
        align-items: center;
        justify-content: center;
        white-space: nowrap;
        min-width: 110px;
      }
      .filter-active {
        background-color: #337ab7 !important;
        color: white !important;
        border-color: #337ab7 !important;
      }
      /* Remove default Shiny wrapper margin and allow it to fill space */
      .filter-row .form-group {
        margin-bottom: 0 !important;
        flex-grow: 1;
      }
      /* Make textarea behave like a single line with horizontal scrolling */
      .filter-row textarea {
        white-space: nowrap !important;
        overflow-x: auto !important;
        overflow-y: hidden !important;
        resize: none !important;
        height: 34px !important;
        line-height: 20px;
        border-radius: 0 4px 4px 0 !important; /* Round only the right corners */
      }
    ")),

    shiny::div(class = "filter-input-container",
      shiny::div(class = "filter-row",
        shiny::span("Keep Rows", id = ns("label_filter"), class = "filter-label"),
        shiny::textAreaInput(ns("filter"), NULL, value = "", width = "100%", rows = 1)
      ),
      shiny::div(class = "filter-row",
        shiny::span("Exclude Rows", id = ns("label_filter_out"), class = "filter-label"),
        shiny::textAreaInput(ns("filter_out"), NULL, value = "", width = "100%", rows = 1)
      )
    ),

    shiny::actionButton(ns("clear"), "Clear Filters"),
    shiny::actionButton(ns("submit"), "Apply Filters"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::checkboxInput(
              ns("cols_all"),
              shiny::h4(
                shiny::tags$strong("Select/Deselect All"),
                style = "margin: 0; overflow-wrap: break-word;"
              ),
              TRUE
            ),
            shiny::div(
              class = "scrollable-checkbox",
              style = "max-height: 350px;",
              shiny::checkboxGroupInput(ns("columns"), "")
            )
          )
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::div(
              style = paste0(
                "display: flex; justify-content: space-between; ",
                "align-items: center; padding: 5px;"
              ),
              shiny::h4(
                shiny::tags$strong("Attribute Info:"),
                style = "margin: 0; overflow-wrap: break-word;"
              ),
              shiny::actionLink(
                ns("popout_meta"),
                label = "",
                icon = shiny::icon(
                  "glyphicon glyphicon-new-window",
                  lib = "glyphicon"
                )
              )
            ),
            shiny::div(
              class = "scrollable-checkbox",
              style = "max-height: 290px;",
              shiny::tableOutput(ns("metainfo"))
            )
          )
        ),
        width = 2
      ),

      shiny::mainPanel(
        shiny::tags$div(
          class = "table-wrapper",
          shiny::tags$div(
            # *** MODULARIZATION FIX: ID built from namespaced DT ID ***
            id = paste0("pagination_", ns("tbl")),
            class = "top-footer",
            shiny::div(
              style = paste0(
                "display:flex; align-items:center;",
                " gap:15px; padding-right: 20px;"
              ),
              shiny::div(
                shiny::strong("Total rows:"),
                shiny::textOutput(ns("totalrows"), inline = TRUE)
              ),
              shiny::div(
                shiny::strong("Total columns:"),
                shiny::textOutput(ns("totalcols"), inline = TRUE)
              ),
              shiny::div(
                shiny::strong("Filtered rows:"),
                shiny::textOutput(ns("filteredrows"), inline = TRUE)
              ),
              shiny::div(
                shiny::strong("Selected columns:"),
                shiny::textOutput(ns("selectedcols"), inline = TRUE)
              )
            ),
            shiny::div(
              style = paste0(
                "display:flex; align-items:center;",
                " gap:12px; margin-left: auto;"
              ),
              # Custom download buttons that export the current rendered dataset
              # (after filter + column selection), replacing the built-in DT
              # download button which only exported visible/paginated rows.
              shiny::div(
                class = "dt-custom-download-group",
                shiny::downloadButton(
                  ns("download_csv"),
                  label = "Download CSV",
                  class = "dt-custom-download-btn",
                  icon = shiny::icon("file-csv")
                ),
                shiny::downloadButton(
                  ns("download_excel"),
                  label = "Download Excel",
                  class = "dt-custom-download-btn dt-custom-download-btn-excel",
                  icon = shiny::icon("file-excel")
                )
              ),
              shiny::tags$div(style = "min-width: 120px;")
            )
          ),
          shiny::tags$div(
            id = ns("container"),
            class = "scrollable-data-container",
            DT::DTOutput(ns("tbl"))
          )
        )
      )
    ),
    shiny::tags$div(
      class = "custom-row-info",
      shiny::textOutput(ns("row_info"))
    )
  )
}
