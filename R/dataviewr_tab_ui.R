#' Internal function for data viewer tab UI
#' @param id The module's namespace ID.
#' @noRd
dataviewr_tab_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::br(),
    shiny::actionButton(ns("load"), "Refresh the Data"),
    shiny::actionButton(ns("generate_code"), "Generate R Code"),
    shiny::h4(shiny::tags$strong("Filter")),
    shiny::textInput(ns("filter"), NULL, value = "", width = "40%"),
    shiny::actionButton(ns("clear"), "Clear"),
    shiny::actionButton(ns("submit"), "Submit"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(shiny::column(12,
                                      shiny::checkboxInput(ns("cols_all"), shiny::h4(shiny::tags$strong("Select/Deselect All"), style = "margin: 0; overflow-wrap: break-word;"), TRUE),
                                      shiny::div(class = "scrollable-checkbox",
                                                 style = "max-height: 350px;",
                                                 shiny::checkboxGroupInput(ns("columns"), "")
                                      )
        )),
        shiny::br(),
        shiny::fluidRow(shiny::column(12,
                                      shiny::div(
                                        style = "display: flex; justify-content: space-between; align-items: center; padding: 5px;",
                                        shiny::h4(shiny::tags$strong("Attribute Info:"), style = "margin: 0; overflow-wrap: break-word;"),
                                        shiny::actionLink(ns("popout_meta"), label="", icon = shiny::icon("glyphicon glyphicon-new-window",lib = "glyphicon"))
                                      ),
                                      shiny::div(class = "scrollable-checkbox",
                                                 style = "max-height: 290px;",
                                                 shiny::tableOutput(ns("metainfo"))
                                      )
        )),
        width = 2
      ),
      shiny::mainPanel(
        shiny::tags$div(
          class = "table-wrapper",
          shiny::tags$div(
            # *** MODULARIZATION FIX: ID must be built from the namespaced DT ID ***
            id = paste0("pagination_", ns("tbl")),
            class = "top-footer",
            shiny::div(
              style = "display:flex; align-items:center; gap:8px;",
              shiny::strong("Total rows:"),
              shiny::textOutput(ns("totalrows"), inline = TRUE)
            ),
            shiny::div(
              style = "display:flex; align-items:center; gap:12px;",
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
