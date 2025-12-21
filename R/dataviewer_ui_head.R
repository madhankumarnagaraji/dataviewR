#' Internal function to create the UI head which contains all static CSS and JavaScript
#' @noRd
dataviewer_ui_head <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("

          /* --- NEW: Full Width CSS to override Shiny's default container padding --- */

          .full-width .container-fluid,
          .full-width .row {
            padding-left: 0 !important;
            padding-right: 0 !important;
            margin-left: 0 !important;
            margin-right: 0 !important;
          }

          .full-width .col-sm-12 {
            padding: 0;
          }

          /* --- END NEW: Full Width CSS --- */
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

          /* Layout: ensure sidebarLayout and mainPanel behave normally */
          .shiny-split-layout, .sidebar-layout {
            width: 100%;
          }

          /* Table + footer wrapper: mainPanel will contain a table wrapper; footer is placed above the table now */
          .table-wrapper {
            display: flex;
            flex-direction: column;
            width: 100%;
            box-sizing: border-box;
          }

          /* Top footer area (now placed above the table) */
          .top-footer {
            width: 100%;
            padding: 8px 12px;
            background: #ffffff;
            border-bottom: 1px solid #e1e1e1;
            display: flex;
            align-items: center;
            justify-content: space-between;
            box-sizing: border-box;
            margin-bottom: 8px;
            /* z-index lowered so Bootstrap/Shiny modals appear above it */
            z-index: 1;
            position: relative;
            flex: 0 0 auto;
          }

          .top-footer .dataTables_info,
          .top-footer .dataTables_paginate {
            display: inline-block;
            vertical-align: middle;
          }

          /* Scrollable data container - this must be the only scrolling element for the table area */
          .scrollable-data-container {
            overflow: auto; /* vertical/horizontal scrolls live here */
            width: 125%;
            max-height: 650px;
            position: relative;
            flex: 1 1 auto;
            min-width: 0; /* allow shrink in flex layouts */
          }

          .scrollable-data-container .dataTables_wrapper {
            width: 100%;
            overflow: visible;
          }
          .scrollable-data-container table.dataTable {
            width: 100% !important;
            margin: 0 !important;
          }

          /* Sticky thead inside scrollable container */
          .scrollable-data-container table.dataTable thead {
            position: sticky;
            top: 0;
            z-index: 10;
            background-color: white;
          }

          /* Prevent auto-scroll on focus for DataTable inputs */
          .dataTables_wrapper input[type='search'] {
            scroll-margin: 0 !important;
            scroll-padding: 0 !important;
          }

          /* Custom row info styling */
          .custom-row-info {
            margin-top: 15px;
            padding: 10px;
            font-size: 14px;
            color: #333;
            background-color: #f9f9f9;
            border: 1px solid #ddd;
            border-radius: 4px;
            display: inline-block;
          }

          .scrollable-checkbox{
            overflow-y: scroll;
            border: 1px solid #ccc;
            padding: 1px;
            background-color: #ffffff;
            overscroll-behavior: contain;
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

        ")),
    shiny::tags$script(shiny::HTML("
          // When Shiny connects, set up listeners and helpers
          $(document).on('shiny:connected', function() {

            // Prevent scroll propagation from small scrollable checkboxes
            $(document).on('mousedown wheel touchstart', '.scrollable-checkbox', function(e) {
              e.stopPropagation();
            });

            // Prevent focus events from causing scroll
            $(document).on('focus', '.scrollable-checkbox input[type=\"checkbox\"]', function(e) {
              e.stopPropagation();
            });

            // Prevent DataTables filter inputs from calling scrollIntoView
            var originalScrollIntoView = Element.prototype.scrollIntoView;
            Element.prototype.scrollIntoView = function(arg) {
              try {
                if (this.tagName === 'INPUT' && this.type === 'search' && $(this).closest('.dataTables_wrapper').length > 0) {
                  return false;
                }
              } catch (e) {}
              return originalScrollIntoView.call(this, arg);
            };

            // Utility: move info+paginate into a top footer with retries
            window.moveDTFooterToTop = function(tableId) {
              try {
                var tableEl = $('#' + tableId);
                if (tableEl.length === 0) return false;
                var wrapper = tableEl.closest('.dataTables_wrapper');
                if (wrapper.length === 0) return false;

                var info = wrapper.find('.dataTables_info').first();
                var paginate = wrapper.find('.dataTables_paginate').first();

                // *** MODULARIZATION FIX: Find pagination div by its namespaced ID ***
                var topFooter = $('#pagination_' + tableId);
                if (topFooter.length === 0) {
                  return false;
                }

                // Detach and append (preserve handlers)
                if (info.length) topFooter.append(info.detach());
                if (paginate.length) topFooter.append(paginate.detach());

                return true;
              } catch (e) {
                console && console.warn && console.warn('moveDTFooterToTop error', e);
                return false;
              }
            };

            // Provide a retry mechanism because DT might render a bit later
            window.ensureDTFooterMovedTop = function(tableId) {
              var attempts = 0;
              var maxAttempts = 30;
              var intv = setInterval(function() {
                attempts++;
                if (window.moveDTFooterToTop(tableId) || attempts >= maxAttempts) {
                  clearInterval(intv);
                }
              }, 100); // try for ~3 seconds
            };

            // === START FIX: Corrected jQuery selector for Enter key ===
            // A more robust way for modules is to find the submit button
            // relative to the filter input.
            $(document).on('keydown', \"input[type='text'][id$='-filter']\", function(e) {
              if (e.key === 'Enter' || e.keyCode === 13) {
                e.preventDefault();
                // FIX: Go to the parent DIV (.form-group), then find the sibling button
                var submitButton = $(this).parent().siblings(\"button[id$='-submit']\");
                if (submitButton.length > 0) {
                  submitButton.click();
                }
              }
            });
            // === END FIX ===

          });
        "))
  )
}
