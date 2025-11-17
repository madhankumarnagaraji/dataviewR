# Test configuration

# Suppress unnecessary output
options(shiny.testmode = TRUE)

# Set seed for reproducibility
set.seed(42)

# Suppress messages during testing
suppressMessages({
  library(dataviewR)
  library(dplyr)
  library(stringr)
})

# Skip tests if shinytest2 not available
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  message("shinytest2 not installed - some tests will be skipped")
}
