# Interactive Data Viewer with Filter and Code Generation

Launches a Shiny application to explore and filter a `data.frame` or
`tibble`. If no data is provided, it opens an import panel to load a
dataset from either the global environment or the packages.

## Usage

``` r
dataviewer(..., background = NULL, port = NULL)
```

## Arguments

- ...:

  One or more `data.frame` or `tibble` objects. If none provided, an
  import UI is shown to load data interactively.

- background:

  Logical. If `TRUE`, runs the app in a background R process using
  `callr`. Requires the `callr` package. Default is TRUE when dataset(s)
  are provided, FALSE when no dataset(s) are provided.

- port:

  Integer. Port number for the Shiny app. If `NULL`, a random available
  port is used. Default is NULL.

## Value

If `background = TRUE`, returns the process ID (character) invisibly. If
`background = FALSE`, returns a Shiny application object.

## Details

This function provides:

- A tab-based interface with data import and viewer options.

- Support for multiple datasets in separate tabs.

- A checkbox panel to select/deselect columns.

- An input for `dplyr`-compatible filter expressions.

- A dynamically generated `dplyr` code preview.

- Metadata display for the variables.

The filtering uses
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and generates user-friendly code to replicate the steps. It also
provides copyable R code that includes column selection and filtering
logic.

## Examples

``` r
if (interactive()) {
    dataviewer(mtcars) # Opens in RStudio Viewer pane or default web browser
    dataviewer(iris, mtcars) # Opens multiple datasets in separate tabs
    dataviewer() # Opens the "Import Dataset" tab (foreground mode - console will be busy)

    # Run in foreground to enable the "Import Dataset" tab alongside 'mtcars'
    dataviewer(mtcars, background = FALSE)

    # Stop background process
    id <- dataviewer(mtcars, iris)
    stop_dataviewer(id)
}
```
