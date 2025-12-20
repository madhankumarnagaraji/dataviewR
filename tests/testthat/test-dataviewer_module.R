library(testthat)
library(shiny)
library(dplyr)
library(labelled) # Ensure this is loaded for the metadata test

test_that("Module: Data loading and initialization", {

  # Mock data
  test_data <- mtcars
  test_data$cyl <- as.factor(test_data$cyl)

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(test_data),
    dataset_name = reactive("mtcars")
  ), {
    # Simulate initialization inputs
    session$setInputs(columns = names(test_data))

    # Check if total rows are calculated correctly
    expect_equal(output$totalrows, format(nrow(test_data), big.mark = ","))

    # Check if columns matches the input we set
    expect_equal(sort(input$columns), sort(names(test_data)))
  })
})

test_that("Module: Filtering Logic", {
  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(mtcars),
    dataset_name = reactive("mtcars")
  ), {
    # Initialize inputs AND trigger the eventReactive with 'load'
    session$setInputs(
      columns = names(mtcars),
      filter = "",
      load = 1
    )

    # Set a filter and trigger submit
    session$setInputs(filter = "cyl == 4")
    session$setInputs(submit = 1)

    # Verify the internal filtered dataframe
    filtered_res <- filter_df()
    expect_equal(nrow(filtered_res), 11)
    expect_true(all(filtered_res$cyl == 4))

    # Test Clear
    session$setInputs(clear = 1)
    session$setInputs(filter = "") # Simulate browser clearing input

    expect_equal(nrow(filter_df()), 32) # Should reset to full data
    expect_equal(input$filter, "")
  })
})

test_that("Module: Invalid Filter Handling", {
  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(mtcars),
    dataset_name = reactive("mtcars")
  ), {
    # Initialize
    session$setInputs(columns = names(mtcars), filter = "", load = 1)

    # Input invalid R syntax
    session$setInputs(filter = "cyl == ") # Incomplete
    session$setInputs(submit = 1)

    # Should default back to original data safely
    expect_equal(nrow(filter_df()), 32)

    # Input dangerous syntax
    session$setInputs(filter = "system('ls')")
    session$setInputs(submit = 1)
    expect_equal(nrow(filter_df()), 32)
  })
})

test_that("Module: Column Selection", {
  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(mtcars),
    dataset_name = reactive("mtcars")
  ), {
    # Initialize inputs AND trigger load
    # Triggers 'filter_df', allowing 'cols_df' and 'final_df' to run
    session$setInputs(columns = names(mtcars), filter = "", load = 1)

    expect_equal(ncol(final_df()), 11)

    # Deselect some columns
    session$setInputs(columns = c("mpg", "cyl"))

    # Check final dataframe
    expect_equal(names(final_df()), c("mpg", "cyl"))
    expect_equal(ncol(final_df()), 2)

    # Deselect ALL columns
    session$setInputs(columns = character(0))
    expect_null(final_df())
  })
})

test_that("Module: R Code Generation", {
  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(mtcars),
    dataset_name = reactive("mtcars")
  ), {
    # Initialize
    session$setInputs(columns = names(mtcars), filter = "", load = 1)

    # Case 1: No changes
    code <- generated_code()
    expect_match(code, "# Generated R Code")
    expect_match(code, "library\\(dplyr\\)")
    expect_match(code, "mtcars")
    expect_false(grepl("filter", code))

    # Case 2: Filter only
    session$setInputs(filter = "mpg > 20")
    session$setInputs(submit = 1)
    code <- generated_code()
    expect_match(code, "filter\\(mpg > 20\\)")

    # Case 3: Select only
    session$setInputs(filter = "")
    session$setInputs(columns = c("mpg", "cyl"))
    code <- generated_code()
    expect_match(code, "select\\(mpg, cyl\\)")

    # Case 4: Filter AND Select
    session$setInputs(filter = "mpg > 20")
    session$setInputs(submit = 1) # Resubmit filter
    code <- generated_code()
    expect_match(code, "filter\\(mpg > 20\\)")
    expect_match(code, "select\\(mpg, cyl\\)")
    expect_match(code, "\\|>")
  })
})

test_that("Module: Metadata Extraction", {
  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(iris),
    dataset_name = reactive("iris")
  ), {
    # Initialize
    session$setInputs(columns = names(iris), filter = "", load = 1)

    # Check if meta_cols reactive works
    meta <- meta_cols()
    expect_s3_class(meta, "data.frame")

    # Check labels using labelled package logic
    labels <- labelled::var_label(meta)

    # We expect the column 'col_name' to have the label "Variable Name"
    expect_equal(labels$col_name, "Variable Name")

    # Check if raw columns exist
    expect_true("col_name" %in% names(meta))

    # Verify row content (Species row should exist)
    species_rows <- meta[grepl("Species", meta$col_name), ]
    expect_gt(nrow(species_rows), 0)
  })
})
