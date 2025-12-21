library(testthat)
library(shiny)
library(dplyr)

test_that("Module: Handles Column Names with Spaces (Quoting)", {
  # Create data with "messy" names
  messy_data <- data.frame(
    "Col A" = 1:5,
    "Col-B" = letters[1:5],
    check.names = FALSE
  )

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(messy_data),
    dataset_name = reactive("messy_data")
  ), {
    # Initialize
    session$setInputs(columns = names(messy_data), filter = "", load = 1)

    # Select the column with a space
    session$setInputs(columns = c("Col A"))

    # 1. Check Dataframe Subset
    # The internal logic handles standard selection, but we verify it works on weird names
    expect_equal(names(final_df()), "Col A")

    # 2. Check Code Generation (The Critical Part)
    # It must generate backticks: select(`Col A`)
    code <- generated_code()
    expect_match(code, "select\\(`Col A`\\)")
    expect_false(grepl("select\\(Col A\\)", code)) # Should NOT see unquoted version
  })
})

test_that("Module: Handles NA in Factors", {
  # Your code explicitly converts NA factors to "<NA>" levels.
  # This verifies that logic works.

  factor_data <- data.frame(
    id = 1:3,
    cat = factor(c("A", "B", NA)),
    stringsAsFactors = TRUE
  )

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(factor_data),
    dataset_name = reactive("factor_data")
  ), {
    # Initialize
    session$setInputs(columns = names(factor_data), filter = "", load = 1)

    res <- final_df()

    # Extract the 'cat' column
    res_cat <- res$cat

    # Verify NA was converted to a level named "<NA>"
    # This logic comes from your use of forcats::fct_na_value_to_level
    expect_true("<NA>" %in% levels(res_cat))
    expect_false(any(is.na(res_cat))) # Should be no actual NAs left
    expect_equal(as.character(res_cat[3]), "<NA>")
  })
})

test_that("Module: Generates valid code for complex filters", {
  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(mtcars),
    dataset_name = reactive("mtcars")
  ), {
    session$setInputs(columns = names(mtcars), filter = "", load = 1)

    # Test %in% operator
    session$setInputs(filter = "cyl %in% c(4, 6)")
    session$setInputs(submit = 1)

    # Check data
    expect_true(all(filter_df()$cyl %in% c(4, 6)))

    # Check code
    code <- generated_code()
    expect_match(code, "filter\\(cyl %in% c\\(4, 6\\)\\)")
  })
})
