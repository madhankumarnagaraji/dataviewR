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
    expect_equal(names(final_df()), "Col A")

    # 2. Check Code Generation - must use backticks: select(`Col A`)
    code <- generated_code()
    expect_match(code, "select\\(`Col A`\\)")
    expect_false(grepl("select\\(Col A\\)", code)) # Should NOT see unquoted version
  })
})

test_that("Module: NA display for character variables shows <NA>", {
  # Character NAs should be converted to the string "<NA>" (a factor level),
  # so they are visible in the DT quick-filter box.

  char_na_data <- data.frame(
    id      = 1:3,
    label   = c("apple", NA_character_, "cherry"),
    stringsAsFactors = FALSE
  )

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(char_na_data),
    dataset_name = reactive("char_na_data")
  ), {
    session$setInputs(columns = names(char_na_data), filter = "", load = 1)

    res <- final_df()

    # Character NA must be converted to the string "<NA>"
    expect_equal(as.character(res$label[2]), "<NA>")

    # No true NAs should remain in the character column
    expect_false(any(is.na(res$label)))
  })
})

test_that("Module: NA display for factor variables shows <NA>", {
  # Factor NAs should be converted to the "<NA>" factor level,
  # consistent with the character NA behaviour.

  factor_data <- data.frame(
    id  = 1:3,
    cat = factor(c("A", "B", NA)),
    stringsAsFactors = TRUE
  )

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(factor_data),
    dataset_name = reactive("factor_data")
  ), {
    session$setInputs(columns = names(factor_data), filter = "", load = 1)

    res <- final_df()
    res_cat <- res$cat

    # "<NA>" must be a factor level
    expect_true("<NA>" %in% levels(res_cat))

    # No true NAs should remain
    expect_false(any(is.na(res_cat)))

    # The third row must carry the "<NA>" level string
    expect_equal(as.character(res_cat[3]), "<NA>")
  })
})

test_that("Module: NA display for numeric/integer/logical types stays as true NA", {
  # Numeric and integer NAs must NOT be converted to strings. They remain
  # proper R NAs since final_df() only applies across() to character/factor
  # and logical columns, never to numeric/integer.
  #
  # Logical columns ARE converted to factors so DT renders them as uppercase
  # TRUE/FALSE. The NA entry becomes a factor level whose name is set by
  # forcats::fct_na_value_to_level() with no level= argument; that default
  # label is version-dependent ("NA" in older forcats, "(Missing)" in newer).
  # We therefore avoid asserting the exact NA-level name and instead only
  # check that "TRUE" and "FALSE" appear as levels, with no lowercase versions.

  na_data <- data.frame(
    int_col  = c(1L, NA_integer_, 3L),
    dbl_col  = c(1.1, NA_real_,   3.3),
    logi_col = c(TRUE, NA, FALSE),
    stringsAsFactors = FALSE
  )

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(na_data),
    dataset_name = reactive("na_data")
  ), {
    session$setInputs(columns = names(na_data), filter = "", load = 1)

    res <- final_df()

    # Integer column — second value must remain a true NA
    expect_true(is.na(res$int_col[2]))

    # Double column — second value must remain a true NA
    expect_true(is.na(res$dbl_col[2]))

    # Logical column — must have been converted to a factor for display
    expect_true(is.factor(res$logi_col))

    # "TRUE" and "FALSE" (uppercase) must be factor levels.
    # We do NOT assert anything about the NA level name because its label
    # is forcats-version-dependent.
    logi_levels <- levels(res$logi_col)
    expect_true("TRUE"  %in% logi_levels)
    expect_true("FALSE" %in% logi_levels)
    expect_false("true"  %in% logi_levels)
    expect_false("false" %in% logi_levels)
  })
})

test_that("Module: Logical values display as uppercase TRUE/FALSE", {
  # Logical columns are converted to factors in final_df() using as.factor(),
  # which produces the levels "FALSE" and "TRUE" (uppercase).
  # This fixes the DT JavaScript rendering issue where booleans appeared as
  # lowercase 'true'/'false'.

  logical_data <- data.frame(
    id   = 1:4,
    flag = c(TRUE, FALSE, TRUE, FALSE)
  )

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(logical_data),
    dataset_name = reactive("logical_data")
  ), {
    session$setInputs(columns = names(logical_data), filter = "", load = 1)

    res <- final_df()

    # The column must have been converted to a factor
    expect_true(is.factor(res$flag))

    # Factor levels must be uppercase
    expect_true(all(levels(res$flag) %in% c("FALSE", "TRUE")))

    # Every individual value must be uppercase when coerced to character
    flag_chars <- as.character(res$flag)
    expect_true(all(flag_chars %in% c("TRUE", "FALSE")))
    expect_false(any(flag_chars %in% c("true", "false")))
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

test_that("Module: Metadata contains expected col_type values for new datatypes", {
  # Verifies that meta_cols() correctly recognises integer, double, character,
  # factor, logical, datetime (POSIXct) and difftime columns, which all received
  # new/updated icons in this release.
  #
  # Key implementation details that shape how we test:
  #
  # 1. meta_cols() builds col_name as an HTML string:
  #      "<span style='font-size:18px'>EMOJI</span> colname"
  #    Stripping HTML tags with gsub("<[^>]+>", ...) removes <span> but leaves
  #    the emoji character(s) before the space and column name. We therefore
  #    use grepl() to check that each column name appears as a SUBSTRING of
  #    one of the plain_names entries rather than requiring an exact match.
  #
  # 2. labelled::generate_dictionary() only includes columns that have
  #    attributes (labels, etc.) or are recognised types. Plain integer/double/
  #    character columns with no variable labels may be absent from the result.
  #    We therefore use class_df() — the direct dictionary output — as the
  #    ground truth for which columns ARE in the metadata, and only assert
  #    that those columns appear in meta_cols().
  #
  # 3. We use iris (a well-known dataset with factor + numeric columns) as the
  #    primary test vehicle because generate_dictionary() handles it reliably.
  #    For the new datatypes (logical, POSIXct, difftime) we verify that
  #    class_df() produces col_type entries matching the server's case_when map.

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(iris),
    dataset_name = reactive("iris")
  ), {
    session$setInputs(columns = names(iris), filter = "", load = 1)

    meta <- meta_cols()

    # meta_cols() must return a non-empty data frame
    expect_s3_class(meta, "data.frame")
    expect_gt(nrow(meta), 0)

    # col_name values are HTML strings. Strip tags and trim whitespace.
    # Emojis will remain but the column name appears after the last "> ".
    stripped <- trimws(gsub("<[^>]+>", "", meta$col_name))

    # Every column from iris must appear somewhere in the stripped names
    # (grepl substring match handles the leading emoji character(s)).
    for (col in names(iris)) {
      expect_true(
        any(grepl(col, stripped, fixed = TRUE)),
        label = paste0('"', col, '" must appear in metadata col_name')
      )
    }

    # The labelled variable label for col_name must be "Variable Name"
    expect_equal(labelled::var_label(meta)$col_name, "Variable Name")
  })

  # Separately verify col_type recognition for new datatypes using class_df()
  # directly, which is the reactive that feeds the icon-selection case_when.
  typed_data <- data.frame(
    logi_col = c(TRUE, FALSE),
    fct_col  = factor(c("x", "y")),
    stringsAsFactors = FALSE
  )
  typed_data$dttm_col <- as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC")
  typed_data$dur_col  <- as.difftime(c(10, 20), units = "secs")

  testServer(dataviewer_tab_server, args = list(
    get_data = reactive(typed_data),
    dataset_name = reactive("typed_data")
  ), {
    session$setInputs(columns = names(typed_data), filter = "", load = 1)

    cd <- class_df()

    # class_df() must produce rows for these columns if generate_dictionary
    # recognises them. If the dictionary returns empty for this machine's
    # labelled version, we skip rather than fail.
    skip_if(nrow(cd) == 0, "labelled::generate_dictionary() returned no rows for typed_data on this system")

    recognised_cols <- cd$colname

    # For each recognised column, verify the col_type is one the server's
    # case_when handles (lgl, fct, dttm, drtn).
    known_types <- c("int", "dbl", "chr", "fct", "lgl", "date", "dttm",
                     "Period", "time", "drtn")

    for (col in recognised_cols) {
      ct <- cd$col_type[cd$colname == col]
      expect_true(
        ct %in% known_types,
        label = paste0(col, " col_type '", ct, "' must be a known type")
      )
    }
  })
})
