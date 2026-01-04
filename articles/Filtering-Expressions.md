# Using Filter and Expressions

## Filtering & Expressions in dataviewR

Filtering in `dataviewR` is done using a single powerful method:  
writing a **`dplyr` compatible expression** in the filter box.

This gives you complete flexibility while keeping the logic clean and
reproducible.

------------------------------------------------------------------------

### 1. Launch dataviewR with a dataset

``` r
library(dataviewR)

dataviewer(iris)
```

Once the app opens, you’ll see a Filter text box where you can type any
valid expression similar to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

### 2. Filtering with Expressions

You can write any filtering condition that you would normally pass to:

``` r
dplyr::filter(...)
```

Basic comparisons

``` mathematica
Sepal.Length > 5
```

Multiple conditions

``` mathematica
Sepal.Length > 5 & Species == "virginica"
```

Using %in%

``` nginx
Species %in% c("setosa", "virginica")
```

Handling missing values

``` csharp
is.na(Petal.Width)
```

String matching

``` scss
grepl("^s", Species)
```

When you click Submit, the expression is evaluated and the dataset
updates.

Invalid expressions show a friendly error notification.

### 3. Re-running, clearing, or updating filters

- Submit → runs the filter
- Clear → resets the filter box

The display updates immediately after submitting.

### 4. How filtering affects the generated R code

Whenever you apply a filter, the exported code reflects exactly what you
typed:

``` r
iris |>
  filter(Species == "setosa" & Sepal.Length > 5) |>
  select(Sepal.Length, Sepal.Width, Species)
```

Filtering always appears before column selection in the generated R
code.

### 5. Tips for Writing Expressions

- Use & instead of &&
- Use %in% for selecting multiple values
- Variable names are case-sensitive
- Treat the filter box like the
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  function
- If something fails, try running your expression directly in the R
  console first

### 6. Note on Quick Filter and Quick Search

The quick filter box (placed below the variable name) will helps to
quickly search for a value in the variable. For character/factor
variable(s) - it shows the distinct values of the variable(s) including
the **NA** values. For numeric variable(s) - it shows an interactive
draggable slider with minimum and maximum values of the variable(s).
These do not reflect in the generated R code as filtering logic is
solely depends on the Filter expression box. At present, NA values in
numeric variables appear as blanks as per DT package formatting but this
shall be fixed in a future enhancement.

The quick search box allows you to quickly check whether a value exists
in the dataset. It searches only within variable values, not variable
names/attributes.

### Summary

In this article, you learned:  
- `dataviewR` uses expression-based filtering system  
- Expressions must be valid similar to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
function  
- The filtered result updates on Submit  
- Exported code reflects your filter exactly  
- Quick filters help browsing but do not contribute to filtering logic  

Expression filtering gives users full flexibility and keeps the workflow
reproducible.

### Next Article

Continue with: [Exploring Multiple
Datasets](https://madhankumarnagaraji.github.io/dataviewR/articles/Multiple-Datasets.md)
