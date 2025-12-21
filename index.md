# dataviewR [![dataviewR website](reference/figures/dataviewR_logo.png)](https://madhankumarnagaraji.r-universe.dev/dataviewR)

**An Interactive and Feature-Rich Data Viewer for R**

`dataviewR` is a powerful Shiny-based interactive data viewer that
transforms the way you explore R data frames and tibbles. With its
intuitive interface and advanced features, it provides comprehensive
data exploration capabilities with automatic code generation for
reproducible workflows.

------------------------------------------------------------------------

## ✨ Key Features

- **Interactive Data Exploration**: View any R data frame or tibble in
  an elegant Shiny interface
- **Advanced Filtering**: Apply complex dplyr-compatible filter
  expressions with live preview
- **Dynamic Column Selection**: Easily select and deselect columns with
  checkbox interface
- **Automatic Code Generation**: Generate reproducible `dplyr` code for
  all your data manipulations
- **Metadata Display**: Access detailed variable information and data
  attributes
- **Data Import Panel**: Built-in data import functionality when no data
  is provided
- **Responsive Design**: Clean, modern interface that works across
  different screen sizes
- **Export Capabilities**: Save your filtered and selected data for
  further analysis

------------------------------------------------------------------------

## 📦 Installation

### Stable Release (CRAN)

``` r
# Install from CRAN (recommended for most users)
install.packages("dataviewR")
```

### For Older R Versions (\< 4.3.0)

If you’re using R versions prior to 4.3.0, you may need to install from
source:

``` r
# For R 4.2.x and earlier
install.packages("dataviewR", type = "source")

# Alternative method using devtools
devtools::install_version("dataviewR", version = "0.1.0", 
                         repos = "http://cran.r-project.org")
```

### Development Version

``` r
# Install the latest development version from GitHub
# install.packages("devtools")
devtools::install_github("madhankumarnagaraji/dataviewR")
```

### Alternative Installation Methods

``` r
# Install from R-universe (often has additional binary support)
install.packages("dataviewR", 
                 repos = c("https://madhankumarnagaraji.r-universe.dev", 
                          "https://cloud.r-project.org"))
```

------------------------------------------------------------------------

## 🚀 Quick Start

``` r
library(dataviewR)

# Launch with built-in datasets
dataviewer(mtcars)
dataviewer(iris)

# Launch with your own data
dataviewer(your_data_frame)

# Launch without data to use the import panel
dataviewer()

# Example with pharmaverseadam data (if package available)
# install.packages("pharmaverseadam")
dataviewer(pharmaverseadam::adae)
```

### What You Can Do

The interactive interface provides: - **Complex Filtering**: Use dplyr
expressions like `mpg > 20 & cyl == 4` - **Column Management**:
Select/deselect columns with intuitive checkboxes - **Data Sorting**:
Click column headers to sort data - **Code Export**: Copy generated
dplyr code for reproducible analysis - **Attribute Inspection**: View
variable labels, types, and metadata - **Search Functionality**: Find
specific values across all columns - **Variable Levels with NA
Visibility**: Displays all distinct values of character and factor
variables, including `NA`, within the quick filter box.

------------------------------------------------------------------------

## 🎯 Use Cases

### **Data Exploration**:

Perfect for initial data investigation and understanding

------------------------------------------------------------------------

## 📁 Package Structure

    dataviewR/
    ├── R/
    │   └── dataviewer.R
    ├── man/
    │   └── dataviewer.Rd
    │   └── figures/
    │       └── dataviewR_logo.png
    ├── tests/
    │   └── testthat/
    │       └── test-dataviewer.R
    ├── vignettes/
    │   └── dataviewR-intro.Rmd
    ├── inst/
    │   └── app/
    ├── data/
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── README.md
    ├── NEWS.md
    ├── LICENSE

------------------------------------------------------------------------

## 📊 System Requirements

- **R Version**: ≥ 4.2.0 (tested on R 4.2.0 through R 4.4.x)
- **Dependencies**: All dependencies are automatically installed
  - `shiny`, `shinyjs`, `DT`, `dplyr`, `labelled`, `forcats`
  - `stringr`, `purrr`, `tibble`, `datamods`, `htmlwidgets`

------------------------------------------------------------------------

### Integration with Analysis Workflows

``` r
# Use dataviewR to explore, then copy the generated code
library(dplyr)

# After using dataviewer(mtcars), you might get code like:
filtered_data <- mtcars %>%
  filter(mpg > 20 & cyl == 4) %>%
  select(mpg, cyl, hp, wt)
```

------------------------------------------------------------------------

## 📖 Documentation

- **Function Reference**:
  [`?dataviewer`](https://madhankumarnagaraji.github.io/dataviewR/reference/dataviewer.md)
  or
  [`help(dataviewer)`](https://madhankumarnagaraji.github.io/dataviewR/reference/dataviewer.md)
- **Package Vignette**:
  [`vignette("dataviewR-intro", package = "dataviewR")`](https://madhankumarnagaraji.github.io/dataviewR/articles/dataviewR-intro.md)
- **CRAN Page**: <https://cran.r-project.org/package=dataviewR>
- **R-universe Page**:
  <https://madhankumarnagaraji.r-universe.dev/dataviewR>

------------------------------------------------------------------------

## 🐛 Issue Reporting & Support

Found a bug or have a feature request?

- **GitHub Issues**: [Open an
  issue](https://github.com/madhankumarnagaraji/dataviewR/issues)
- **Email**: <madhanmanoj1999@gmail.com>
- **Bug Reports**: Please include your R version, operating system, and
  reproducible example

When reporting issues, please include: 1. Your R version (`R.version`)
2. Package version (`packageVersion("dataviewR")`) 3. Operating system
4. Reproducible example demonstrating the issue

------------------------------------------------------------------------

## 🤝 Contributing

Contributions are welcome! Please feel free to: - Report bugs and
request features via GitHub Issues - Submit pull requests for bug fixes
or enhancements - Improve documentation and examples - Share use cases
and feedback

------------------------------------------------------------------------

## 📝 Citation

If you use dataviewR in your research or publications, please cite:

``` r
citation("dataviewR")
```

    To cite dataviewR in publications use:

      Madhan Kumar N (2025). dataviewR: An Interactive and Feature-Rich Data Viewer.
      R package version 0.1.0. https://CRAN.R-project.org/package=dataviewR

    A BibTeX entry for LaTeX users is:

      @Manual{,
        title = {dataviewR: An Interactive and Feature-Rich Data Viewer},
        author = {Madhan Kumar N},
        year = {2025},
        note = {R package version 0.1.0},
        url = {https://CRAN.R-project.org/package=dataviewR},
      }

------------------------------------------------------------------------

## 📄 License

This package is licensed under the [MIT
License](https://madhankumarnagaraji.github.io/dataviewR/LICENSE).

    MIT License

    Copyright (c) 2025 Madhan Kumar N

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

------------------------------------------------------------------------

## 🌟 Acknowledgments

- Built with [Shiny](https://shiny.posit.co/) for the interactive
  interface
- Powered by [DT](https://rstudio.github.io/DT/) for advanced data
  tables
- Utilizes [dplyr](https://dplyr.tidyverse.org/) for data manipulation
- Special thanks to the R community for feedback and suggestions

------------------------------------------------------------------------

## 📈 Development Status

### Roadmap

Future enhancements may include:

- Multi-dataset tabs for viewing multiple datasets side by side within
  the same session.
- Bug fixes, if any
- Additional customization options

------------------------------------------------------------------------

**Created and maintained by [N Madhan
Kumar](mailto:madhanmanoj1999@gmail.com)**

*Making R data exploration more interactive and intuitive.* ✨
