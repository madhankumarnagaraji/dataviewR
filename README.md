# dataviewR <a href="https://madhankumarnagaraji.r-universe.dev/dataviewR"><img src="man/figures/dataviewR_logo.png" align="right" width="180" alt="dataviewR logo" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/dataviewR)](https://CRAN.R-project.org/package=dataviewR)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/dataviewR?color=blue)](https://r-pkg.org/pkg/dataviewR)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![DOI](https://img.shields.io/badge/DOI-10.32614/CRAN.package.dataviewR-blue.svg)](https://doi.org/10.32614/CRAN.package.dataviewR)
[![R-universe](https://madhankumarnagaraji.r-universe.dev/badges/dataviewR)](https://madhankumarnagaraji.r-universe.dev/dataviewR)
<!-- badges: end -->

**An Interactive and Feature-Rich Data Viewer for R**

`dataviewR` is a powerful Shiny-based interactive data viewer that transforms the way you explore R data frames and tibbles. With its intuitive interface and advanced features, it provides comprehensive data exploration capabilities with automatic code generation for reproducible workflows.

---

## ✨ Key Features

- **Interactive Data Exploration**: View any R data frame or tibble in an elegant Shiny interface
- **Multi-Dataset Support** — Compare / View multiple datasets at once, e.g. `dataviewer(iris, mtcars)`  
- **Advanced Filtering**: Apply complex dplyr-compatible filter expressions with live preview
- **Dynamic Column Selection**: Easily select and deselect columns with checkbox interface
- **Automatic Code Generation**: Generate reproducible `dplyr` code for all your data manipulations
- **Metadata Display**: Access detailed variable information and data attributes
- **Import Panel**: Built-in data import functionality when no data is provided
- **Responsive Design**: Clean, modern interface that works across different screen sizes
- **Export Capabilities**: Save your filtered and selected data for further analysis

---

Below is a complete tutorial about the package and its key features as explained above

## 📦 Installation

Install the stable release from CRAN:

```r
install.packages("dataviewR")
```

Or the latest development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("madhankumarnagaraji/dataviewR")
```

Or from R-universe (binary builds available):

```r
install.packages("dataviewR", 
                 repos = c("https://madhankumarnagaraji.r-universe.dev", 
                          "https://cloud.r-project.org"))
```

---

## 🚀 Quick Start

```r
library(dataviewR)

# Launch with a dataset
dataviewer(iris)

# View multiple datasets at once
dataviewer(iris, mtcars)

# or launch without data to use the import panel
dataviewer()
```

## Learn More

- [Get Started Guide](articles/Introduction-to-dataviewR.html) — overview and visuals  
- [User Articles](articles/) — feature demos and tutorials  
- [Reference Documentation](reference/)  

## Contributing & Support

We welcome contributions, ideas, and issue reports!  
Visit the [GitHub repository](https://github.com/madhankumarnagaraji/dataviewR)  
or open an [issue here](https://github.com/madhankumarnagaraji/dataviewR/issues).

License

This package is licensed under the [MIT License](LICENSE).

*Making R data exploration more interactive and intuitive.* ✨
