
# dataviewR

**Author**: N Madhan Kumar  
**Purpose**: A Shiny-based interactive viewer provides more features to view the dataframes and tibbles such as column selection, complex filtering, code reproducibility and attribute information.

---

## ✨ Features

- View any R data frame or tibble in a Shiny app
- Complex Filtering, Sorting, Column Selection and Attribute Information
- Auto-generate equivalent `dplyr` code
- Lightweight and easy to use

---

## 📦 Installation

To install the development version from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install dataviewR from GitHub
devtools::install_github("your-username/dataviewR")
```

---

## 🚀 Usage

```r
library(dataviewR)

# Launch the app with your data frame
dataviewer(mtcars)
dataviewer(pharmaverseadam::adae)
```

You can use any tibble or data.frame as input. The app provides:
- Complex filtering condition
- Interactive column filters (with NA values if present)
- Code generation for reproducibility
- Column selection
- Attibute information
- Responsive layout

---

## 📁 Folder Structure

- `R/` – Package functions  
- `man/` – Help files  
- `inst/app/` – Shiny UI and server logic (if applicable)  
- `vignettes/` – Long-form documentation (optional)

---

## 📄 License

This package is licensed under the MIT License.

---

## 🙋‍♂️ Author

Created and maintained by **N Madhan Kumar**.  
For any issues or feature requests, please [open an issue](https://github.com/madhankumarnagaraji/dataviewR/issues) or send an email to madhanmanoj1999@gmail.com.
