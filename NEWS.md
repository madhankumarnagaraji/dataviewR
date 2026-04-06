# dataviewR 1.1.0

## 🔧 Updates

- Added pharmaverse badge in the README file as dataviewR is now a part of the pharmaverse set of packages.

- Now, the missing values will be shown as <NA> for character/factor variables and NA for all other datatypes. Also, updated the documentation vignettes for the same.

- select() function with all variables will not appear in the generated R code when all variables (columns) of a dataset are selected.

- Updated the Generated R code non-editable for the user.

- Added writexl package in the DESCRIPTION file.

- Added Total columns, Filtered rows, Selected columns counts next to Total rows for easy accessibility to the users.

- Increased the number of rows of the dataviewer to 500 from 10.

- In the Attribute Info table,
    - Updated the icons of integer, double and character variables.
    - Added icons for new datatypes/classes such as factor, logical, time and difftime (duration).
    - Added a space between the icon and the variable name.
    - Increased the size of the icons.

## 🐛 Bug Fixes

- Now, the logical values are coming in uppercase in the dataviewer. Earlier, because of the Javascript DT package, it was shown in lowercase.

- Added separate Download buttons for CSV and Excel files as the download button from the {DT} package is not working properly.

## 📝 Documentation

- Added writexl::write_xlsx() in the roxygen2 comments dataviewer.R file

- Updated the images and videos of the updated UI in the vignettes.

- Added "data.table" object in addition to "data.frame" and "tibble" objects.

- In the DESCRIPTION file, updated the minimum {shiny} version to 1.11.1 and added "data.table" in the package description.

# dataviewR 1.0.1

## 🐛 Bug Fixes

- Fixed "Connection refused" errors on RStudio Server/Cloud by implementing smart background polling.
- Fixed a race condition where the "Clear" button occasionally failed to reset the data filter.
- Improved stability of background process management.

# dataviewR 1.0.0

## 🔧 Updates
- Added **Multi-dataset support** to the `dataviewer()` function: users can now pass a list of datasets (#22).
- Implemented **background processing** using the `callr` package. The `dataviewer()` application now opens without blocking the R console (#22).
- Added new functions to manage the background processes: `list_dataviewers()`, `stop_dataviewer()`, and `stop_all_dataviewers()` (#26).
- Optimized the viewing layout with a compact design for a better user experience (#22).
- Added a pop-up option to the Attribute Info table (#22).
- Updated the `README.md` and added new vignettes ("Introduction to dataviewR", "Using Filter and Expressions", "Exploring Multiple Datasets", "Working with Clinical Datasets", "Exporting data and Wrapping Up the Session") (#10).
- Created the package website via GitHub Pages (#17).

## 🐛 Bug Fixes
- Fixed variable name handling in generated R code to support both conventional and non-conventional (e.g., with spaces) names (#22).
- Minor bug fixes to improve stability and performance.

## 📝 Documentation
- Added new authors and the GitHub Pages URL to the `DESCRIPTION` file.

# dataviewR 0.1.1

## 🔧 Updates
- Updated the GitHub repository link in the DESCRIPTION file.
- Updated the README file with more details.
- Increased the package version number to 0.1.1.
- Updated the name of the dataset in the generated R code.

## 🐛 Bug Fixes
- Minor bug fixes to improve stability and performance.

## 📝 Documentation
- Added a `NEWS.md` file to track package changes.

# dataviewR 0.1.0

- Initial release
