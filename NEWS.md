# dataviewR 1.0.2

## 🔧 Updates
- Now, the missing values are showing as <NA> for character/factor datatypes and NA for all other datatypes

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
