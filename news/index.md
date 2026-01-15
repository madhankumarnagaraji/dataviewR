# Changelog

## dataviewR 1.0.0

CRAN release: 2026-01-10

### 🔧 Updates

- Added **Multi-dataset support** to the
  [`dataviewer()`](https://madhankumarnagaraji.github.io/dataviewR/reference/dataviewer.md)
  function: users can now pass a list of datasets
  ([\#22](https://github.com/madhankumarnagaraji/dataviewR/issues/22)).
- Implemented **background processing** using the `callr` package. The
  [`dataviewer()`](https://madhankumarnagaraji.github.io/dataviewR/reference/dataviewer.md)
  application now opens without blocking the R console
  ([\#22](https://github.com/madhankumarnagaraji/dataviewR/issues/22)).
- Added new functions to manage the background processes:
  [`list_dataviewers()`](https://madhankumarnagaraji.github.io/dataviewR/reference/list_dataviewers.md),
  [`stop_dataviewer()`](https://madhankumarnagaraji.github.io/dataviewR/reference/stop_dataviewer.md),
  and
  [`stop_all_dataviewers()`](https://madhankumarnagaraji.github.io/dataviewR/reference/stop_all_dataviewers.md)
  ([\#26](https://github.com/madhankumarnagaraji/dataviewR/issues/26)).
- Optimized the viewing layout with a compact design for a better user
  experience
  ([\#22](https://github.com/madhankumarnagaraji/dataviewR/issues/22)).
- Added a pop-up option to the Attribute Info table
  ([\#22](https://github.com/madhankumarnagaraji/dataviewR/issues/22)).
- Updated the `README.md` and added new vignettes (“Introduction to
  dataviewR”, “Using Filter and Expressions”, “Exploring Multiple
  Datasets”, “Working with Clinical Datasets”, “Exporting data and
  Wrapping Up the Session”)
  ([\#10](https://github.com/madhankumarnagaraji/dataviewR/issues/10)).
- Created the package website via GitHub Pages
  ([\#17](https://github.com/madhankumarnagaraji/dataviewR/issues/17)).

### 🐛 Bug Fixes

- Fixed variable name handling in generated R code to support both
  conventional and non-conventional (e.g., with spaces) names
  ([\#22](https://github.com/madhankumarnagaraji/dataviewR/issues/22)).
- Minor bug fixes to improve stability and performance.

### 📝 Documentation

- Added new authors and the GitHub Pages URL to the `DESCRIPTION` file.

## dataviewR 0.1.1

CRAN release: 2025-07-21

### 🔧 Updates

- Updated the GitHub repository link in the DESCRIPTION file.
- Updated the README file with more details.
- Increased the package version number to 0.1.1.
- Updated the name of the dataset in the generated R code.

### 🐛 Bug Fixes

- Minor bug fixes to improve stability and performance.

### 📝 Documentation

- Added a `NEWS.md` file to track package changes.

## dataviewR 0.1.0

CRAN release: 2025-06-14

- Initial release
