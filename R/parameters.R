required_directories <- c(
  "data",
  "inst/extdata",
  "man",
  "R",
  "tests/testthat")

required_files <- c(
  ".gitignore",
  ".Rbuildignore",
  "DESCRIPTION",
  "NAMESPACE",
  "tests/testthat.R")
# If you're using testthat in a package, you should put your tests in tests/testthat.
# Each test file should start with test and end in .R or .r.
# To ensure R CMD check runs your tests, place the following code in tests/testthat.R:
#
# library(testthat)
# library(yourpackage)
#
# test_check("yourpackage")
#
# Also make sure to add Suggests: testthat to your DESCRIPTION.
