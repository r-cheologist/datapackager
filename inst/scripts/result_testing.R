unlink("~/packagetest2", recursive = TRUE)
library(datapackageR)
library(magrittr)

pkg_root2 <-
  "~" %>%
  file.path("packagetest2")

data.frame(
  x   = 1,
  y   = 1:10,
  fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
  write.table(
    file      = file.path(dirname(pkg_root2), "data_dummy.tsv"),
    sep       = "\t",
    col.names = TRUE,
    row.names = FALSE)
init(
  root = pkg_root2,
  files_to_include = file.path(dirname(pkg_root2), "data_dummy.tsv"),
  file_reading_function = "read.csv",
  file_reading_options = list(sep = "\t", stringsAsFactors = FALSE))

devtools::check(pkg_root2, run_dont_test = TRUE)
