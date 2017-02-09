# Load the package
library(datapackageR)
library(magrittr)

tmp_dir <- tempdir()
package_root <- tmp_dir %>%
  file.path("testpackage")

# Create two "data files"
tmp_data <- data.frame(
  First.Column  = LETTERS,
  Second.Column = LETTERS %>%
    seq_along()
)
tmp_data %>%
  write.table(
    file = tmp_dir %>%
      file.path("data_file_1.csv"),
    sep = "\t")
tmp_data %>%
  write.csv(
    file = tmp_dir %>%
      file.path("data_file_2.csv"))

# Initiate data package infrastructure and add one of the files
init(
  root = package_root,
  files_to_include = file.path(tmp_dir, "data_file_1.csv"),
  file_reading_function = "read.delim")
# unlink(package_root,recursive = TRUE)

# Post-initiation addition (with reading option)
include_data_file(
  root = package_root,
  file_to_include = file.path(tmp_dir, "data_file_2.csv"),
  file_reading_function = "read.delim",
  file_reading_options = list(sep = ","))

# Deletion of the first file
remove_data_file(
  root = package_root,
  file_to_remove = "data_file_1.csv")

# Add a remote file (from Billing et al. (2016). Comprehensive transcriptomic
# and proteomic characterization of human mesenchymal stem cells reveals source
# specific cellular markers. Sci Rep 6, 21507.
# EXCLUDED FROM BUILDS
library(readxl)
include_data_file(
  root = package_root,
  file_to_include = "http://www.nature.com/article-assets/npg/srep/2016/160209/srep21507/extref/srep21507-s4.xls",
  file_is_url = TRUE,
  file_reading_function = "read_excel",
  file_reading_options = list(skip = 1),
  file_reading_package_dependencies = "readxl",
  file_distributable = FALSE)

