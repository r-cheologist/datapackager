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
  objects_to_include = file.path(tmp_dir, "data_file_1.csv"),
  parsing_function = "read.delim")
# unlink(package_root,recursive = TRUE)

# Post-initiation addition (with reading option)
include_data(
  root = package_root,
  object_to_include = file.path(tmp_dir, "data_file_2.csv"),
  parsing_function = "read.delim",
  parsing_options = list(sep = ","))

# Deletion of the first file
remove_data(
  root = package_root,
  object_to_remove = "data_file_1.csv")

# Add a remote file (from Billing et al. (2016). Comprehensive transcriptomic
# and proteomic characterization of human mesenchymal stem cells reveals source
# specific cellular markers. Sci Rep 6, 21507.
# EXCLUDED FROM BUILDS
library(readxl)
tmp_url <- "http://www.nature.com/article-assets/npg/srep/2016/160209/srep21507/extref/srep21507-s4.xls"
include_data(
  root = package_root,
  object_to_include = tmp_url,
  package_dependencies = "readxl",
  parsing_function = "read_excel",
  parsing_options = list(skip = 1),
  distributable = FALSE)

# Simulate fresh package checkout/install NOT including undistributed data
datapackageR:::make_data_path(package_root, tmp_url) %>%
  unlink()
datapackageR:::make_extdata_path(package_root, tmp_url) %>%
  unlink()

# Fetch the "seperately distributed" data
retrieve_missing_remote_data(package_root)

# Run tests
devtools::install(pkg = package_root)
devtools::test(pkg = package_root)

# Clean upo
remove.packages(package_root %>% basename())
unlink(package_root, recursive = TRUE)

