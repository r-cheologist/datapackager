context("--> Checking integrity of data referenced in 'data_catalogue' <--")

require(<PACKAGENAME>)
require(testthat)

package_name <- "<PACKAGENAME>"

for(entry in nrow(data_catalogue)){

  file_base_name <- data_catalogue %>%
    magrittr::extract(i, "File")
  file_path_compressed <- system.file(
    file.path(
        "extdata",
        file_base_name) %>%
      paste0(".zip"),
    package = package_name,
    mustWork = TRUE)

  test_that(
    paste0(
      "'",
      file_base_name,
      "' in its compressed form matches the checksum."),
    {
      require(digest)
      expect_identical(
        digest(
          file = file_path_compressed,
          algo = data_catalogue %>%
            magrittr::extract(i, "Hashing.Algo")),
        data_catalogue %>%
          magrittr::extract(i, "Hash.Compressed"))
  })

  unzip_target_dir <- tempdir()
  utils::unzip(
    zipfile = file_path_compressed,
    exdir = unzip_target_dir)
  file_path <- unzip_target_dir %>%
    file.path(file_base_name)

  test_that(
    paste0(
      "'",
      file_base_name,
      "' in its decompressed form matches the checksum."),
    {
      require(digest)
      expect_identical(
        digest(
          file = file_path,
          algo = data_catalogue %>%
            magrittr::extract(i, "Hashing.Algo")),
        data_catalogue %>%
          magrittr::extract(i, "Hash.Unompressed"))
    })

  file_content_fresh <- data_catalogue %>%
    magrittr::extract(i, "File.Reading.Function") %>%
    do.call(
      c(
        file_path,
        data_catalogue %>%
          magrittr::extract(i, "File.Reading.Options")) %>%
          as.list())

  test_that(
    paste0(
      "The R object derived from '",
      file_base_name,
      "' represents the original file."), {
        expect_equal(
          file_base_name %>%
            get0(),
          file_content_fresh)})
}
