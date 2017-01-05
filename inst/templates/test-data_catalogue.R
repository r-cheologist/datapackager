require(<PACKAGENAME>)
require(testthat)

context("--> Checking integrity of data referenced in 'data_catalogue' <--")

package_name <- "<PACKAGENAME>"

for(entry in names(data_catalogue)){

  data_catalogue %>%
    magrittr::extract2(entry) %>%
    magrittr::extract2("File.Reading.Package.Dependencies") %>%
    sapply(
      require,
      character.only = TRUE)

  file_base_name <- data_catalogue %>%
    magrittr::extract2(entry) %>%
    magrittr::extract2("File")
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
            magrittr::extract2(entry) %>%
            magrittr::extract2("Hashing.Algo")),
        data_catalogue %>%
          magrittr::extract2(entry) %>%
          magrittr::extract2("Hash.Compressed"))
  })

  unzip_target_dir <- tempdir()
  utils::unzip(
    zipfile = file_path_compressed,
    exdir = unzip_target_dir,
    junkpaths = TRUE)
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
            magrittr::extract2(entry) %>%
            magrittr::extract2("Hashing.Algo")),
        data_catalogue %>%
          magrittr::extract2(entry) %>%
          magrittr::extract2("Hash.Uncompressed"))
    })

  file_content_fresh <- data_catalogue %>%
    magrittr::extract2(entry) %>%
    magrittr::extract2("File.Reading.Function") %>%
    do.call(
      c(
        file_path %>%
          as.list(),
        data_catalogue %>%
          magrittr::extract2(entry) %>%
          magrittr::extract2("File.Reading.Option")))

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
