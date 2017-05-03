#' @details This function is provided by \pkg{datapackageR} (version <DATAPACKAGERVERSION>).
#' @export
test_data_integrity <- function(
  package_name = NULL
){

# Check prerequisites -----------------------------------------------------
  package_name %>%
    assertive.types::assert_is_a_string() %>%
    assertive.sets::assert_is_subset(installed.packages())

# Processing --------------------------------------------------------------
  testthat::context("--> Checking integrity of data referenced in 'data_catalogue' <--")

  for(entry in names(data_catalogue)){

    source_is_remote <- data_catalogue %>%
      magrittr::extract2(entry) %>%
      magrittr::extract2("Remote.File")

    # Test integrity of compressed 'extdata'
    ## Make path
    file_base_name <- data_catalogue %>%
      magrittr::extract2(entry) %>%
      magrittr::extract2("File")
    file_path_compressed <- system.file(
      file.path(
        "extdata",
        file_base_name) %>%
        paste0(".zip"),
      package = package_name,
      mustWork = source_is_remote %>%
        magrittr::not())

    ## Test
    if(file_path_compressed %>%
       magrittr::equals(""))
    {
      warning(
        "Catalogue entry '",
        entry,
        "' separately distributed and not present. Conisder fetching with 'fetch_missing_remote_data'."
      )
    } else {
      # zip includes timestamps no matter what settings - files zipped at
      # different times will never have identical hashes
      if(source_is_remote %>%
         magrittr::not())
      {
        testthat::test_that(
          paste0(
            "'",
            file_base_name,
            "' in its compressed form matches the checksum."),
          {
            testthat::expect_identical(
              digest::digest(
                file = file_path_compressed,
                algo = data_catalogue %>%
                  magrittr::extract2(entry) %>%
                  magrittr::extract2("Hashing.Algo")),
              data_catalogue %>%
                magrittr::extract2(entry) %>%
                magrittr::extract2("Hash.Compressed"))
          })
      }
    }

    # Test integrity of extracted 'extdata'
    ## Extract the data temporarily
    if(file_path_compressed %>%
       magrittr::equals("") %>%
       magrittr::not())
    {
      unzip_target_dir <- tempdir()
      utils::unzip(
        zipfile = file_path_compressed,
        exdir = unzip_target_dir,
        junkpaths = TRUE)
      ## Make path
      file_path <- unzip_target_dir %>%
        file.path(file_base_name)
      ## Test
      testthat::test_that(
        paste0(
          "'",
          file_base_name,
          "' in its decompressed form matches the checksum."),
        {
          testthat::expect_identical(
            digest::digest(
              file = file_path,
              algo = data_catalogue %>%
                magrittr::extract2(entry) %>%
                magrittr::extract2("Hashing.Algo")),
            data_catalogue %>%
              magrittr::extract2(entry) %>%
              magrittr::extract2("Hash.Uncompressed"))
        })
    }

    # Test integrity of stored object
    testthat::test_that(
      paste0(
        " the R opbject based on '",
        file_base_name,
        "' matches the checksum."),
      {
        testthat::expect_identical(
          digest::digest(
            file_base_name %>%
              get0(),
            algo = data_catalogue %>%
              magrittr::extract2(entry) %>%
              magrittr::extract2("Hashing.Algo")),
          data_catalogue %>%
            magrittr::extract2(entry) %>%
            magrittr::extract2("Hash.Object"))
      })

    # Test equivalency of the stored object/data and the raw data
    ## Load dependencies necessary to recreate data structures
    data_catalogue %>%
      magrittr::extract2(entry) %>%
      magrittr::extract2("File.Reading.Package.Dependencies") %>%
      sapply(
        require,
        character.only = TRUE)

    ## Re-parse the raw data content
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

    ## Test
    testthat::test_that(
      paste0(
        "The R object derived from '",
        file_base_name,
        "' represents the original file."), {
          testthat::expect_equal(
            file_base_name %>%
              get0(),
            file_content_fresh)})
  }
}
