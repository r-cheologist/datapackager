#' @title test_data_integrity
#' @aliases test_data_integrity
#' @description Uses information in the \code{\link{data_catalogue}} to check the
#' integrity of data sets shipped.
#' @details This function allows for integrity checking of data sets included
#' via \code{\link[digest]{digest}}, using the cryptographic hash information
#' stored in \code{\link{data_catalogue}}.
#' @param package_name Single \code{\link{character}} object, providing the name
#' of the package for which data integrity is to be tested.
#' @author Johannes Graumann
#' @export
test_data_integrity <- function(
  package_name = NULL
){

# Check prerequisites -----------------------------------------------------
  package_name %>%
    assertive.types::assert_is_a_string() %>%
    assertive.sets::assert_is_subset(
      utils::installed.packages())

# Processing --------------------------------------------------------------
  require(package_name, character.only = TRUE)
  testthat::context("--> Checking integrity of data referenced in 'data_catalogue' <--")

  for (entry in data_catalogue)
  {
    with(entry,
         {
## Test integrity of stored object ----
           testthat::test_that(
             paste0("The R opbject based on '",Base.Name,"' matches the checksum."),
             {
               testthat::expect_identical(
                 digest::digest(
                   object = get0(Base.Name),
                   algo   = Hashing.Algo,
                   file   = FALSE),
                 Hash.Object)
             })
## Deal with entries where 'extdata' expected ----
           if (Data.Type %in% c("file", "url"))
           {
             ext_data_path <- system.file(
               file.path(
                 "extdata",
                 Base.Name) %>%
                 paste0(".zip"),
               package = package_name,
               mustWork = FALSE)
             if (identical(ext_data_path, ""))
             {
               warning(
                 "Catalogue entry '",
                 Base.Name,
                 "' possibly separately distributed and not present. Conisder fetching with 'retreive_missing_remote_data'.")
             } else {
### Test integrity of compressed 'extdata' ----
               # zip includes timestamps no matter what settings - files zipped at
               # different times will never have identical hashes
               # testthat::test_that(
               #   paste0("'", Base.Name, "' in its compressed form matches the checksum."),
               #   {
               #     testthat::expect_identical(
               #       digest::digest(
               #         object = ext_data_path,
               #         algo   = Hashing.Algo,
               #         file   = TRUE),
               #       Hash.Compressed)
               #   })

### Test integrity of extracted 'extdata' ----
               unzip_target_dir <- tempdir()
               unzipped_file_path <- utils::unzip(
                 zipfile   = ext_data_path,
                 exdir     = unzip_target_dir,
                 junkpaths = TRUE)
               testthat::test_that(
                 paste0("'", Base.Name, "' in its decompressed form matches the checksum."),
                 {
                   testthat::expect_identical(
                     digest::digest(
                       object = unzipped_file_path,
                       algo   = Hashing.Algo,
                       file   = TRUE),
                     Hash.Uncompressed)
                 })

### Test equivalency of the stored object/data and the raw data ----
               # Load dependencies necessary to recreate data structures
               Package.Dependencies %>%
                 sapply(
                   require,
                   character.only = TRUE)

               # Re-parse the raw data content
               object_fresh <- parse_data(
                 path             = unzipped_file_path,
                 reading_function = Parsing.Function,
                 reading_options  = Parsing.Options)

               # Test
               testthat::test_that(
                 paste0(" the R object derived from '", Base.Name,"' represents the original file."),
                 {
                   testthat::expect_equal(
                     get0(Base.Name),
                     object_fresh)
                 })
             }
           }
         })
  }
}
