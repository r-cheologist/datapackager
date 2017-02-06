fetch_missing_remote_data <- function(
  root = getwd(),
  file_user = NULL,
  file_password = NULL)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root()

  if(file_user %>%
     is.null() %>%
     magrittr::not())
  {
    file_user %>%
      assertive.types::assert_is_character()
  }

  if(file_password %>%
     is.null() %>%
     magrittr::not())
  {
    file_password %>%
      assertive.types::assert_is_character()
  }

  assertive.properties::assert_all_are_same_length(
    file_user,
    file_password)

# Processing --------------------------------------------------------------
  # Load the data catalogue
  data_catalogue <- root %>%
    load_data_catalogue_from_file()

  # Select only what's marked as being remote
  subsetter <- data_catalogue %>%
    sapply(
      function(x){
        x %>%
          magrittr::extract2("Remote.File") %>%
          return()
      })
  data_catalogue %<>% magrittr::extract2(subsetter)

  # Select only what isn't present
  raw_data_present <- data_catalogue %>%
    sapply(
      function(x){
        file.path(
          root,
          "inst",
          "extdata",
          x %>%
            magrittr::extract2("File") %>%
            paste0(".zip")) %>%
          assertive.files::is_readable_file() %>%
          return()
      })
  parsed_data_present <- data_catalogue %>%
    sapply(
      function(x){
        file.path(
          root,
          "data",
          x %>%
            magrittr::extract2("File") %>%
            paste0(".rda")) %>%
          assertive.files::is_readable_file() %>%
          return()
      })
  assertive.base::assert_are_identical(raw_data_present, parsed_data_present)
  data_catalogue %<>%
    magrittr::extract2(raw_data_present)

  # Retrieve, compress & save files
  stop()

  # Parse files & save results
}
