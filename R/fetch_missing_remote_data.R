#' @export
fetch_missing_remote_data <- function(
  root = getwd(),
  user = NULL,
  password = NULL)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root()

  if(user %>%
     is.null() %>%
     magrittr::not())
  {
    user %>%
      assertive.types::assert_is_character()
  }

  if(password %>%
     is.null() %>%
     magrittr::not())
  {
    password %>%
      assertive.types::assert_is_character()
  }

  assertive.properties::assert_are_same_length(
    user,
    password)

# Processing --------------------------------------------------------------
  # Load the data catalogue
  data_catalogue <- root %>%
    load_data_file_as_object()

  # Select only what's marked as being remote
  subsetter <- data_catalogue %>%
    sapply(
      function(x){
        x %>%
          magrittr::extract2("File.Is.Remote") %>%
          return()
      })
  data_catalogue %<>%
    magrittr::extract(subsetter)

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
      }) %>%
    unname()
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
      }) %>%
    unname()
  assertive.base::assert_are_identical(
    raw_data_present,
    parsed_data_present)
  data_catalogue %<>%
    magrittr::extract(
      raw_data_present %>%
        magrittr::not())

  if(user %>%
      is.null() %>%
      magrittr::not())
  {
    assertive.sets::is_subset(
      user %>%
        length(),
      c(1,data_catalogue %>% length())
    )
  }

  for (mei in (data_catalogue %>% seq_along()))
  {
    me <- data_catalogue %>%
      magrittr::extract2(mei)
    # Retrieve file & check integrity
    tmp_path <- with(
      me,
      {
        message(File)
        tmp_path <- retrieve_remote_data(
          url = Remote.Source,
          user = user %>%
            length() %>%
            switch(
              "0" = NULL,
              "1" = user,
              user[mei]),
          password = password %>%
            length() %>%
            switch(
              "0" = NULL,
              "1" = password,
              password[mei]))
        assertive.base::assert_are_identical(
          Hash.Uncompressed,
          tmp_path %>%
            digest::digest(
              algo = Hashing.Algo,
              file = TRUE))
        tmp_path %>%
          return()
      })
    # Parse file, check object integrity & save results
    with(
      me,
      {
        tmp_object <- parse_data(
          path = tmp_path,
          reading_function = File.Reading.Function,
          reading_options = File.Reading.Options)
        assertive.base::assert_are_identical(
          Hash.Object,
          tmp_object %>%
            digest::digest(
              algo = Hashing.Algo))
        data_rename_and_writeout(
          data_object = tmp_object,
          file_name = File,
          root = root,
          compression_algo = Compression.Algo)
      })
    # Compress & save
    ## Integrity checking not possible on compressed files, as zip includes
    ## timestamps no matter what setting --> never matching to prior compression
    with(
      me,
      {
        save_zipfile(
          uncomp_path = tmp_path,
          root = root) %>%
          digest::digest(
            algo = Hashing.Algo,
            file = TRUE)
      })
  }
}
