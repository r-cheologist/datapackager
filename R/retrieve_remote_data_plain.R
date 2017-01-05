#' @export
retrieve_remote_data_plain <- function(
  url,
  user = NULL,
  password = NULL,
  ...
)
{

# Check prerequisites -----------------------------------------------------
  url %>%
    assertive.types::assert_is_a_string()

    if(user %>%
     is.null() %>%
     magrittr::not())
  {
    user %>%
      assertive.types::assert_is_a_string()
  }
  if(password %>%
     is.null() %>%
     magrittr::not())
  {
    password %>%
      assertive.types::assert_is_a_string()
  }
  c(user, password) %>%
    length() %>%
    assertive.sets::assert_is_subset(c(0,2))

# Processing --------------------------------------------------------------
  # Make the request
  if(user %>%
     is.null())
  {
    httr_get <- url %>%
      httr::GET()
  } else {
    httr_get <- url %>%
      httr::GET(
        httr::authenticate(
          user = user,
          password = password,
          ...))
  }

  # Check for errors
  if( httr_get %>%
      httr::http_error())
  {
    httr_status <- httr_get %>%
      httr::http_status()
    stop(
      "Can't access URL: ",
      httr_status %>%
        magrittr::extract2("message"))
  }

  # Extract the content
  httr_content <- httr_get %>%
    httr::content(as = "raw")

  # Write the content to file
  tmp_dir <- tempdir()
  url <- file.path(
    tmp_dir,
    url %>%
      basename())
  writeBin(
    object = httr_content,
    con = url)

  # Return the new path
  url %>%
    return()
}
