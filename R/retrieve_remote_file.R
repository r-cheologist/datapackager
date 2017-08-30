#' @title retrieve_remote_file
#' @aliases retrieve_remote_file
#' @description Convenience funtion to retrieve a data file from an URL.
#' @details Wraps functionality provided by \pkg{httr}.
#' @param url Single \code{\link{character}} representing the URL to retrieve.
#' @param user Single \code{\link{character}} object used for authentication
#' where retrieval requires it.
#' @param password Single \code{\link{character}} object used for authentication
#' where retrieval requires it.
#' @param ... Further parameters handed to \code{\link[httr]{authenticate}}.
#' @return Returns a \code{\link{character}} object representing the path of the
#' temporary file resulting from the retrieval.
#' @author Johannes Graumann
#' @seealso \code{\link{retrieve_missing_remote_data}}
#' @examples
#' # Load tools
#' require(magrittr)
#'
#' # Retrieve a remote file (from Billing et al. (2016). Comprehensive transcriptomic
#' # and proteomic characterization of human mesenchymal stem cells reveals
#' # source specific cellular markers. Sci Rep 6, 21507.
#' # Licensed under the Creative Commons Attribution 4.0 International License.
#' # http://creativecommons.org/licenses/by/4.0/
#' ## Complicated URL generation to circumvent line length restrictions
#' tmp_url <- paste0(
#'   c("http://www.nature.com/article-assets/npg/srep",
#'   "2016/160209/srep21507/extref/srep21507-s4.xls"),
#'   collapse = "/")
#' (tmp_path <- tmp_url %>%
#'   retrieve_remote_file())
#'
#' # Explore the file
#' \donttest{
#'   if(requireNamespace("readxl", quietly = TRUE))
#'   {
#'     require(readxl)
#'     tmp_path %>%
#'       read_excel(skip = 1) %>%
#'       str()
#'   }
#' }
#' @export
retrieve_remote_file <- function(
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
