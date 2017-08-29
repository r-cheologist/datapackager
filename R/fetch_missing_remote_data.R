#' @title fetch_missing_remote_data
#' @aliases fetch_missing_remote_data
#' @description Convenience funtion to retrieve data files missing in the file
#' system.
#' @details Retrieves files listed in a \code{\link{data_catalogue}}, but not
#' distributed with the package.
#'
#' An intended use case is e.g. the publication of a package that documents the
#' analysis of a data set, where the raw data itself is subject to a separate
#' data sharing agreement.
#'
#' Uses \code{\link{retrieve_remote_file}} after identifying data files/objects
#' not present and proceeds to add the (compressed) files and objects
#' analogously to \code{\link{include_data_file}}, using the parameters listed
#' in \code{\link{data_catalogue}}.
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure resides.
#' @param user Single \code{\link{character}} object used for authentication
#' where retrieval requires it.
#' @param password Single \code{\link{character}} object used for authentication
#' where retrieval requires it.
#' @param ... Further parameters handed to \code{\link{retrieve_remote_file}}.
#' @return If successful, returns a single \code{\link{logical} TRUE} object via
#' \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{retrieve_remote_file}}, \code{\link{data_catalogue}}
#' @examples
#' # Load tools
#' library(magrittr)
#'
#' # Generate package infrastructure
#' ## Define a package root
#' pkg_root <- tempdir() %>%
#'   file.path("packagetest")
#'
#' ## Create the infrastructure and add a remote file on the fly
#' ## (from Billing et al. (2016). Comprehensive transcriptomic
#' ## and proteomic characterization of human mesenchymal stem cells reveals
#' ## source specific cellular markers. Sci Rep 6, 21507;
#' ## Licensed under the Creative Commons Attribution 4.0 International License.
#' ## http://creativecommons.org/licenses/by/4.0/)
#' \donttest{
#'   require(readxl)
#'   init(
#'     root = pkg_root,
#'     files_to_include = paste0(
#'       c("http://www.nature.com/article-assets/npg/srep",
#'         "2016/160209/srep21507/extref/srep21507-s4.xls"),
#'       collapse = "/"),
#'     file_is_url = TRUE,
#'     file_reading_function = "read_excel",
#'     file_reading_options = list(skip = 1),
#'     file_reading_package_dependencies = "readxl",
#'     file_distributable = FALSE)
#'
#'   # Investigate the data catalogue
#'   tmp_env <- new.env()
#'   pkg_root %>%
#'     file.path("data","data_catalogue.rda") %>%
#'     load(envir = tmp_env)
#'   tmp_env$data_catalogue %>%
#'   str()
#'
#'   # Investigate derived objects in the file system
#'   (tmp_files <- pkg_root %>%
#'      list.files(pattern = "srep21507", recursive = TRUE))
#'
#'   # Delete them to simulate undistributed data
#'   pkg_root %>%
#'     file.path(tmp_files) %>%
#'     unlink()
#'
#'   ## See: all gone ...
#'   pkg_root %>%
#'     list.files(pattern = "srep21507", recursive = TRUE)
#'
#'   # Fetch the missing data back using the parameters in the 'data_catalogue'
#'   pkg_root %>%
#'     fetch_missing_remote_data()
#'     ## See: all back!
#'     pkg_root %>%
#'       list.files(pattern = "srep21507", recursive = TRUE)
#' }
#'
#' # Clean up the package root - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
fetch_missing_remote_data <- function(
  root = getwd(),
  user = NULL,
  password = NULL,
  ...)
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
        tmp_path <- retrieve_remote_file(
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
              password[mei]),
          ...)
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

  # Return TRUE if successful
  TRUE %>%
    invisible()
}
