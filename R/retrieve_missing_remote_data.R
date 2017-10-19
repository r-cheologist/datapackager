#' @title retrieve_missing_remote_data
#' @aliases retrieve_missing_remote_data
#' @description Convenience funtion to retrieve data files missing in the file
#' system.
#' @details Retrieves remote files listed in a \code{\link{data_catalogue}}, but
#' not distributed with the package.
#'
#' An intended use case is e.g. the publication of a package that documents the
#' analysis of a data set, where the raw data itself is subject to a separate
#' data sharing agreement.
#'
#' Uses \code{\link{retrieve_remote_file}} after identifying data files/objects
#' not present and proceeds to add the (compressed) files and objects
#' analogously to \code{\link{include_data}}, using the parameters listed in
#' \code{\link{data_catalogue}}.
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
#' ## Create the infrastructure and add a remote file (needing parsing) on the
#' ## fly (from Billing et al. (2016). Comprehensive transcriptomic and
#' ## proteomic characterization of human mesenchymal stem cells reveals source
#' ## specific cellular markers. Sci Rep 6, 21507; Licensed under the Creative
#' ## Commons Attribution 4.0 International License.
#' ## http://creativecommons.org/licenses/by/4.0/)
#' \donttest{
#'   if(requireNamespace("readxl", quietly = TRUE))
#'   {
#'     require(readxl)
#'     init(
#'       root = pkg_root,
#'       files_to_include = paste0(
#'         c("http://www.nature.com/article-assets/npg/srep",
#'           "2016/160209/srep21507/extref/srep21507-s4.xls"),
#'         collapse = "/"),
#'       file_is_url = TRUE,
#'       file_reading_function = "read_excel",
#'       file_reading_options = list(skip = 1),
#'       file_reading_package_dependencies = "readxl",
#'       file_distributable = FALSE)
#'
#'     # Investigate the data catalogue
#'     tmp_env <- new.env()
#'     pkg_root %>%
#'       file.path("data","data_catalogue.rda") %>%
#'       load(envir = tmp_env)
#'     tmp_env$data_catalogue %>%
#'     str()
#'
#'     # Investigate derived objects in the file system
#'     (tmp_files <- pkg_root %>%
#'        list.files(pattern = "srep21507", recursive = TRUE))
#'
#'     # Delete them to simulate undistributed data
#'     pkg_root %>%
#'       file.path(tmp_files) %>%
#'       unlink()
#'
#'     ## See: all gone ...
#'     pkg_root %>%
#'       list.files(pattern = "srep21507", recursive = TRUE)
#'
#'     # retrieve the missing data back using the parameters in the 'data_catalogue'
#'     pkg_root %>%
#'       retrieve_missing_remote_data()
#'       ## See: all back!
#'       pkg_root %>%
#'         list.files(pattern = "srep21507", recursive = TRUE)
#'   }
#' }
#'
#' # Clean up the package root - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
retrieve_missing_remote_data <- function(
  root = getwd(),
  user = NULL,
  password = NULL,
  ...)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root()

  if (!is.null(user))
  {
    user %>%
      assertive.types::assert_is_a_string()
  }

  if (!is.null(password))
  {
    password %>%
      assertive.types::assert_is_a_string()
  }

# Processing --------------------------------------------------------------
## Load the data catalogue ----
  data_catalogue <- root %>%
    file.path("data", "data_catalogue.rda") %>%
    load_data_file_as_object()

## Where's remote extdata expected but not present? ----
  needs_retrieval_of_extdata <- data_catalogue %>%
    sapply(
      function(x){
        with(x,
             {
               if (
                 Data.Type %>%
                 stringi::stri_detect_fixed(
                   "url",
                   negate = TRUE,
                   opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)))
               {
                 return(FALSE)
               }

               make_extdata_path(root, Base.Name) %>%
                 assertive.files::is_readable_file() %>%
                 magrittr::not() %>%
                 return()
             })
      })

## Where's parsed data expected but not present? ----
  needs_parsing_of_extdata <- data_catalogue %>%
    sapply(
      function(x)
      {
        with(x,
             {
               if (
                 Data.Type %in% c("file", "url") %>%
                 magrittr::not())
               {
                 return(FALSE)
               }

               make_data_path(root, Base.Name) %>%
                 assertive.files::is_readable_file() %>%
                 magrittr::not() %>%
                 return()
             })
      })

## Where's data corresponding to remote serialized objects missing? ----
  needs_saving_of_remote_data <- data_catalogue %>%
    sapply(
      function(x)
      {
        with(x,
             {
               if (
                 Data.Type %>%
                   stringi::stri_detect_fixed(
                     pattern = "url_",
                     negate = TRUE,
                     opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)))
               {
                 return(FALSE)
               }

               make_data_path(root, Base.Name) %>%
                 assertive.files::is_readable_file() %>%
                 magrittr::not() %>%
                 return()
             })
      }
    )

## Fetch missing remote extdata & check for data integrity ----
  for (entry in data_catalogue %>%
       magrittr::extract2(needs_retrieval_of_extdata))
  {
    with(entry,
         {
           tmp_path <- retrieve_remote_file(
             url      = Full.Name,
             user     = user,
             password = password,
             ...)

           assertive.base::assert_are_identical(
             digest::digest(
               object = tmp_path,
               algo   = Hashing.Algo,
               file   = TRUE),
             Hash.Uncompressed
           )

           save_zipfile(tmp_path, file.path(root,"inst", "extdata"))
         })
  }

## Parse extdata with missing object representation & check for data integrity ----
  for (entry in data_catalogue %>%
       magrittr::extract2(needs_parsing_of_extdata))
  {
    with(entry,
         {
           tmp_object <- parse_data(
             path             = make_extdata_path(root, Base.Name),
             reading_function = Reading.Function,
             reading_options  = Reading.Options)

           assertive.base::assert_are_identical(
             digest::digest(
               object = tmp_object,
               algo   = Hashing.Algo,
               file   = FALSE),
             Hash.Object)

           data_rename_and_writeout(
             data_object      = tmp_object,
             file_name        = Base.Name,
             root             = root,
             compression_algo = Compression.Algo)
         })
  }

## Retrieve missing remote data & test its integrity ----
  for (entry in data_catalogue %>%
       magrittr::extract2(needs_saving_of_remote_data))
  {
    with(entry,
         {
           tmp_path <- retrieve_remote_file(
             url      = Full.Name,
             user     = user,
             password = password,
             ...)

           if (stringi::stri_endswith_fixed(str = Data.Type, pattern =  "rds"))
           {
             tmp_object <- readRDS(tmp_path)
           }

           if (stringi::stri_endswith_fixed(str = Data.Type, pattern = "rda"))
           {
             tmp_env <- new.env()
             tmp_path %>%
               load(envir = tmp_env)
             if (length(tmp_env) != 1)
             {
               warning(
                 "Multiple objects contained in '",
                 Full.Name,
                 "'. Only the first one is extracted.")
             }
             tmp_object <- get(
               tmp_env %>%
                 names() %>%
                 magrittr::extract2(1),
               envir = tmp_env)
           }

           assertive.base::assert_are_identical(
             digest::digest(
               object = tmp_object,
               algo   = Hashing.Algo,
               file   = FALSE),
             Hash.Object)

           data_rename_and_writeout(
             data_object      = tmp_object,
             file_name        = Base.Name,
             root             = root,
             compression_algo = Compression.Algo)
         })
  }

## Return TRUE if successful ----
  TRUE %>%
    invisible()
}
