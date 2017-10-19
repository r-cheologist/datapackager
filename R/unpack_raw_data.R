#' @title unpack_raw_data
#' @aliases unpack_raw_data
#' @description Convenience function to unpack raw data packaged by
#' \pkg{datapackageR}.
#' @details Using \code{\link[utils]{unzip}}, the function unpacks all data sets
#' listed in \code{\link{data_catalogue}} (or a user-defined selection thereof)
#' into \code{target_directory}.
#' @param package_name Single \code{\link{character}} naming the
#' (\pkg{datapackageR}-managed) \code{R} package the data set(s) to be extracted
#' reside in.
#' @param file_names \code{\link{character}} object representing the names
#' (\code{\link{basename}}s) of the data sets to be extracted. Defaults to all
#' data sets listed in \code{\link{data_catalogue}} if set to \code{\link{NULL}}.
#' @param target_dir Single \code{\link{character}} defining the target
#' directory into which the data sets are extracted.
#' @return Returns a named \code{\link{character}} object containing the paths
#' pointing at the extracted data files.
#' @author Johannes Graumann
#' @seealso \code{\link[utils]{unzip}}, \code{\link{data_catalogue}}
#' @examples
#' # Load tools
#' library(magrittr)
#'
#' # Create a package with immediate data inclusion
#' ## Define a package root
#' pkg_root <- tempdir() %>%
#'   file.path("packagetest")
#' ## Create a dummy data file
#' data.frame(
#'   x   = 1,
#'   y   = 1:10,
#'   fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
#' write.table(
#'   file      = file.path(dirname(pkg_root), "data_dummy.tsv"),
#'   sep       = "\t",
#'   col.names = TRUE,
#'   row.names = FALSE)
#' ## Create the package infrastructure, including the dummy file
#' init(
#'   root = pkg_root,
#'   objects_to_include = file.path(dirname(pkg_root), "data_dummy.tsv"),
#'   parsing_function = "read.csv",
#'   parsing_options = list(sep = "\t", stringsAsFactors = FALSE))
#'
#' # Install the resulting package
#' devtools::install(pkg_root)
#'
#' # Extract and explore
#' unpack_raw_data(package_name = "packagetest") %>%
#'   file.info()
#'
#' # Uninstall the package
#' remove.packages("packagetest")
#'
#' # Clean up the package roots - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
unpack_raw_data <- function(
  package_name = NULL,
  file_names = NULL,
  target_dir = tempdir())
{
# Check prerequisites -----------------------------------------------------
  package_name %>%
    assertive.types::assert_is_a_string() %>%
    assertive.sets::assert_is_subset(utils::installed.packages())

  if (file_names %>%
      is.null())
  {
    file_names <- data_catalogue %>%
      names()
  } else {
    file_names %>%
      assertive.types::assert_is_character() %>%
      assertive.sets::assert_is_subset(
        data_catalogue %>%
          names())
  }

  target_dir %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_dirs()

# Processing --------------------------------------------------------------
  file_names %>%
    vapply(
      FUN = function(x){
        # Unzip the raw data into the target directory
        ## Construct the expected path of the *.zip file
        file.path(
            "extdata",
            paste0(x,".zip")) %>%
        ## Find it in the file system/package
          system.file(
            package = package_name,
            mustWork = TRUE) %>%
        ## Unzip it into the target directory
          utils::unzip(
            exdir     = target_dir,
            junkpaths = TRUE)

        # Return the new path
        target_dir %>%
          file.path(x) %>%
          return()
      },
      FUN.VALUE = "A",
      USE.NAMES = TRUE) %>%
    return()
}

utils::globalVariables("data_catalogue")
