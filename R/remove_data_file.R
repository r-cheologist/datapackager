#' @title remove_data_file
#' @aliases remove_data_file
#' @description Removing a data file from \pkg{datapackageR}s infrastructure.
#' @details The function essentially reverses the steps taken by
#' \code{\link{include_data_file}} (documented there).
#' @param file_to_remove Single \code{\link{character}} representing the name or
#' path of a data file to be removed.
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure to be modified resides.
#' @param data_catalogue Single \code{link{character}} object representing the
#' relative path to the \code{\link{data_catalogue}}-containing \code{R} data
#' file within the packaging infrastructure (defaulting to
#' \code{data/data_catalogue.rda} if set to \code{\link{NULL}}).
#' @param save_catalogue Single \code{\link{logical}} indicating whether the
#' amended \code{\link{data_catalogue}} object is to be saved back to the
#' package infrastructure or just (silently) returned.
#' @return Returns a \code{\link{list}} of \code{\link{data_catalogue}}
#' characteristics via \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{include_data_file}}, \code{\link{data_catalogue}}
#' @examples
#' # Load tools
#' library(magrittr)
#'
#' # Generate package infrastructure
#' ## Define a package root
#' pkg_root <- tempdir() %>%
#'   file.path("packagetest")
#'
#' ## Create the infrastructure
#' init(
#'   root = pkg_root)
#'
#' ## Investigate the data catalogue
#' tmp_env <- new.env()
#' pkg_root %>%
#'   file.path("data/data_catalogue.rda") %>%
#'   load(envir = tmp_env)
#' tmp_env$data_catalogue %>%
#'   str()
#'
#' # Add a local data file
#' ## Create a dummy data file
#' data.frame(
#'   x   = 1,
#'   y   = 1:10,
#'   fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
#'   write.table(
#'     file      = file.path(dirname(pkg_root), "data_dummy.tsv"),
#'     sep       = "\t",
#'     col.names = TRUE,
#'     row.names = FALSE)
#'
#' ## Add the dummy file to the existing package infrastructure
#' data_catalogue <- include_data_file(
#'   file_to_include = file.path(dirname(pkg_root), "data_dummy.tsv"),
#'   root = pkg_root,
#'   file_reading_function = "read.csv",
#'   file_reading_options = list(sep = "\t", stringsAsFactors = FALSE))
#'
#' ## Investigate the data catalogue
#' data_catalogue %>%
#'   str()
#'
#' # Remove the data file
#' data_catalogue <- remove_data_file(
#'   file_to_remove = "data_dummy.tsv",
#'   root = pkg_root)
#'
#' ## Investigate the result
#' data_catalogue %>%
#'   str()
#'
#' # Clean up the package root - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
remove_data_file <- function(
  file_to_remove,
  root,
  data_catalogue = NULL,
  save_catalogue = TRUE)
{
# Check prerequisites -----------------------------------------------------
  file_to_remove %>%
    assertive.types::assert_is_a_string()

  root %>%
    assert_is_a_valid_package_root()

  if(
    data_catalogue %>%
      is.null())
  {
    data_catalogue <- root %>%
      load_data_file_as_object()
  }
  data_catalogue %>%
    assertive.types::assert_is_list() %>%
    assertive.properties::assert_has_all_attributes(
      c("default_compression_algo", "default_hashing_algo"))

  relative_raw_data_target_path <- file.path(
    "inst",
    "extdata",
    file_to_remove %>%
      basename() %>%
      paste0(".zip"))
  raw_data_target_path <- root %>%
    file.path(relative_raw_data_target_path)

  relative_r_object_target_path <- file.path(
    "data",
    file_to_remove %>%
      basename() %>%
      paste0(".rda"))
  r_object_target_path <- root %>%
    file.path(relative_r_object_target_path)

  save_catalogue %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------
  # Remove compressed version of file from package infrastructure
  raw_data_target_path %>%
    file.remove()

  # Remove the on-disk representation of the R object
  r_object_target_path %>%
    file.remove()

  # TODO?: Remove package dependencies from DESCRIPTION

  # Remove files from .Rbuildignore (as appropriate)
  remove_from_rbuildignore <- c(
      relative_raw_data_target_path,
      relative_r_object_target_path) %>%
    paste0("^", gsub("\\.", "\\\\.", .), "$")
  root %>%
    file.path(".Rbuildignore") %>%
    readLines() %>%
    setdiff(remove_from_rbuildignore) %>%
    writeLines(
      root %>%
        file.path(".Rbuildignore"))

  # Remove files from .gitignore (as appropriate)
  manage_gitignore(
    gitignore_file = file.path(root, ".gitignore"),
    relative_path = c(
      relative_raw_data_target_path,
      relative_r_object_target_path),
    state = "absent")

  # Update data_catalogue
  data_catalogue[[file_to_remove %>% basename()]] <- NULL

  # If requested: save data_catalogue
  if(save_catalogue){
    devtools::use_data(
      data_catalogue,
      pkg = root,
      internal = FALSE,
      overwrite = TRUE,
      compress = attr(
        data_catalogue,
        "default_compression_algo"))
  }
  # (Invisibly) return
  data_catalogue %>%
    invisible()
}
