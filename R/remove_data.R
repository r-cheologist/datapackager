#' @title remove_data
#' @aliases remove_data
#' @description Removing a data entry from \pkg{datapackageR}s infrastructure.
#' @details The function essentially reverses the steps taken by
#' \code{\link{include_data}} (documented there).
#' @param object_to_remove Single \code{\link{character}} representing the name
#' of a data set to be removed.
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure to be modified resides.
#' @param data_catalogue Single \code{link{character}} object representing the
#' relative path to the \code{\link{data_catalogue}}-containing \code{R} data
#' file within the packaging infrastructure (defaulting to
#' \code{data/data_catalogue.rda} if set to \code{\link{NULL}}).
#' @param keep_meta_data Single \code{\link{logical}} indicating
#' whether to \strong{only} remove stored raw and parsed data objects from
#' \code{inst/extdata} \code{data}, respectively, without touching the
#' \code{\link{data_catalogue}} (\code{TRUE} case), or to also purge the
#' corresponding entry from \code{\link{data_catalogue}} (\code{FALSE} case;
#' default).
#' @param save_catalogue Single \code{\link{logical}} indicating whether the
#' amended \code{\link{data_catalogue}} object is to be saved back to the
#' package infrastructure or just (silently) returned.
#' @return Returns a \code{\link{list}} of \code{\link{data_catalogue}}
#' characteristics via \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{include_data}}, \code{\link{data_catalogue}}
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
#' data_catalogue <- init(
#'   root = pkg_root)
#'
#' ## Investigate the data catalogue
#' data_catalogue %>%
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
#' data_catalogue <- include_data(
#'   object_to_include = file.path(dirname(pkg_root), "data_dummy.tsv"),
#'   root = pkg_root,
#'   parsing_function = "read.csv",
#'   parsing_options = list(sep = "\t", stringsAsFactors = FALSE))
#'
#' ## Investigate the data catalogue
#' data_catalogue %>%
#'   str()
#'
#' # Remove the data file
#' data_catalogue <- remove_data(
#'   object_to_remove = "data_dummy.tsv",
#'   root = pkg_root)
#'
#' ## Investigate the result
#' data_catalogue %>%
#'   str()
#'
#' # Clean up the package root - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
remove_data <- function(
  object_to_remove,
  root,
  data_catalogue = NULL,
  keep_meta_data = FALSE,
  save_catalogue = TRUE)
{
# Check prerequisites -----------------------------------------------------
  object_to_remove %>% assert_is_a_string()

  root %>%
    assert_is_a_valid_package_root()

  if (
    data_catalogue %>%
      is.null())
  {
    data_catalogue <- root %>%
      file.path("data", "data_catalogue.rda") %>%
      load_data_file_as_object()
  }
  data_catalogue %>%
    assert_is_a_valid_data_catalogue()

  keep_meta_data %>% assert_is_a_bool()

  save_catalogue %>% assert_is_a_bool()

# Processing --------------------------------------------------------------
  # Generate paths
  relative_raw_data_target_path <- make_extdata_path("", object_to_remove)
  raw_data_target_path <- make_extdata_path(root, object_to_remove)

  relative_r_object_target_path <- make_data_path("", object_to_remove)
  r_object_target_path <- make_data_path(root, object_to_remove)

  relative_data_documentation_path <- file.path(
    "R",
    paste0(
      "data-",
      object_to_remove %>%
        basename(),
      ".R"))
  data_documentation_path <- root %>%
    file.path(relative_data_documentation_path)

  # Remove compressed version of file from package infrastructure
  if (file.exists(raw_data_target_path))
  {
    raw_data_target_path %>%
      file.remove()
  }

  # Remove the on-disk representation of the R object
  if (file.exists(r_object_target_path))
  {
    r_object_target_path %>%
      file.remove()
  }

  # TODO?: Remove package dependencies from DESCRIPTION

  # Remove data set documentation(stub)
  if ((!keep_meta_data) && file.exists(data_documentation_path))
  {

    data_documentation_path %>%
      file.remove()
  }

  # Remove files from .Rbuildignore (as appropriate)
  if (!keep_meta_data)
  {
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
  }

  # Remove files from .gitignore (as appropriate)
  if (!keep_meta_data)
  {
    manage_gitignore(
      gitignore_file = file.path(root, ".gitignore"),
      relative_path = c(
        relative_raw_data_target_path,
        relative_r_object_target_path),
      state = "absent")
  }

  # Update data_catalogue
  if (!keep_meta_data)
  {
    data_catalogue[[object_to_remove %>% basename()]] <- NULL
  }

  # If requested: save data_catalogue
  if (save_catalogue){
    usethis::proj_set(path = root)
    usethis::use_data(
      data_catalogue,
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
