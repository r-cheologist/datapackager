#' @title include_data_file
#' @aliases include_data_file
#' @description Integrating a data file into \pkg{datapackageR}s infrastructure.
#' @details The function proceeds as follows:
#' \enumerate{
#'   \item If the file does not reside locally, it is downloaded in a first step
#'     (depending on \code{File.Is.Remote}),
#'      using \code{\link{retrieve_remote_file}}.
#'   \item Next, a \code{zip} compressed version of the file is saved to
#'     \code{inst/extdata} of the package infrastructure, using the internal
#'     function \code{save_zipfile}.
#'   \item For future integrity checking, cryptographic hashes are captured for
#'     both the uncompressed and \code{zip}-compressed versions of the file
#'     (using \code{hashing_algo}).
#'   \item Using the function & options provided by \code{file_reading_function}
#'     and \code{file_reading_options}, respectively, the data file is parsed
#'     into an \code{R} object and that object is cryptographically hashed as
#'     well (again using \code{hashing_algo}). Using the internal helper
#'     function \code{data_rename_and_writeout}, the object is renamed to the
#'     original file's \code{\link{basename}} and saved into the \code{data}
#'     directory of the package's infrastructure while utilizing the data
#'     compression gien by \code{compression_algo}.
#'   \item If \code{file_reading_function} requires dependencies, the
#'     appropriate packages (given by \code{File.Reading.Package.Dependencies})
#'     are integrated into the \code{DESCRIPTION} file using
#'     \pkg{devtools}-provided tools.
#'   \item A \pkg{roxygen2}-based documentation stub is installed into the
#'     target package infrastructure.
#'   \item Depending on the options \code{File.Git.Ignore} and
#'     \code{File.R.Buildignore}, \code{.gitignore} and \code{.Rbuildignore} in
#'     the top level of the resulting package infrastructure are amended using
#'     the internal function \code{manage_gitignore} and
#'     \code{\link[devtools]{use_build_ignore}}, respectively.
#'   \item Using the information gathered, the \code{\link{data_catalogue}} file
#'     is updated, saved into the worked-on package infrastructure (if
#'     \code{save_catalogue == TRUE}) and finally (invisibly) returned.
#' }
#' @param file_to_include Single \code{\link{character}} representing a path to
#' a data file to be included (may also be an URL; see \code{file_is_url}).
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure is to reside. The directory
#' must have been \code{\link{init}}.
#' @param data_catalogue Single \code{link{character}} object representing the
#' relative path to the \code{\link{data_catalogue}}-containing \code{R} data
#' file within the packaging infrastructure (defaulting to
#' \code{data/data_catalogue.rda} if set to \code{\link{NULL}}).
#' @param file_is_url Single \code{\link{logical}} indicating whether the data
#' file included resides remotely and must be retreived for integration.
#' @param file_user Single \code{\link{character}} object used for authentication
#' where retrieval of a remote data file requires it.
#' @param file_password Single \code{\link{character}} object used for
#' authentication where retrieval of a remote data file requires it.
#' @param file_reading_function Single \code{\link{character}} object naming the
#' function used for parsing the data file into an \code{R} object.
#' @param file_reading_options Named \code{\link{list}} of options used by
#' \code{fiel_reading_function} for parsing the data file into an \code{R}
#' object.
#' @param file_reading_package_dependencies A \code{\link{character}} object
#' naming \code{R} extension packages required for \code{file_reading_function}.
#' @param file_distributable Single \code{\link{logical}} shortcut for
#' \code{file_gitignore == TRUE && file_rbuildignore ==TRUE}.
#' @param file_gitignore Single \code{\link{logical}} indicating whether the
#' objects resulting from the integration of the data file into the package
#' infrastructure are to be listed in \code{.gitignore} to provent their
#' integration into a \code{git} repository - handy e.g. in the case, where an
#' \code{R} package documenting a statistical analysis is to be distributed
#' without the underlying raw data.
#' @param file_rbuildignore Single \code{\link{logical}} indicating whether the
#' objects resulting from the integration of the data file into the package
#' infrastructure are to be listed in \code{.Rbuildignore} to provent their
#' integration into builds of the resulting \code{R}package - handy e.g. in the
#' case, where an \code{R} package documenting a statistical analysis is to be
#' distributed without the underlying raw data.
#' @param compression_algo Single \code{\link{character}} defining the method
#' for compression of \code{R} objects stored in the package infrastructure.
#' Defaults to the \code{default_compression_algo} \code{\link{attributes}} of
#' the \code{\link{data_catalogue}} used.
#' @param hashing_algo Single \code{\link{character}} defining the method
#' for cryptographic hashing (through \code{\link[digest]{digest}}; for details
#' see there) of \code{R} objects stored in the package infrastructure.Defaults
#' to the \code{default_hashing_algo} \code{\link{attributes}} of the
#' \code{\link{data_catalogue}} used.
#' @param save_catalogue Single \code{\link{logical}} indicating whether the
#' amended \code{\link{data_catalogue}} object is to be saved back to the
#' package infrastructure or just (silently) returned.
#' @return Returns a \code{\link{list}} of \code{\link{data_catalogue}}
#' characteristics via \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{init}}, \code{\link{remove_data_file}},
#' \code{\link[digest]{digest}}, \code{\link{data_catalogue}}
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
#' # Add a remote file (from Billing et al. (2016). Comprehensive transcriptomic
#' # and proteomic characterization of human mesenchymal stem cells reveals
#' # source specific cellular markers. Sci Rep 6, 21507.
#' # Licensed under the Creative Commons Attribution 4.0 International License.
#' # http://creativecommons.org/licenses/by/4.0/
#' # EXCLUDED FROM BUILDS
#' \donttest{
#'   if(requireNamespace("readxl", quietly = TRUE))
#'   {
#'     require(readxl)
#'     ## Complicated URL generation to circumvent line length restrictions
#'     tmp_url <- paste0(
#'       c("http://www.nature.com/article-assets/npg/srep",
#'         "2016/160209/srep21507/extref/srep21507-s4.xls"),
#'       collapse = "/")
#'     include_data_file(
#'       root = pkg_root,
#'       file_to_include = tmp_url,
#'       file_is_url = TRUE,
#'       file_reading_function = "read_excel",
#'       file_reading_options = list(skip = 1),
#'       file_reading_package_dependencies = "readxl",
#'       file_distributable = FALSE)
#'
#'     ## Investigate the data catalogue
#'     data_catalogue %>%
#'       str()
#'   }
#' }
#'
#' # Clean up the package root - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
include_data_file <- function(
  file_to_include,
  root,
  data_catalogue = NULL,
  file_is_url = FALSE,
  file_user = NULL,
  file_password = NULL,
  file_reading_function,
  file_reading_options = NULL,
  file_reading_package_dependencies = NULL,
  file_distributable = TRUE,
  file_gitignore = !file_distributable,
  file_rbuildignore = !file_distributable,
  compression_algo = NULL,
  hashing_algo = NULL,
  save_catalogue = TRUE)
{
# Check prerequisites -----------------------------------------------------
  file_to_include %>%
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
    file_to_include %>%
      basename() %>%
      paste0(".zip"))
  raw_data_target_path <- root %>%
    file.path(relative_raw_data_target_path)

  relative_r_object_target_path <- file.path(
    "data",
    file_to_include %>%
      basename() %>%
      paste0(".rda"))
  r_object_target_path <- root %>%
    file.path(relative_r_object_target_path)

  file_is_url %>%
    assertive.types::assert_is_a_bool()
  if(file_is_url %>%
     magrittr::not())
  {
    file_to_include %>%
      assertive.files::assert_all_are_readable_files()
  }

  if(file_user %>%
     is.null() %>%
     magrittr::not())
  {
    file_user %>%
      assertive.types::assert_is_a_string()
  }
  if(file_password %>%
     is.null() %>%
     magrittr::not())
  {
    file_password %>%
      assertive.types::assert_is_a_string()
  }
  c(file_user, file_password) %>%
    length() %>%
    assertive.sets::assert_is_subset(c(0,2))

  file_reading_function %>%
    assertive.types::assert_is_a_string() %>%
    assert_all_are_function_names()

  if(
    file_reading_options %>%
      is.null() %>%
      magrittr::not())
  {
    file_reading_options %>%
      assertive.types::assert_is_list()
  }

  if(
    file_reading_package_dependencies %>%
    is.null() %>%
    magrittr::not())
  {
    file_reading_package_dependencies %>%
      assertive.types::assert_is_character() %>%
      assertive.sets::assert_is_subset(
        utils::installed.packages())
  }

  if(
    file_distributable %>%
    is.null() %>%
    magrittr::not())
  {
    file_distributable %>%
      assertive.types::assert_is_a_bool()
  }

  file_gitignore %>%
    assertive.types::assert_is_a_bool()

  file_rbuildignore %>%
    assertive.types::assert_is_a_bool()

  if(
    compression_algo %>%
      is.null())
  {
    compression_algo <- attr(
      data_catalogue,
      "default_compression_algo")
  } else {
    compression_algo %<>%
      assertive.types::assert_is_character() %>%
      match.arg(
        choices = c("xz", "bzip2", "gzip"),
        several.ok = FALSE)
  }

  if(
    hashing_algo %>%
    is.null())
  {
    hashing_algo <- attr(
      data_catalogue,
      "default_hashing_algo")
  } else {
    hashing_algo %<>%
      assertive.types::assert_is_character() %>%
      match.arg(
        choices = c("sha512", "md5", "sha1", "crc32", "sha256", "xxhash32",
                    "xxhash64", "murmur32"),
        several.ok = FALSE)
  }

  save_catalogue %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------
  # Retreive the file if it is not local
  if(file_is_url){
    file_url <- file_to_include
    file_to_include %<>% retrieve_remote_file(
      user = file_user,
      password = file_password)
  }

  # Insert compressed version of file into package infrastructure
  save_zipfile(
    uncomp_path = file_to_include,
    root = root)

  # Capture hashes
  hash_uncompressed <- file_to_include %>%
    digest::digest(
      algo = hashing_algo,
      file = TRUE)
  hash_compressed <- raw_data_target_path %>%
    digest::digest(
      algo = hashing_algo,
      file = TRUE)

  # Parse the data & capture another hash
  tmp_object <- parse_data(
    path = file_to_include,
    reading_function = file_reading_function,
    reading_options = file_reading_options)
  hash_object <- tmp_object %>%
    digest::digest(
      algo = hashing_algo)

  # Rename the object and write it out
  data_rename_and_writeout(
    data_object = tmp_object,
    file_name = file_to_include,
    root = root,
    compression_algo = compression_algo)
  # devtools::use_data(
  #   as.symbol(file_to_include %>%
  #     basename()),
  #   pkg = root,
  #   internal = FALSE,
  #   overwrite = FALSE,
  #   compress = compression_algo)

  # Add package dependencies to DESCRIPTION
  if(
    file_reading_package_dependencies %>%
    is.null() %>%
    magrittr::not())
  {
    for(pk in file_reading_package_dependencies){
      devtools::use_package(
        package = pk,
        type = "Imports",
        pkg = root)
    }
  }

  # Install a roxygen2-based documentation stub
  template_name <- "data-data_documentation_stub.brew"
  system.file(
    file.path("templates", template_name),
    package = "datapackageR",
    mustWork = TRUE) %>%
    brew::brew(
      output = root %>%
        file.path(
          "R",
          template_name %>%
            pathological::replace_extension("R") %>%
            stringi::stri_replace_all_fixed(
              pattern = "data_documentation_stub",
              replacement = file_to_include
                %>% basename())))

  # Add file to .Rbuildignore (as appropriate)
  if(file_rbuildignore){
    devtools::use_build_ignore(
      files = c(
        relative_raw_data_target_path,
        relative_r_object_target_path),
      escape = TRUE,
      pkg = root)
  }

  # Add file to .gitignore (as appropriate)
  if(file_gitignore){
    manage_gitignore(
      gitignore_file = file.path(root, ".gitignore"),
      relative_path = c(
        relative_raw_data_target_path,
        relative_r_object_target_path),
      state = "present")
  }

  # Update data_catalogue
  data_catalogue[[file_to_include %>% basename()]] <- list(
      File = file_to_include %>%
        basename(),
      File.Is.Remote = file_is_url,
      Remote.Source = ifelse(file_is_url, file_url, NA_character_),
      Hashing.Algo = hashing_algo,
      Compression.Algo = compression_algo,
      Hash.Uncompressed = hash_uncompressed,
      Hash.Compressed = hash_compressed,
      Hash.Object = hash_object,
      File.Reading.Function = file_reading_function,
      File.Reading.Options = file_reading_options,
      File.Reading.Package.Dependencies = file_reading_package_dependencies,
      File.Git.Ignore = file_gitignore,
      File.R.Buildignore = file_rbuildignore)

  # If requested: save data_catalogue
  if(save_catalogue){
    devtools::use_data(
      data_catalogue,
      pkg = root,
      internal = FALSE,
      overwrite = TRUE,
      compress = compression_algo)
  }

  # (Invisibly) return
  data_catalogue %>%
    invisible()
}
