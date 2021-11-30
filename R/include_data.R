#' @title include_data
#' @aliases include_data
#' @description Integrating data into \pkg{datapackageR}s infrastructure.
#' @details
#' Supported data types are the following:
#' \describe{
#'   \item{\code{auto}:}{Instructs the function to attempt autonomous
#'     determination of the data type from \code{object_to_include} (default).}
#'   \item{\code{object}:}{Indicates \code{object_to_include} to be a (local)
#'     \code{R} object.}
#'   \item{\code{file}:}{Indicates \code{object_to_include} to be a (local)
#'     file requiring parsing using \code{parsing_function}.}
#'   \item{\code{url}:}{Indicates \code{object_to_include} to be a remote file
#'     requiring parsing using \code{parsing_function}.}
#'   \item{\code{file_rda} or \code{url_rda}:}{Indicate \code{object_to_include}
#'     to represent the path to a local (\code{file_*} or remote \code{url_*})
#'     \code{*.Rda/*.RData} file.}
#'   \item{\code{file_rds} or \code{url_rds}:}{Indicate \code{object_to_include}
#'     to represent the path to a local (\code{file_*} or remote \code{url_*})
#'     \code{*.Rds} file.}
#' }
#'
#' The function proceeds as follows:
#' \enumerate{
#'   \item Inputs are checked and \code{type} determination is attempted
#'     (as required).
#'   \item If the data does not reside locally, it is downloaded in a first step
#'     using \code{\link{retrieve_remote_file}}.
#'   \item Next and for data requiring parsing, a \code{zip} compressed version
#'     of the file is saved to \code{inst/extdata} of the package
#'     infrastructure, using the internal function \code{save_zipfile}.
#'   \item For future integrity checking and as applicable, cryptographic hashes
#'     are captured for both the uncompressed and \code{zip}-compressed versions
#'     of the file (using \code{hashing_algo}). If \code{saved_elements} does
#'     \strong{not} contain \code{extdata}, the \code{zip}-file is subsequently
#'     purged from the package infrastructure.
#'   \item As necessary (see \code{type}) and using the function & options
#'     provided by \code{parsing_function} and \code{parsing_options},
#'     respectively, data files are parsed into an \code{R} object.
#'   \item For all \code{type}s the implied \code{R} object is cryptographically
#'     hashed (again using \code{hashing_algo}).
#'   \item If \code{data} is requested by \code{saved_elements}, the internal
#'     helper function \code{data_rename_and_writeout} is used to rename the
#'     objectto the original's \code{\link{basename}} and saved into the
#'     \code{data} directory of the package's infrastructure while utilizing the
#'     data compression given by \code{compression_algo}.
#'   \item If \code{package_dependencies} are given (for either parsing a file
#'     or dealing with an \code{R} object), the appropriate packages are
#'     integrated into the \code{DESCRIPTION} file using \pkg{devtools}-provided
#'     tools.
#'   \item A \pkg{roxygen2}-based documentation stub is installed into the
#'     target package infrastructure.
#'   \item Depending on the options \code{gitignore} and
#'     \code{rbuildignore}, \code{.gitignore} and \code{.Rbuildignore} in
#'     the top level of the resulting package infrastructure are amended using
#'     the internal function \code{manage_gitignore} and
#'     \code{\link[usethis]{use_build_ignore}}, respectively.
#'   \item Using the information gathered, the \code{\link{data_catalogue}} file
#'     is updated, saved into the worked-on package infrastructure (if
#'     \code{save_catalogue == TRUE}) and finally (invisibly) returned.
#' }
#' @param object_to_include Single \code{\link{character}} representing a path
#' to a data file to be included (may also be an URL) or the name of an \code{R}
#' object.
#' @param type \code{\link{character}} object defining the supported data type.
#' See "Details'" for more.
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure is to reside. The directory
#' must have been created by \code{\link{init}} (ensuring the presence of the
#' infrastructure).
#' @param data_catalogue Single \code{link{character}} object representing the
#' relative path (within \code{root}) to the
#' \code{\link{data_catalogue}}-containing \code{R} data file within the
#' packaging infrastructure (defaulting to \code{data/data_catalogue.rda} if
#' set to \code{\link{NULL}}).
#' @param saved_elements \code{\link{character}} object indicating what aspects
#' of included data to save. \code{data} implies saving of (parsed) data objects
#' to the \code{data} directory within \code{root}, \code{extdata} saving of
#' unparsed files to \code{inst/extdata} within \code{root} (only used for the
#' \code{type}s \code{file} and \code{url}) and \code{all} is a shorthand for
#' both. When set to \code{NULL} \strong{only} list an additional entry in the
#' \code{\link{data_catalogue}}.
#' @param distributable Single \code{\link{logical}} shortcut for
#' \code{gitignore == TRUE && rbuildignore ==TRUE}.
#' @param gitignore Single \code{\link{logical}} indicating whether the
#' objects resulting from the integration of the data file into the package
#' infrastructure are to be listed in \code{.gitignore} to prevent their
#' integration into a \code{git} repository - handy e.g. in the case, where an
#' \code{R} package documenting a statistical analysis is to be distributed
#' without the underlying raw data.
#' @param rbuildignore Single \code{\link{logical}} indicating whether the
#' objects resulting from the integration of the data file into the package
#' infrastructure are to be listed in \code{.Rbuildignore} to provent their
#' integration into builds of the resulting \code{R}package - handy e.g. in the
#' case, where an \code{R} package documenting a statistical analysis is to be
#' distributed without the underlying raw data.
#' @param package_dependencies A \code{\link{character}} object naming \code{R}
#' extension packages required for \code{parsing_function} or dealing with the
#' \code{R} object to be included.
#' @param parsing_function Single \code{\link{character}} object naming the
#' function used for parsing the data file into an \code{R} object.
#' @param parsing_options Named \code{\link{list}} of options used by
#' \code{parsing_function} for reading the data file into an \code{R}
#' object.
#' @param user Single \code{\link{character}} object used for authentication
#' where retrieval of a remote data file requires it.
#' @param password Single \code{\link{character}} object used for
#' authentication where retrieval of a remote data file requires it.
#' @param save_catalogue Single \code{\link{logical}} indicating whether the
#' amended \code{\link{data_catalogue}} object is to be saved back to the
#' package infrastructure or just (silently) returned.
#' @param compression_algo Single \code{\link{character}} defining the method
#' for compression of \code{R} objects stored in the package infrastructure.
#' Defaults to the \code{default_compression_algo} \code{\link{attributes}} of
#' the \code{\link{data_catalogue}} used.
#' @param hashing_algo Single \code{\link{character}} defining the method
#' for cryptographic hashing (through \code{\link[digest]{digest}}; for details
#' see there) of \code{R} objects stored in the package infrastructure.Defaults
#' to the \code{default_hashing_algo} \code{\link{attributes}} of the
#' \code{\link{data_catalogue}} used.
#' @return Returns a \code{\link{list}} of \code{\link{data_catalogue}}
#' characteristics via \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{init}}, \code{\link{remove_data}},
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
#'     x   = 1,
#'     y   = 1:10,
#'     fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
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
#' # Add a remote file that needs parsing (from Billing et al. (2016).
#' # Comprehensive transcriptomic and proteomic characterization of human
#' # mesenchymal stem cells reveals source specific cellular markers. Sci Rep 6,
#' # 21507. Licensed under the Creative Commons Attribution 4.0 International
#' # License. http://creativecommons.org/licenses/by/4.0/
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
#'     data_catalogue <- include_data(
#'       object_to_include = tmp_url,
#'       root = pkg_root,
#'       parsing_function = "read_excel",
#'       parsing_options = list(skip = 1),
#'       package_dependencies = "readxl",
#'       distributable = FALSE)
#'
#'     ## Investigate the data catalogue
#'     data_catalogue %>%
#'       str()
#'   }
#' }
#'
#' # Add a local object
#' local_data_object <-list(A = LETTERS, B = letters)
#' data_catalogue <- include_data(
#'   object_to_include = "local_data_object",
#'   root = pkg_root)
#'
#' # Add a remote *.Rda (pasted to make CRAN line length requirements)
#' data_catalogue <- include_data(
#'   object_to_include = paste(
#'     "https://bitbucket.org",
#'     "graumannlabtools",
#'     "datapackager",
#'     "downloads",
#'     "remote_rda.Rda",
#'      sep = "/"),
#'   root = pkg_root)
#'
#' # Add a remote *.Rds (pasted to make CRAN line length requirements)
#' data_catalogue <- include_data(
#'   object_to_include = paste(
#'     "https://bitbucket.org",
#'     "graumannlabtools",
#'     "datapackager",
#'     "downloads",
#'     "remote_rds.Rds",
#'     sep = "/"),
#'   root = pkg_root)
#'
#' # Investigate the data catalogue
#' data_catalogue %>%
#'   str()
#'
#' # Have a look at the package structure
#' list.files(pkg_root, recursive = TRUE)
#'
#' # Clean up the package root - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' @export
include_data <- function(
  object_to_include,
  type                 = c("auto", "object", "file", "file_rda", "file_rds", "url", "url_rda", "url_rds"),
  root                 = getwd(),
  data_catalogue       = NULL,
  saved_elements       = c("all", "data", "extdata"),
  distributable        = TRUE,
  gitignore            = !distributable,
  rbuildignore         = !distributable,
  package_dependencies = NULL,
  parsing_function     = NULL,
  parsing_options      = NULL,
  user                 = NULL,
  password             = NULL,
  save_catalogue       = TRUE,
  compression_algo     = NULL,
  hashing_algo         = NULL
)
{
# Check prerequisites -----------------------------------------------------
## Only accepting string ----
  object_to_include %>%
    assertive.types::assert_is_a_string()

## Deal with input type ----
  type %<>%
    match.arg(
      choices = type,
      several.ok = FALSE)
  ## Automated type determination
  if (stringi::stri_detect_fixed(type, "auto"))
  {
    if (exists(object_to_include)) { type <- "object" }
    else {
      prefix <- ifelse(
        object_to_include %>%
          is_url(),
        "url",
        "file")
      ending <- object_to_include %>%
        analyse_extension()
      type <- ifelse(
        identical(ending, ""),
        prefix,
        paste(prefix, ending, sep = "_"))
    }
  }
  ## Conditional check(s)
  if (stringi::stri_startswith_fixed(type, "file"))
  {
    object_to_include %>%
      assertive.files::assert_all_are_readable_files()
  }

## Check package root & data_catalogue ----
  root %>%
    assert_is_a_valid_package_root
  if (is.null(data_catalogue))
  {
    data_catalogue <- root %>%
      file.path("data", "data_catalogue.rda") %>%
      load_data_file_as_object()
  }
  data_catalogue %>%
    assert_is_a_valid_data_catalogue()
  usethis::proj_set(path = root)

## Determine with which elements to save ----
  choices_saved_elements <- c("all", "data", "extdata")
  if (!is.null(saved_elements))
  {
    saved_elements %<>%
      match.arg(
        choices = choices_saved_elements,
        several.ok = TRUE)
    if (any(stringi::stri_detect_fixed(str = saved_elements, pattern = "all", opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE))))
    {
      saved_elements <- choices_saved_elements
    }
  }

## Deal with variables related to sharing/distribution ----
  if (!is.null(distributable))
  {
    distributable %>%
      assertive.types::assert_is_a_bool()
  }
  if (!is.null(gitignore))
  {
    gitignore %>%
      assertive.types::assert_is_a_bool()
  }
  if (!is.null(rbuildignore))
  {
    rbuildignore %>%
      assertive.types::assert_is_a_bool()
  }

## Check package dependencies needed for reading/parsing ----
  if (!is.null(package_dependencies))
  {
    package_dependencies %>%
      assertive.types::assert_is_character() %>%
      assertive.sets::assert_is_subset(
        utils::installed.packages())
  }

  # Check needs for data parsing
  if (type %in% c("file", "url"))
  {
    if (is.null(parsing_function))
    {
      stop("Type 'file' & 'url' require definition of 'parsing_function'.")
    }
  }

  if (!is.null(parsing_function))
  {
    parsing_function %>%
    assertive.types::assert_is_a_string() %>%
    assert_all_are_function_names()
  }
  if (!is.null(parsing_options))
  {
    parsing_options %>%
      assertive.types::assert_is_list()
  }

## Check needs for authenticated retrieval of remate data ----
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
  c(user, password) %>%
    length() %>%
    assertive.sets::assert_is_subset(c(0,2))

## Deal with algorithm selection ----
  if (is.null(compression_algo))
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

  hashing_algo %<>%
    match_function_arg(
      fun = digest::digest,
      fun_arg_name = "algo",
      several.ok = FALSE,
      default = attr(
        data_catalogue,
        "default_hashing_algo"))

## Indicating whether to only return the modified catalogue or also save it ... ----
  save_catalogue %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------
## Build paths ----
  relative_extdata_path <- make_extdata_path("", object_to_include)
  extdata_path <- make_extdata_path(root, object_to_include)

  relative_data_path <- make_data_path("", object_to_include)

## Retrieve the file if it is not local ----
  original_object_to_include <- object_to_include
  if (stringi::stri_startswith_fixed(type, "url"))
  {
    object_to_include %<>%
      retrieve_remote_file(
        user = user,
        password = password)
  }

## Operations specific to data NOT representing (serialized) R objects ----
  if (type %in% c("file", "url"))
  {
    ## Insert compressed version of file into package infrastructure
    ## Also done if cataloging only requested - to capture a hash from the zip file
    save_zipfile(
      uncomp_path = object_to_include,
      target_dir = root %>%
        file.path("inst", "extdata"))
    ## Capture hashes
    hash_uncompressed <- object_to_include %>%
      digest::digest(
        algo = hashing_algo,
        file = TRUE)
    hash_compressed <- extdata_path %>%
      digest::digest(
        algo = hashing_algo,
        file = TRUE)

    ## Remove the compressed file if only entry in catalog requested
    if (!("extdata" %in% saved_elements)) { unlink(extdata_path) }

    ## Parse the data & capture another hash
    tmp_object <- parse_data(
      path             = object_to_include,
      reading_function = parsing_function,
      reading_options  = parsing_options)
  }

## Operations specific to data representing (serialized) R objects ----
  if (type %in% c("object", "file_rda", "file_rds", "url_rda", "url_rds"))
  {
    # Deal with hashes that are not applicable
    hash_uncompressed <- NULL
    hash_compressed <- NULL
    # Deal with all possible data provisioning
    if (identical(type, "object"))
    {
      tmp_object <- get(x = object_to_include)
    } else if (stringi::stri_endswith_fixed(type, "rda"))
    {
      tmp_env <- new.env()
      object_to_include %>%
        load(envir = tmp_env)
      if (length(tmp_env) != 1)
      {
        warning(
          "Multiple objects contained in '",
          object_to_include,
          "'. Only the first one is extracted.")
      }
      tmp_object <- get(
        tmp_env %>%
          names() %>%
          magrittr::extract2(1),
        envir = tmp_env)
    } else if (stringi::stri_endswith_fixed(type, "rds"))
    {
      tmp_object <- object_to_include %>%
        readRDS()
    }
  }
## Capture a hash of the in-memory object ----
  hash_object <- tmp_object %>%
    digest::digest(
      algo = hashing_algo,
      file = FALSE)

## Rename the object and write it out ----
  if ("data" %in% saved_elements)
  {
    data_rename_and_writeout(
      data_object      = tmp_object,
      file_name        = object_to_include,
      root             = root,
      compression_algo = compression_algo)
  }

## Add package dependencies to DESCRIPTION ----
  if (!is.null(package_dependencies))
  {
    for (pk in package_dependencies)
    {
      usethis::use_package(
        package = pk,
        type    = "Imports")
    }
  }

## Install a roxygen2-based documentation stub ----
  template_name <- "data-data_documentation_stub.brew"
  system.file(
    file.path("templates", template_name),
    package  = "datapackageR",
    mustWork = TRUE) %>%
    brew::brew(
      output = root %>%
        file.path(
          "R",
          template_name %>%
            fs::path_ext_set("R") %>%
            stringi::stri_replace_all_fixed(
              pattern = "data_documentation_stub",
              replacement = object_to_include
              %>% basename())))

## Add file to .Rbuildignore (as appropriate) ----
  if (rbuildignore)
  {
    if ("extdata" %in% saved_elements)
    {
      usethis::use_build_ignore(
        files  = relative_extdata_path,
        escape = TRUE)
    }
    if ("data" %in% saved_elements)
    {
      usethis::use_build_ignore(
        files  = relative_data_path,
        escape = TRUE)
    }
  }

## Add file to .gitignore (as appropriate) ----
  if (gitignore)
  {
    if ("extdata" %in% saved_elements)
    {
      manage_gitignore(
        gitignore_file = file.path(root, ".gitignore"),
        relative_path = relative_extdata_path,
        state = "present")
    }
    if ("data" %in% saved_elements)
    {
      manage_gitignore(
        gitignore_file = file.path(root, ".gitignore"),
        relative_path = relative_data_path,
        state = "present")
    }
  }

## Update data_catalogue ----
  data_catalogue[[object_to_include %>% basename()]] <- list(
    Base.Name            = object_to_include %>%
      basename(),
    Data.Type            = type,
    Full.Name            = original_object_to_include,
    Hashing.Algo         = hashing_algo,
    Compression.Algo     = compression_algo,
    Hash.Uncompressed    = hash_uncompressed,
    Hash.Compressed      = hash_compressed,
    Hash.Object          = hash_object,
    Parsing.Function     = parsing_function,
    Parsing.Options      = parsing_options,
    Package.Dependencies = package_dependencies,
    Git.Ignore           = gitignore,
    R.Buildignore        = rbuildignore)

## If requested: save data_catalogue ----
  if (save_catalogue)
  {
    withr::with_dir(
      root,
      usethis::use_data(
        data_catalogue,
        internal = FALSE,
        overwrite = TRUE,
        compress = compression_algo))
  }

## (Invisibly) return ----
  data_catalogue %>%
    invisible()
}
