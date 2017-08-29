#' @title init
#' @aliases init
#' @description Generating the \code{R} package infrastructure used by
#' \pkg{datapackageR}.
#' @details Heavily relying on functionality provided by \pkg{devtools}, this
#' function initializes in the file system the infrastructure for an \code{R}
#' package build around (raw) data to be analyzed.
#'
#' In detail the function proceeds as follows:
#' \enumerate{
#'   \item The package \code{root} directory is created (and partially populated)
#'     using \code{\link[devtools]{create}}, using the \code{use_rstudio}
#'     paramter as appropriate.
#'   \item As \pkg{testthat}-dependent components are included, the
#'      corresponding infrastructure is initialized using
#'      \code{\link[devtools]{use_testthat}}.
#'   \item Following the standard packaging structure, directories \code{data},
#'     \code{inst/extdata} & \code{inst/scripts} are created for downstream use.
#'   \item Next, a \code{\link{list}}-based object named
#'     \code{\link{data_catalogue}} is created (and eventually saved to
#'     \code{data}) with \code{default_compression_algo} and
#'     \code{default_hashing_algo} as \code{\link{attributes}}. This is the
#'     central component of the data/content tracking at the core of
#'     \pkg{datapackageR}. See \code{\link{data_catalogue}} for details.
#'   \item If \code{files_to_include != NULL}, \code{\link{include_data_file}}
#'     is used to include the files given into the packaging structure.
#'   \item The \code{\link{data_catalogue}} (and it's documentation) are saved
#'     in the \code{root}-based structure.
#'   \item \pkg{testthat}-borne functionality is installed into the new package
#'     to enable automated (integrity)checking of the data packaged etc.
#'   \item \pkg{datapackageR} then adds further functions to the new,
#'     data-wrapping package, which enable e.g. retrieval of remote data etc.
#'     without having \pkg{datapackageR} loaded - or even present.
#'   \item An \code{R} script documenting the \pkg{datapackageR} call used to
#'     generate the new package infrastructure is added to its
#'     \code{inst/scripts} directory.
#'   \item Finally, packages required by the installed functionally (and
#'     potentially by implied \code{\link{include_data_file}} usage).
#' }
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure is to reside. The directory
#' must be nonexistent. \code{basename(root)} corresponds to the name of the
#' package and must thus conform to \code{R}s package naming conventions.
#' @param default_compression_algo Single \code{\link{character}} defining the
#' default method for compression of \code{R} objects stored in the package
#' infrastructure using \code{\link[devtools]{use_data}} (see there for
#' details). Defaulting to \code{xz}, which usually gives best (most compact)
#' results.
#' @param default_hashing_algo Single \code{\link{character}} naming the
#' cryptographic hashing function used for integrity testing of objects/files
#' via \code{\link[digest]{digest}} (see there for details). Defaulting to the
#' strong \code{sha512} option.
#' @param files_to_include \code{\link{character}} representing paths to data
#' files included using \code{\link{include_data_file}} (may also be URLs; see
#' \code{file_is_url} option of \code{\link{include_data_file}}).
#' @param use_rstudio Single \code{\link{logical}} handed to
#' \code{\link[devtools]{create}} and indicating whether infrastructure used by
#' \code{RStudio} is to be generated in the resulting data-wrapping package.
#' @param ... Paramters handed through to \code{\link{include_data_file}}. See
#' there for details
#' @return Returns \code{\link{TRUE}} via \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{save}}, \code{\link[digest]{digest}},
#' \code{\link{include_data_file}}, \code{\link[devtools]{create}}
#' @examples
#' # Load tools
#' library(magrittr)
#'
#' # Simple Example: Just generate infrastructure
#' ## Define a package root
#' pkg_root <- tempdir() %>%
#'   file.path("packagetest")
#'
#' ## Create the infrastructure
#' init(
#'   root = pkg_root)
#'
#' ## (Crudely) investigate the result
#' ### In the file system ...
#' pkg_root %>%
#'   list.files(
#'     full.names = TRUE,
#'     recursive  = TRUE)
#'
#' ### The data catalogue ...
#' tmp_env <- new.env()
#' pkg_root %>%
#'   file.path("data/data_catalogue.rda") %>%
#'   load(envir = tmp_env)
#' tmp_env$data_catalogue %>%
#'   str()
#'
#' # A more complex example with immediate data inclusion
#' ## Define a package root
#' pkg_root2 <- tempdir() %>%
#'   file.path("packagetest2")
#'
#' ## Create a dummy data file
#' data.frame(
#'   x   = 1,
#'   y   = 1:10,
#'   fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
#'   write.table(
#'     file      = file.path(dirname(pkg_root2), "data_dummy.tsv"),
#'     sep       = "\t",
#'     col.names = TRUE,
#'     row.names = FALSE)
#'
#' ## Create the package infrastructure, including the dummy file
#' init(
#'   root = pkg_root2,
#'   files_to_include = file.path(dirname(pkg_root2), "data_dummy.tsv"),
#'   file_reading_function = "read.csv",
#'   file_reading_options = list(sep = "\t", stringsAsFactors = FALSE))
#'
#' ## (Crudely) investigate the result
#' ### In the file system ...
#' pkg_root2 %>%
#'   list.files(
#'     full.names = TRUE,
#'     recursive  = TRUE)
#'
#' ### The data catalogue ...
#' tmp_env <- new.env()
#' pkg_root2 %>%
#'   file.path("data/data_catalogue.rda") %>%
#'   load(envir = tmp_env)
#' tmp_env$data_catalogue %>%
#'   str()
#'
#' # Clean up the package roots - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' unlink(pkg_root2, recursive = TRUE)
#' @export
init <- function(
  root = getwd(),
  default_compression_algo = c("xz", "bzip2", "gzip"),
  default_hashing_algo = c("sha512", "md5", "sha1", "crc32", "sha256",
                           "xxhash32", "xxhash64", "murmur32"),
  files_to_include = NULL,
  use_rstudio = TRUE,
  ...)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root

  data_catalogue_file <- root %>%
    file.path("data", "data_catalogue.rda")
  if(data_catalogue_file %>%
     file.exists())
  {
    stop(
      "'",
      data_catalogue_file,
      "' exists. Traces of prior initializations must be manually removed.")
  }

  default_compression_algo %<>%
    assertive.types::assert_is_character() %>%
    match.arg(
      choices = c("xz", "bzip2", "gzip"),
      several.ok = FALSE)

  default_hashing_algo %<>%
    assertive.types::assert_is_character() %>%
    match.arg(
      choices = c("sha512", "md5", "sha1", "crc32", "sha256", "xxhash32",
                  "xxhash64", "murmur32"),
      several.ok = FALSE)

  if(
    files_to_include %>%
      is.null() %>%
      magrittr::not())
  {
    files_to_include %>%
      assertive.types::assert_is_character()
  }

  use_rstudio %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------
  # Create infrastructure
  devtools::create(path = root, rstudio = use_rstudio)
  devtools::use_testthat(pkg = root)
  root %>%
    file.path("inst", "extdata") %>%
    dir.create(recursive = TRUE)
  root %>%
    file.path("inst", "scripts") %>%
    dir.create(recursive = TRUE)
  root %>%
    file.path("data") %>%
    dir.create()

  # Create the data catalogue
  data_catalogue <- list()
  attr(data_catalogue, "default_compression_algo") <- default_compression_algo
  attr(data_catalogue, "default_hashing_algo") <- default_hashing_algo

  # If present, iterate over data files to be integrated
  if (
    files_to_include %>%
    is.null() %>%
    magrittr::not())
  {
    for(i in files_to_include)
    {
      ## Include the files into the infrastructure
      data_catalogue <- i %>%
        include_data_file(
          root = root,
          data_catalogue = data_catalogue,
          save_catalogue = FALSE,
          ...)
    }
  }

  # Write out the catalogue
  devtools::use_data(
    data_catalogue,
    pkg = root,
    internal = FALSE,
    overwrite = FALSE,
    compress = default_compression_algo)

  # Install the testthat tests that check the catalogue and the functions they need
  template_name <- "test_data_integrity.R"
  system.file(
    file.path("templates", template_name),
    package = "datapackageR",
    mustWork = TRUE) %>%
    templating(
      replacements = list(
        DATAPACKAGERVERSION = "datapackageR" %>%
          utils::packageVersion() %>%
          as.character()),
      target = root %>%
        file.path("R", template_name))

  pkg_to_import_from <- c(
    "assertive.sets",
    "assertive.types",
    "digest",
    "magrittr",
    "testthat",
    "utils")

  # Install datapackageR functionality
  warning("Upon release this should be replaced by a dependency on 'datapackageR'.")
  script_files <- c(
      "data-data_catalogue.R",
      "reexport.R",
      "retrieve_remote_data.R") %>%
    sapply(
      function(x){
        system.file(
          file.path("sharedfunctions",x),
          package="datapackageR",
          mustWork = TRUE)
      })
  for(sf in script_files){
    file.copy(
    sf,
    root %>%
      file.path(
        "R",
        sf %>%
          basename()))
  }
  pkg_to_import_from %<>% c(
    "assertive.sets",
    "assertive.types",
    "httr",
    "magrittr")

  # Install a script documenting this call
  c("# The R package infrastructure was largely generated using",
      "# 'datapackageR::init' as follows.") %>%
    c(
      sys.calls()[[1]] %>%
        deparse()) %>%
    writeLines(
      con = root %>%
        file.path("inst", "scripts", "00_create_pkg_infrastructure.R"))

  pkg_to_import_from %<>%
    unique()
  for(pkg in pkg_to_import_from)
  {
    devtools::use_package(
      package = pkg,
      type = "Imports",
      pkg = root)
  }

  pkg_to_recommend <- "datapackageR" %>%
    unique()
  for(pk in pkg_to_recommend)
  {
    devtools::use_package(
      package = pkg,
      type = "suggests",
      pkg = root)
  }

# Todo --------------------------------------------------------------------
#stop("implement URL retrieval (testing, new call_reteive")

# Provide silent return value ---------------------------------------------
  TRUE %>%
    invisible()
}

