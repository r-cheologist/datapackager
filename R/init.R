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
#'     using \code{\link[devtools]{create}}, and the \code{use_rstudio}
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
#'   \item If \code{objects_to_include != NULL}, \code{\link{include_data}}
#'     is used to include the files given into the packaging structure.
#'   \item The \code{\link{data_catalogue}} (and it's documentation) are saved
#'     in the \code{root}-based structure.
#'   \item \pkg{testthat}-borne functionality is installed into the new package
#'     to enable automated (integrity)checking of the data packaged etc.
#'   \item \pkg{datapackageR} then adds further functions to the new,
#'     data-wrapping package, which enable e.g. retrieval of remote data etc.
#'   \item An \code{R} script documenting the \pkg{datapackageR} call used to
#'     generate the new package infrastructure is added to its
#'     \code{inst/scripts} directory.
#'   \item Finally, packages are added to the \code{DESCRIPTION} file as
#'     required (and potentially by implied \code{\link{include_data}} usage).
#' }
#' @param root Single \code{\link{character}} representing the path of the
#' directory in which the package infrastructure is to reside. The directory
#' cannot exist yet. \code{basename(root)} corresponds to the name of the
#' package and must thus conform to \code{R}s package naming conventions.
#' @param default_compression_algo Single \code{\link{character}} defining the
#' default method for compression of \code{R} objects stored in the package
#' infrastructure using \code{\link[usethis]{use_data}} (see there for
#' details). Defaulting to \code{xz}, which usually gives best (most compact)
#' results.
#' @param default_hashing_algo Single \code{\link{character}} naming the
#' cryptographic hashing function used for integrity testing of objects/files
#' via \code{\link[digest]{digest}} (see there for details). Defaulting to the
#' strong \code{sha512} option.
#' @param objects_to_include \code{\link{character}} representing paths to data
#' files included using \code{\link{include_data}} (may also be URLs).
#' @param use_rstudio Single \code{\link{logical}} handed to
#' \code{\link[devtools]{create}} and indicating whether infrastructure used by
#' \code{RStudio} is to be generated in the resulting data-wrapping package.
#' @param description \code{\link{list}} of descriptions values to override
#' default values or add additional values (see \code{\link[devtools]{create}}
#' for details). Setting this to \code{\link{NULL}} implies the defaults of the
#' underlying function.
#' @param ... Paramters handed through to \code{\link{include_data}}. See
#' there for details
#' @return Returns the resulting \code{\link{data_catalogue}} via
#' \code{\link{invisible}}.
#' @author Johannes Graumann
#' @seealso \code{\link{save}}, \code{\link[digest]{digest}},
#' \code{\link{include_data}}, \code{\link[devtools]{create}}
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
#' data_catalogue <- init(
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
#' data_catalogue %>%
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
#' data_catalogue <- init(
#'   root = pkg_root2,
#'   objects_to_include = file.path(dirname(pkg_root2), "data_dummy.tsv"),
#'   parsing_function = "read.csv",
#'   parsing_options = list(sep = "\t", stringsAsFactors = FALSE))
#'
#' ## (Crudely) investigate the result
#' ### In the file system ...
#' pkg_root2 %>%
#'   list.files(
#'     full.names = TRUE,
#'     recursive  = TRUE)
#'
#' ### The data catalogue ...
#' data_catalogue %>%
#'   str()
#'
#' # Clean up the package roots - ensure proper example testing by R CMD check
#' unlink(pkg_root, recursive = TRUE)
#' unlink(pkg_root2, recursive = TRUE)
#' @export
init <- function(
  root = getwd(),
  default_compression_algo = c("xz", "bzip2", "gzip"),
  default_hashing_algo = match_function_arg(
    arg = NULL,
    fun = digest::digest,
    fun_arg_name = "algo",
    several.ok = FALSE,
    default = "sha512"),
  objects_to_include = NULL,
  use_rstudio = TRUE,
  description = NULL,
  ...)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root

  data_catalogue_file <- root %>%
    file.path("data", "data_catalogue.rda")
  if (data_catalogue_file %>%
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
    match_function_arg(
      fun = digest::digest,
      fun_arg_name = "algo",
      several.ok = FALSE,
      default = "sha5121")

  if (
    objects_to_include %>%
      is.null() %>%
      magrittr::not())
  {
    objects_to_include %>%
      assertive.types::assert_is_character()
  }

  use_rstudio %>%
    assertive.types::assert_is_a_bool()

  if (
    description %>%
      is.null() %>%
      magrittr::not())
  {
    description %>%
      assertive.types::assert_is_list()
  }

# Processing --------------------------------------------------------------
  # Create infrastructure
  if (
    description %>%
    is.null())
  {
    devtools::create(
      path = root,
      rstudio = use_rstudio)
  } else {
    devtools::create(
      path = root,
      rstudio = use_rstudio,
      description = description)
  }
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
    objects_to_include %>%
    is.null() %>%
    magrittr::not())
  {
    for (i in objects_to_include)
    {
      ## Include the files into the infrastructure
      data_catalogue <- i %>%
        include_data(
          root = root,
          data_catalogue = data_catalogue,
          save_catalogue = FALSE,
          ...)
    }
  }

  # Write out the catalogue
  usethis::proj_set(path = root)
  usethis::use_data(
    data_catalogue,
    internal = FALSE,
    overwrite = FALSE,
    compress = default_compression_algo)

  # Install the testthat tests that check the catalogue and the functions they need
  # template_name <- "test_data_integrity.brew"
  # system.file(
  #     file.path("templates", template_name),
  #     package = "datapackageR",
  #     mustWork = TRUE) %>%
  #   brew::brew(
  #     output = root %>%
  #       file.path(
  #         "R",
  #         template_name %>%
  #           pathological::replace_extension("R")))

  template_name <- "testthat.brew"
  system.file(
      file.path("templates", template_name),
      package = "datapackageR",
      mustWork = TRUE) %>%
    brew::brew(
      output = root %>%
        file.path(
          "tests",
          template_name %>%
            pathological::replace_extension("R")))

  template_name <- "test-data_catalogue.brew"
  system.file(
    file.path("templates", template_name),
    package = "datapackageR",
    mustWork = TRUE) %>%
    brew::brew(
      output = root %>%
        file.path(
          "tests",
          "testthat",
          template_name %>%
            pathological::replace_extension("R")))

  pkg_to_import_from <- c(
    "assertive.base",
    "assertive.files",
    "assertive.properties",
    "assertive.sets",
    "assertive.types",
    "digest",
    "httr",
    "magrittr",
    "utils")

  # Install datapackageR functionality
  # warning("Upon release this should be replaced by a dependency on 'datapackageR'.")
  script_files <- c(
      "datapackageR-data-data_catalogue.R",
      "datapackageR-reexport.R") %>%
    sapply(
      function(x){
        system.file(
          file.path("sharedfunctions",x),
          package  = "datapackageR",
          mustWork = TRUE)
      })
  for (sf in script_files)
  {
    file.copy(
    sf,
    root %>%
      file.path(
        "R",
        sf %>%
          basename()))
  }
  # pkg_to_import_from %<>% c(
  #   "assertive.sets",
  #   "assertive.types",
  #   "magrittr")

  # Install a script documenting this call
  c("# The R package infrastructure was largely generated using",
      "# 'datapackageR::init' as follows.") %>%
    c(
      sys.calls()[[1]] %>%
        deparse()) %>%
    writeLines(
      con = root %>%
        file.path("inst", "scripts", "00_create_pkg_infrastructure.R"))

# Deal with package dependencies ------------------------------------------
  pkg_to_depend_on <- c(
    "datapackageR") %>%
    unique()
  for (pkg in pkg_to_depend_on)
  {
    devtools::use_package(
      package = pkg,
      type = "Depends",
      pkg = root)
  }

  pkg_to_import_from %<>%
    unique()
  for (pkg in pkg_to_import_from)
  {
    devtools::use_package(
      package = pkg,
      type = "Imports",
      pkg = root)
  }

  pkg_to_recommend <- c(
    "readxl") %>%
    unique()
  for (pk in pkg_to_recommend)
  {
    devtools::use_package(
      package = pkg,
      type = "Suggests",
      pkg = root)
  }

# Result-package internal processes ---------------------------------------
  devtools::document(pkg = root)

# Todo --------------------------------------------------------------------

# Provide silent return value ---------------------------------------------
  data_catalogue %>%
    invisible()
}

