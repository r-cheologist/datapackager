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

  # Provide documentation for the catalogue
  template_name <- "data-data_catalogue.R"
  system.file(
      file.path("templates", template_name),
      package = "datapackageR",
      mustWork = TRUE) %>%
    templating(
      replacements = list(
        PACKAGENAME = root %>%
          basename()),
      target = root %>%
        file.path("R", template_name))

  # Install the testthat tests that check the catalogue and the functions they need
  template_name <- "test-data_catalogue.R"
  system.file(
      file.path("templates", template_name),
      package = "datapackageR",
      mustWork = TRUE) %>%
    templating(
      replacements = list(
        PACKAGENAME = root %>%
          basename()),
      target = root %>%
        file.path("tests", "testthat", template_name))

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

