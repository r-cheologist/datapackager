#' @export
init <- function(
  root = getwd(),
  default_compression_algo = c("xz", "bzip2", "gzip"),
  default_hashing_algo = c("sha512", "md5", "sha1", "crc32", "sha256",
                           "xxhash32", "xxhash64", "murmur32"),
  files_to_include = NULL,
  file_reading_function = NULL,
  file_reading_options = NULL,
  file_reading_package_dependencies = NULL,
  file_gitignore = FALSE,
  file_rbuildignore = FALSE
)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assertive.types::assert_is_a_string() %>%
    c(
      root %>%
        dirname()) %>%
    assertive.files::assert_any_are_dirs()

  data_catalogue_file <- root %>%
    file.path("data", "data_catalogue.rda")
  if(data_catalogue_file %>%
     file.exists())
  {
    stop("'", data_catalogue_file, "' exists. Traces of prior initializations must be manually removed.")
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
      assertive.types::assert_is_character() %>%
      assertive.files::assert_all_are_readable_files()

    file_reading_function %>%
      assertive.types::assert_is_character() %>%
      length() %>%
      assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
    file_reading_function %>%
      datapackageR:::assert_all_are_function_names()

    if(
      file_reading_options %>%
        is.null() %>%
        magrittr::not())
    {
      file_reading_options %>%
        assertive.types::assert_is_list() %>%
        length() %>%
        assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
    }

    if(
      file_reading_package_dependencies %>%
      is.null() %>%
      magrittr::not())
    {
      file_reading_package_dependencies %>%
        assertive.types::assert_is_character() %>%
        assertive.sets::assert_is_subset(utils::installed.packages())
    }

    file_gitignore %>%
      assertive.types::assert_is_a_bool() %>%
      length() %>%
      assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))

    file_rbuildignore %>%
      assertive.types::assert_is_a_bool() %>%
      length() %>%
      assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
  }

# Processing --------------------------------------------------------------
  # Create infrastructure
  devtools::create(path = root, rstudio = TRUE)
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
  if(
    files_to_include %>%
    is.null() %>%
    magrittr::not())
  {
    for(i in files_to_include %>% seq_along())
    {
      ## Extract individual parameters
      tmp_file_to_include <- files_to_include %>%
        magrittr::extract2(i)
      tmp_file_reading_function <- file_reading_function %>%
        length() %>%
        switch(
          "1" = file_reading_function,
          file_reading_function %>%
            magrittr::extract2(i))
      tmp_file_reading_options <- file_reading_options %>%
        length() %>%
        switch(
          "0" = NULL,
          "1" = file_reading_options,
          file_reading_options %>%
            magrittr::extract2(i))
      tmp_file_gitignore <- file_gitignore %>%
        length() %>%
        switch(
          "1" = file_gitignore,
          file_gitignore %>%
            magrittr::extract2(i))
      tmp_file_rbuildignore <- file_rbuildignore %>%
        length() %>%
        switch(
          "1" = file_rbuildignore,
          file_rbuildignore %>%
            magrittr::extract2(i))

      ## Include the files into the infrastructure
      data_catalogue <- tmp_file_to_include %>%
        include_data_file(
          root = root,
          data_catalogue = data_catalogue,
          file_reading_function = tmp_file_reading_function,
          file_reading_options = tmp_file_reading_options,
          file_reading_package_dependencies = file_reading_package_dependencies,
          file_gitignore = tmp_file_gitignore,
          file_rbuildignore = tmp_file_rbuildignore,
          compression_algo = NULL,
          hashing_algo = NULL,
          save_catalogue = FALSE)
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

  # Install the testthat tests that check the catalogue
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

  # Install a script documenting this call
  c("# The R package infrastructure was largely generated using",
      "# 'datapackageR::init' as follows.") %>%
    c(
      sys.calls()[[1]] %>%
        deparse()) %>%
    writeLines(
      con = "root" %>%
        file.path("inst", "scripts", "create_pkg_infrastructure.R"))

# Todo --------------------------------------------------------------------
#stop("implement URL retrieval (include_data_file, testing, new call_reteive")
}

