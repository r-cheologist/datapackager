#' @export
init <- function(
  root = getwd(),
  default_compression_algo = c("xz", "bzip2", "gzip"),
  default_hashing_algo = c("sha512", "md5", "sha1", "crc32", "sha256",
                           "xxhash32", "xxhash64", "murmur32"),
  files_to_include = NULL,
  file_is_url = FALSE,
  file_user = NULL,
  file_password = NULL,
  file_reading_function = NULL,
  file_reading_options = NULL,
  file_reading_package_dependencies = NULL,
  file_distributable = TRUE,
  file_gitignore = !file_distributable,
  file_rbuildignore = !file_distributable,
  use_rstudio = TRUE)
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

    file_is_url %>%
      assertive.types::assert_is_logical() %>%
      length() %>%
      assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
    file_is_url %<>%
      length() %>%
      switch(
        "1" = rep(
          file_is_url,
          files_to_include %>% length()),
        file_is_url)

    files_to_include %>%
      magrittr::extract(
        file_is_url %>%
          magrittr::not()) %>%
      assertive.files::assert_all_are_readable_files()
    if(file_user %>%
       is.null() %>%
       magrittr::not())
    {
      file_user %>%
        assertive.types::assert_is_character() %>%
        length() %>%
        assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
    }

    if(file_password %>%
       is.null() %>%
       magrittr::not())
    {
      file_password %>%
        assertive.types::assert_is_character() %>%
        length() %>%
        assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
    }

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

    if(
      file_distributable %>%
      is.null() %>%
      magrittr::not())
    {
      file_distributable %>%
        assertive.types::assert_is_logical() %>%
        length() %>%
        assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
    }

    file_gitignore %>%
      assertive.types::assert_is_logical() %>%
      length() %>%
      assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))

    file_rbuildignore %>%
      assertive.types::assert_is_logical() %>%
      length() %>%
      assertive.sets::assert_is_subset(c(1, files_to_include %>% length()))
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
      tmp_file_is_url <- file_is_url %>%
        magrittr::extract2(i)
      tmp_file_user <- file_user %>%
        length() %>%
        switch(
          "1" = file_user,
          file_user %>%
            magrittr::extract2(i))
      tmp_file_password <- file_password %>%
        length() %>%
        switch(
          "1" = file_password,
          file_password %>%
            magrittr::extract2(i))
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
      tmp_file_distributable <- file_distributable %>%
        length() %>%
        switch(
          "0" = NULL,
          "1" = file_distributable,
          file_distributable %>%
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
          file_is_url = tmp_file_is_url,
          file_user = tmp_file_user,
          file_password = tmp_file_password,
          file_reading_function = tmp_file_reading_function,
          file_reading_options = tmp_file_reading_options,
          file_reading_package_dependencies = file_reading_package_dependencies,
          file_distributable = tmp_file_distributable,
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
          packageVersion() %>%
          as.character()),
      target = root %>%
        file.path("R", template_name))

  for(pkg in c("assertive.sets", "assertive.types", "digest", "magrittr", "testthat", "utils")){
    devtools::use_package(
      package = pkg,
      type = "Imports",
      pkg = root)
  }

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
  for(pkg in c("assertive.types", "assertive.sets", "httr", "magrittr")){
    devtools::use_package(
      package = pkg,
      type = "Imports",
      pkg = root)
  }

  # Install a script documenting this call
  c("# The R package infrastructure was largely generated using",
      "# 'datapackageR::init' as follows.") %>%
    c(
      sys.calls()[[1]] %>%
        deparse()) %>%
    writeLines(
      con = root %>%
        file.path("inst", "scripts", "create_pkg_infrastructure.R"))

# Todo --------------------------------------------------------------------
#stop("implement URL retrieval (testing, new call_reteive")
}

