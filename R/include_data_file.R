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
  file_gitignore = FALSE,
  file_rbuildignore = FALSE,
  compression_algo = NULL,
  hashing_algo = NULL,
  save_catalogue = TRUE,
  ...)
{
# Check prerequisites -----------------------------------------------------
  file_to_include %>%
    assertive.types::assert_is_a_string()

  root %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_dirs()

  if(
    data_catalogue %>%
      is.null())
  {
    import_env <- new.env()
    root %>%
      file.path("data", "data_catalogue.rda") %>%
      load(envir = import_env)
    data_catalogue <- import_env %>%
      magrittr::extract2("data_catalogue")
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
    datapackageR:::assert_all_are_function_names()

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
      assertive.sets::assert_is_subset(utils::installed.packages())
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
  # Retreive the file if it is not locally present
  if(file_is_url){
    ## Make the request
    if(file_user %>%
       is.null())
    {
      httr_get <- file_to_include %>%
        httr::GET()
    } else {
      httr_get <- file_to_include %>%
        httr::GET(
          httr::authenticate(
            user = file_user,
            password = file_password,
            ...))
    }
    ## Check for errors
    if( httr_get %>%
        httr::http_error())
    {
      httr_status <- httr_get %>%
        httr::http_status()
      stop(
        "Can't access URL: ",
        httr_status %>%
          magrittr::extract2("message"))
    }
    ### Extract the content
    httr_content <- httr_get %>%
      httr::content(as = "raw")
    ### Write the content to file
    tmp_dir <- tempdir()
    file_to_include <- file.path(
      tmp_dir,
      file_to_include %>%
        basename())
    writeBin(
      object = httr_content,
      con = file_to_include)
  }

  # Insert compressed version of file into package infrastructure
  utils::zip(
    zipfile = raw_data_target_path,
    files = file_to_include,
    flags = "-D")

  # Capture hashes
  hash_uncompressed <- file_to_include %>%
    digest::digest(
      algo = hashing_algo,
      file = TRUE)
  hash_compressed <- raw_data_target_path %>%
    digest::digest(
      algo = hashing_algo,
      file = TRUE)

  # Parse the data
  tmp_object <- file_reading_function %>%
    do.call(
      c(file_to_include, file_reading_options) %>%
        as.list())

  # Rename the object and write it out
  assign(
    file_to_include %>%
      basename(),
    tmp_object)
  save(
    list = file_to_include %>%
      basename(),
    file = root %>%
      file.path(
        "data",
        file_to_include %>%
          basename %>%
          paste0(".rda")),
    compress = compression_algo)
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
      Hashing.Algo = hashing_algo,
      Hash.Uncompressed = hash_uncompressed,
      Hash.Compressed = hash_compressed,
      File.Reading.Function = file_reading_function,
      File.Reading.Option = file_reading_options,
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
