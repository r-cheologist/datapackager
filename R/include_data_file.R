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
  save_catalogue = TRUE,
  ...)
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
      load_data_catalogue_from_file()
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
    file_to_include %<>% retrieve_remote_data(
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
