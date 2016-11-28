#' @export
remove_data_file <- function(
  data_catalogue,
  root,
  file_to_remove,
  save_catalogue = TRUE)
{
# Check prerequisites -----------------------------------------------------
  data_catalogue %>%
    assertive.types::assert_is_data.frame() %>%
    assertive.properties::assert_has_all_attributes(
      c("default_compression_algo", "default_hashing_algo")) %>%
    colnames() %>%
    assertive.sets::assert_are_set_equal(c("File", "Hashing.Algo",
      "Hash.Uncompressed", "Hash.Compressed", "File.Reading.Function",
      "File.Reading.Option", "File.Git.Ignore", "File.R.Buildignore"))

  root %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_dirs()

  file_to_remove %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_readable_files() %>%
    assertive.sets::assert_is_subset(
      data_catalogue %>%
        magrittr::extract2("File"))

  relative_raw_data_target_path <- file.path(
    "inst",
    "extdata",
    file_to_include %>%
      basename() %>%
      paste0(".zip"))
  raw_data_target_path <- root %>%
    file.path(relative_raw_data_target_path) %>%
    assertive.files::assert_any_are_existing_files()

  relative_r_object_target_path <- file.path(
    "data",
    file_to_include %>%
      basename() %>%
      paste0(".rda"))
  r_object_target_path <- root %>%
    file.path(relative_r_object_target_path) %>%
    assertive.files::assert_any_are_readable_files()

  save_catalogue %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------
  # Remove compressed version of file into package infrastructure
  raw_data_target_path %>%
    file.remove()

  # Remove R object representation
  r_object_target_path %>%
    file.remove()

  # Remove files from .Rbuildignore (as appropriate)
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

  # Remove files from .gitignore (as appropriate)
  manage_gitignore(
    gitignore_file = file.path(root, ".gitignore"),
    relative_path = c(
      relative_raw_data_target_path,
      relative_r_object_target_path),
    state = "absent")

  # Update data_catalogue
  data_catalogue %<>%
    magrittr::extract(
      data_catalogue %>%
        magrittr::extract2("File") %>%
        magrittr::is_in(file_to_remove) %>%
        magrittr::not(),
    )

  # If requested: save data_catalogue
  if(save_catalogue){
    data_catalogue %>%
      devtools::use_data(
        pkg = root,
        internal = FALSE,
        overwrite = TRUE,
        compress = compression_algo)
    invisible(TRUE)
  } else {
    ## (Invisibly) return
    data_catalogue %>%
      invisible()
  }
}
