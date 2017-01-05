#' @export
remove_data_file <- function(
  file_to_remove,
  root,
  data_catalogue = NULL,
  save_catalogue = TRUE)
{
# Check prerequisites -----------------------------------------------------
  file_to_remove %>%
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
    file_to_remove %>%
      basename() %>%
      paste0(".zip"))
  raw_data_target_path <- root %>%
    file.path(relative_raw_data_target_path)

  relative_r_object_target_path <- file.path(
    "data",
    file_to_remove %>%
      basename() %>%
      paste0(".rda"))
  r_object_target_path <- root %>%
    file.path(relative_r_object_target_path)

  save_catalogue %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------
  # Remove compressed version of file from package infrastructure
  raw_data_target_path %>%
    file.remove()

  # Remove the on-disk representation of the R object
  r_object_target_path %>%
    file.remove()

  # TODO?: Remove package dependencies from DESCRIPTION

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
  data_catalogue[[file_to_remove %>% basename()]] <- NULL

  # If requested: save data_catalogue
  if(save_catalogue){
    devtools::use_data(
      data_catalogue,
      pkg = root,
      internal = FALSE,
      overwrite = TRUE,
      compress = attr(
        data_catalogue,
        "default_compression_algo"))
  }
  # (Invisibly) return
  data_catalogue %>%
    invisible()
}
