#' @noRd
load_data_file_as_object <- function(
  path = file.path(getwd(), "data", "data_catalogue.rda"),
  name = fs::path_ext_remove(basename(path)))
{
# Check prerequisites -----------------------------------------------------
  path %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_readable_files()

  name %>%
    assertive.types::assert_is_a_string() %>%
    assertive.code::assert_all_are_valid_variable_names()

# Process -----------------------------------------------------------------
  import_env <- new.env()
  path %>%
    load(envir = import_env)
  import_env %>%
    magrittr::extract2(name) %>%
    return()
}
