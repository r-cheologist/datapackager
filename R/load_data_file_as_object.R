load_data_file_as_object <- function(
  path = file.path(".", "data", "data_catalogue.rda"),
  name = pathological::strip_extension(basename(path)))
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
