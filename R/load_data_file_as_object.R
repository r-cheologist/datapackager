load_data_file_as_object <- function(
  root,
  relative_path = file.path("data", "data_catalogue.rda"))
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root()

# Process -----------------------------------------------------------------
  import_env <- new.env()
  root %>%
    file.path(relative_path) %>%
    load(envir = import_env)
  import_env %>%
    magrittr::extract2("data_catalogue") %>%
    return()
}
