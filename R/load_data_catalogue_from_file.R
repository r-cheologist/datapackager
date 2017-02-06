load_data_catalogue_from_file <- function(
  root)
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assert_is_a_valid_package_root()

# Process -----------------------------------------------------------------
  import_env <- new.env()
  root %>%
    file.path("data", "data_catalogue.rda") %>%
    load(envir = import_env)
  import_env %>%
    magrittr::extract2("data_catalogue") %>%
    return()
}
