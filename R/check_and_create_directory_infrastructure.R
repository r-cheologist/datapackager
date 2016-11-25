check_and_create_directory_infrastructure <- function(
  root = getwd())
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_dirs()

# Processing --------------------------------------------------------------
  # Build proper paths
  required_directory_paths <- required_directories %>%
    build_path(root = root)
  # Check what's not present
  needs_creation <- required_directory_paths %>%
    sapply(dir.exists) %>%
    magrittr::not()
  # Create what's necessary
  required_directory_paths %>%
    magrittr::extract(needs_creation) %>%
    pathological::create_dirs()
}
