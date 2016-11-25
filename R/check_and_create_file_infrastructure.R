check_and_create_file_infrastructure <- function(
  root = getwd())
{
# Check prerequisites -----------------------------------------------------
  root %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_dirs()

# Processing --------------------------------------------------------------
  # Build proper paths
  required_file_paths <- required_files %>%
    build_path(root =root)
  # Check what's not present
  needs_creation <- required_file_paths %>%
    sapply(file.exists) %>%
    magrittr::not()
  # Create what's necessary
  required_file_paths %>%
    magrittr::extract(needs_creation) %>%
    pathological::create_files()
}
