#' @noRd
manage_gitignore <- function(
  gitignore_file = file.path(getwd(), ".gitignore"),
  relative_path,
  state = c("present","absent"))
{

# Check prerequisites -----------------------------------------------------
  gitignore_file %>% assert_is_a_string()

  relative_path %>% assert_is_character()

  state %>%
    assert_is_character() %>%
    match.arg(
      choices = c("absent", "present"),
      several.ok = FALSE)

# Processing --------------------------------------------------------------
  # Read out existing gitignore (or create the data structure empty)
  if(
    gitignore_file %>%
      assertive.files::is_readable_file())
  {
    file_contents <- gitignore_file %>%
      readLines()
  } else {
    file_contents <- c()
  }

  # Create .gitignore compatible path
  relative_path %<>%
    normalizePath(
      winslash = "/",
      mustWork = FALSE)

  # Append/delete path (as appropriate)
  if(state == "present"){
    file_contents %<>%
      union(relative_path)
  } else {
    file_contents %<>%
      setdiff(relative_path)
  }

  # Sort and write out
  file_contents %>%
    sort() %>%
    writeLines(
      con = gitignore_file)

  file_contents %>%
    invisible()
}
