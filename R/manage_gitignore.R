manage_gitignore <- function(
  gitignore_file = file.path(getwd(), ".gitignore"),
  relative_path,
  state = c("present","absent"))
{

# Check prerequisites -----------------------------------------------------
  gitignore_file %>%
    assertive.types::assert_is_a_string()

  relative_path %>%
    assertive.types::assert_is_character() %>%
    assertive.files::assert_all_are_readable_files()

  state %>%
    assertive.types::assert_is_character() %>%
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
      winslash = "/")

  # Append/delete path (as appropriate)
  if(state == "present"){
    file_contents %<>%
      c(relative_path)
  } else {
    for(path in relative_path){
      file_contents %<>%
        magrittr::extract(
          file_contents %>%
            stringi::stri_detect_fixed(
              pattern = path,
              negate = TRUE))
    }
  }

  # Uniquify, sort and write out
  file_contents %>%
    unique() %>%
    sort() %>%
    writeLines(
      con = gitignore_file)

  TRUE %>%
    invisible()
}
