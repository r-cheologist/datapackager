#' @export
templating <- function(
  template,
  replacements = list(),
  target = NULL)
{
# Check prerequisites -----------------------------------------------------
  template %>%
    assertive.types::assert_is_a_string() %>%
    assertive.files::assert_all_are_readable_files()

  replacements %>%
    assertive.types::assert_is_list() %>%
    assertive.properties::assert_has_names() %>%
    lapply(assertive.types::assert_is_a_string)

  if(
    target %>%
    is.null() %>%
    magrittr::not())
  {
    target %>%
      assertive.types::assert_is_a_string()
  }

# Processing --------------------------------------------------------------
  # Read the template
  file_content <- template %>%
    readLines()

  # Perform the replacements
  for(ins in replacements %>% names){
    file_content %<>%
      stringi::stri_replace_all_fixed(
        replacements = ins %>%
          paste0("<", ., ">"),
        replacement = replacements %>%
          magrittr::extract2(ins))
  }

  # Write out (as appropriate)
  if(
    target %>%
    is.null() %>%
    magrittr::not())
  {
    file_content %>%
      writeLines(target)
  }

  # Return (invisibly)
  file_content %>%
    invisible()
}
