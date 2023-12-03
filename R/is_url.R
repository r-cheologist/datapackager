#' @noRd
is_url <- function(
  x,
  protocols = c("https", "http", "ftp", "ftps"))
{

# Check prerequisites -----------------------------------------------------
  x %>% assert_is_a_string()

  protocols %<>%
    assert_is_character() %>%
    paste0("://")

# Process -----------------------------------------------------------------
  x %>%
    stringi::stri_startswith_fixed(
      protocols,
      opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)) %>%
    any() %>%
    return()
}
