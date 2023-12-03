#' @noRd
analyse_extension <- function(
  x,
  extensions = list(rda = c("Rda", "RData"), rds = "Rds"),
  default = "")
{

# Check prerequisites -----------------------------------------------------
  x %>% assert_is_a_string()

  extensions %>%
    assert_is_list() %>%
    assert_has_names() %>%
    sapply(assert_is_character)

  default %>% assert_is_a_string()

# Processing --------------------------------------------------------------
  # Extract extensions
  extension <- x %>%
    stringi::stri_replace_all_regex(
      pattern = ".*\\.([^\\.]+)$",
      replacement = "$1")

  # Match to table(s)
  matching_table <- extensions %>%
    sapply(
      function(y){
        extension %>%
          stringi::stri_detect_fixed(
            pattern = y,
            opts_fixed = stringi::stri_opts_fixed(case_insensitive = TRUE)) %>%
          any() %>%
          return()
      })

  # Subset, ensure default & return
  extensions %>%
    names() %>%
    magrittr::extract(matching_table) %>%
    ifelse(identical(., character(0)), default, .) %>%
    return()
}
