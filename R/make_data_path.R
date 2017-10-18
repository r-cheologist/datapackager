#' @noRd
make_data_path <- function(root, basename)
{
  root %>%
    file.path(
      "data",
      basename %>%
        basename() %>%
        paste0(".rda")) %>%
    # To deal with empty root:
    stringi::stri_trim_both(
      pattern = paste0("[^", .Platform$file.sep, "]")) %>%
    return()
}
