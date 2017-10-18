#' @noRd
make_extdata_path <- function(root, basename)
{
  root %>%
    file.path(
      "inst",
      "extdata",
      basename %>%
        basename() %>%
        paste0(".zip")) %>%
    # To deal with empty root:
    stringi::stri_trim_both(
      pattern = paste0("[^", .Platform$file.sep, "]")) %>%
    return()
}
