#' @noRd
make_extdata_path <- function(root, basename)
{
  output <- root %>%
    file.path(
      "inst",
      "extdata",
      basename %>%
        basename() %>%
        paste0(".zip"))

  # Deal with empty root:
  if (identical(root, ""))
  {
    output %<>%
      stringi::stri_trim_both(
        pattern = paste0("[^", .Platform$file.sep, "]"))
  }

  output %>%
    return()
}
