#' @noRd
make_data_path <- function(root, basename)
{
  output <- root %>%
    file.path(
      "data",
      basename %>%
        basename() %>%
        paste0(".rda"))

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
