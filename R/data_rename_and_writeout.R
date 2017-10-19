#' @noRd
data_rename_and_writeout <- function(data_object, file_name, root, compression_algo){
  assign(
    file_name %>%
      basename(),
    data_object)
  save(
    list = file_name %>%
      basename(),
    file = make_data_path(root, file_name),
    compress = compression_algo)
}
