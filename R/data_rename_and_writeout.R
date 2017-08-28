data_rename_and_writeout <- function(data_object, file_name, root, compression_algo){
  assign(
    file_name %>%
      basename(),
    data_object)
  save(
    list = file_name %>%
      basename(),
    file = root %>%
      file.path(
        "data",
        file_name %>%
          basename %>%
          paste0(".rda")),
    compress = compression_algo)
}
