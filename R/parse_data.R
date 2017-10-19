#' @noRd
parse_data <- function(path, reading_function, reading_options){
  reading_function %>%
    do.call(
      c(path, reading_options) %>%
        as.list()) %>%
    return()
}
