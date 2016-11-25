build_path <- function(x, root = getwd()){
  x %>%
    sapply(
      function(y){
        file.path(root, y) %>%
          return()
      }) %>%
    return()
}
