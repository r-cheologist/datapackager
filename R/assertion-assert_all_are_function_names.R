#' @noRd
assert_all_are_function_names <- function(fn){
  fn %>%
    assert_is_character() %>%
    sapply(
      function(x){
        parse(text = x) %>%
          eval() %>%
          assert_is_function()
      })
  fn %>%
    invisible()
}
