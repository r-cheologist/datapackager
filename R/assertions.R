assert_all_are_function_names <- function(fn){
  fn %>%
    assertive.types::assert_is_character() %>%
  sapply(
    function(x){
      parse(text = x) %>%
        eval() %>%
        assertive.types::assert_is_function()
    })
  fn %>%
    invisible()
}
