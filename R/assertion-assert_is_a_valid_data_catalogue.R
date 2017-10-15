assert_is_a_valid_data_catalogue <- function(obj){
  obj %>%
    assertive.types::assert_is_list() %>%
    assertive.properties::assert_has_all_attributes(
      c("default_compression_algo", "default_hashing_algo")) %>%
    invisible()
}
