assert_is_a_valid_data_catalogue <- function(obj){
  obj %>%
    assertive.types::assert_is_list() %>%
    assertive.properties::assert_has_all_attributes(
      c("default_compression_algo", "default_hashing_algo"))

  if (length(obj) >= 1)
  {
    obj %<>%
      assertive.properties::assert_has_names() %>%
      lapply(
        function(x)
        {
          x %>%
            assertive.types::assert_is_list() %>%
            assertive.properties::assert_has_names() %>%
            names() %>%
            assertive.sets::assert_are_set_equal(
              c("Base.Name", "Data.Type", "Full.Name", "Hashing.Algo",
                "Compression.Algo", "Hash.Uncompressed", "Hash.Compressed",
                "Hash.Object", "Parsing.Function", "Parsing.Options",
                "Package.Dependencies", "Git.Ignore", "R.Buildignore"))
          x %>%
            return()
        })
    }
  obj %>%
    invisible()
}
