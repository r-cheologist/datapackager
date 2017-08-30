assert_is_a_valid_package_root <- function(pkgr){
  pkgr %>%
    assertive.types::assert_is_a_string()

  pkgr_exists <- pkgr %>%
    file.exists()

  if(pkgr_exists){
    pkgr %>%
      assertive.files::assert_all_are_dirs()
  } else {
    pkgr %>%
      dirname() %>%
      assertive.files::assert_all_are_dirs()
  }
}
