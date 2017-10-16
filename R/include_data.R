#' @export
include_data <- function(
  object_to_include,
  type                 = c("auto", "object", "file", "file_rda", "file_rds", "url", "url_rda", "url_rds"),
  root                 = ".",
  data_catalogue       = NULL,
  saved_elements       = c("all", "data", "extdata"),
  distributable        = TRUE,
  gitignore            = !distributable,
  rbuildignore         = !distributable,
  package_dependencies = NULL,
  parsing_function     = NULL,
  parsing_options      = NULL,
  user                 = NULL,
  password             = NULL,
  save_catalogue       = TRUE,
  compression_algo     = NULL,
  hashing_algo         = NULL
)
{
# Check prerequisites -----------------------------------------------------
  # Only accepting string
  object_to_include %>%
    assertive.types::assert_is_a_string()

  # Deal with input type
  type %<>%
    match.arg(
      choices = type,
      several.ok = FALSE)
  ## Automated type determination
  if ( stringi::stri_detect_fixed(type, "auto"))
  {
    if (exists(object_to_include)) { type <- "object" }
    else {
      prefix <- ifelse(
        object_to_include %>%
          is_url(),
        "url",
        "file")
      ending <- object_to_include %>%
        analyse_extension()
      type <- ending %>%
        ifelse(identical(., ""), prefix, paste(prefix, ending, sep = "_"))
    }
  }

  # Check package root
  root %>%
    assert_is_a_valid_package_root
  if (is.null(data_catalogue))
  {
    data_catalogue <- root %>%
      file.path("data", "data_catalogue.rda") %>%
      load_data_file_as_object()
  }
  data_catalogue %>%
    assert_is_a_valid_data_catalogue()

  # Deal with which elements to save
  saved_elements %<>%
    match.arg(
      choices = saved_elements,
      several.ok = TRUE)

  # Deal with variables related to sharing/distribution
  if (!is.null(distributable))
  {
    distributable %>%
      assertive.types::assert_is_a_bool()
  }
  if (!is.null(gitignore))
  {
    gitignore %>%
      assertive.types::assert_is_a_bool()
  }
  if (!is.null(rbuildignore))
  {
    rbuildignore %>%
      assertive.types::assert_is_a_bool()
  }

  # Check package dependencies needed for reading/parsing
  if (!is.null(package_dependencies))
  {
    package_dependencies %>%
      assertive.types::assert_is_character() %>%
      assertive.sets::assert_is_subset(
        utils::installed.packages())
  }

  # Check needs for data parsing
  if (!is.null(parsing_function))
  {
    parsing_function %>%
    assertive.types::assert_is_a_string() %>%
    assert_all_are_function_names()
  }
  if (!is.null(parsing_options))
  {
    parsing_options %>%
      assertive.types::assert_is_list()
  }

  # Check needs for authenticated retrieval of remate data
  if (!is.null(user))
  {
    user %>%
      assertive.types::assert_is_a_string()
  }
  if (!is.null(password))
  {
    password %>%
      assertive.types::assert_is_a_string()
  }
  c(user, password) %>%
    length() %>%
    assertive.sets::assert_is_subset(c(0,2))

  # Deal with algorithm selection
  if (!is.null(compression_algo))
  {
    compression_algo <- attr(
      data_catalogue,
      "default_compression_algo")
  }
  else {
    compression_algo %<>%
      assertive.types::assert_is_character() %>%
      match.arg(
        choices = c("xz", "bzip2", "gzip"),
        several.ok = FALSE)
  }

  hashing_algo %<>%
    match_function_arg(
      fun = digest::digest,
      fun_arg_name = "algo",
      several.ok = FALSE,
      default = attr(
        data_catalogue,
        "default_hashing_algo"))

  # Indicating whether to only return the modified catalogue or also save it ...
  save_catalogue %>%
    assertive.types::assert_is_a_bool()

# Processing --------------------------------------------------------------


}
