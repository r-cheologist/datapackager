#' @export
match_function_arg <- function(
  arg,
  fun,
  fun_arg_name,
  several.ok = FALSE,
  default = NULL)
{
# Check prerequisites -----------------------------------------------------
  fun %>%
    assertive.types::assert_is_function()

  fun_arg_name %>%
    assertive.types::assert_is_a_string()
  fun %>%
    methods::formalArgs() %>%
    assertive.sets::assert_is_superset(fun_arg_name)

  several.ok %>%
    assertive.types::assert_is_a_bool()

  arg %>%
    is.null() %>%
    c(default %>%
        is.null()) %>%
    assertive.base::assert_any_are_false()

# Processing --------------------------------------------------------------
  # Deal with defaults
  if (is.null(arg))
  {
    arg <- default
  }
  # Store the class - 'formals' will return type 'language' ...
  arg_class <- arg %>%
    class()
  # Check
  arg <- arg %>%
    match.arg(
      choices = fun %>%
        formals() %>%
        magrittr::extract2(fun_arg_name),
      several.ok = several.ok)
  # Cast to input-equivalent
  as(arg, arg_class) %>%
    return()
}
