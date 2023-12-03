#' @title match_function_arg
#' @aliases match_function_arg
#' @description An extension of \code{\link{match.arg}} to check an argument
#' against an argument of a given function.
#' @param arg An \code{R} object to be checked.
#' @param fun A \code{\link{function}} from which the argument to be checked
#' against is derived.
#' @param fun_arg_name A single \code{\link{character}} object determining which
#' argument of \code{fun} is used to check \code{arg} against.
#' @param several.ok Single \code{\link{logical}} specifying if arg should be
#' allowed to have more than one element (see \code{\link{match.arg}}).
#' @param default A default value for \code{arg} should it be
#' \code{\link{NULL}}.
#' @seealso \code{link{match.arg}}, \code{\link{formals}}
#' @examples
#' # Have a look at the function/argument we are going to check against:
#' formals(read.table)
#' formals(read.table)[["numerals"]]
#'
#' # This works
#' match_function_arg(
#'   "allow.loss",
#'    fun = read.table,
#'    fun_arg_name = "numerals")
#'
#' # As thus this ...
#' match_function_arg(
#'   c("allow.loss", "no.loss"),
#'   fun = read.table,
#'   fun_arg_name = "numerals",
#'   several.ok = TRUE)
#'
#' \dontrun{
#' # This doesn't
#' match_function_arg(
#'   "allow.floss",
#'   fun = read.table,
#'   fun_arg_name = "numerals")
#' }
#'
#' # Here we give a default (for programming use)
#' match_function_arg(
#'   NULL,
#'   fun = read.table,
#'   fun_arg_name = "numerals",
#'   default = "warn.loss")
#' @export
match_function_arg <- function(
  arg,
  fun,
  fun_arg_name,
  several.ok = FALSE,
  default = NULL)
{
# Check prerequisites -----------------------------------------------------
  fun %>% assert_is_function()

  fun_arg_name %>% assert_is_a_string()
  fun %>%
    methods::formalArgs() %>%
    assertive.sets::assert_is_superset(fun_arg_name)

  several.ok %>% assert_is_a_bool()

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
  methods::as(arg, arg_class) %>%
    return()
}
