#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom methods formalArgs
has_arg <- function(x, fn = sys.function(sys.parent()))
{
  x <- get_name_in_parent(x)
  has_arg_(x, fn)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
has_arg_ <- function(x, fn = sys.function(sys.parent()))
{
  formal_args_of_fn <- formalArgs(fn)
  if(!x %in% formal_args_of_fn)
  {
    fn_name <- get_name_in_parent(fn)
    fail <- false(
      gettext("%s is not an argument of %s."),
      sQuote(x),
      sQuote(fn_name)
    )
    if("..." %in% formal_args_of_fn)
    {
      dots_call <- eval(quote(substitute(list(...))), sys.parent())
      if(!x %in% names(dots_call))
      {
        return(fail)
      }
    } else
    {
      return(fail)
    }
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom utils find
is_binding_locked <- function(x, env = if(is_scalar(e <- find(.xname))) as.environment(e) else parent.frame(), .xname = get_name_in_parent(x))
{
  assert_is_environment(env)
  .xname <- force(.xname)
  env <- force(env)
  if(!exists(.xname, env, inherits = FALSE))
  {
    return(
      false(
        gettext("%s does not exist in %s."),
        .xname,
        format(env)
      )
    )
  }
  if(!bindingIsLocked(.xname, env))
  {
    return(
      false(
        gettext("%s is not locked (read-only) in %s."),
        .xname,
        format(env)
      )
    )
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base use_first
is_debugged <- function(x, .xname = get_name_in_parent(x))
{
  # isdebugged accepts x as either a function or a string
  if(!is.function(x))
  {
    x <- coerce_to(use_first(x), "character", .xname)
  }
  if(!isdebugged(x))
  {
    return(
      false(
        gettext("%s is not being debugged."),
        .xname
      )
    )
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
is_error_free <- function(x)
{
  res <- try(x, silent = TRUE)
  if(inherits(res, "try-error"))
  {
    return(false(attr(res, "condition")$message))
  }
  ok <- TRUE
  attr(ok, "result") <- res
  ok
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base bapply
is_existing <- function(
  x,
  envir = parent.frame(),
  inherits = TRUE,
  .xname = get_name_in_parent(x)
)
{
  x <- coerce_to(x, "character", .xname)
  if(is_empty(x)) return(logical(0))
  if(length(x) > 1L)
  {
    return(bapply(
      x,
      is_existing,
      envir    = envir,
      inherits = inherits
    ))
  }
  if(!exists(
    x,
    envir    = envir,
    inherits = inherits
  ))
  {
    return(false(gettext("%s does not exist."), .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is_na
is_if_condition <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_logical(x, .xname)))
  {
    return(ok)
  }
  if(!(ok <- is_scalar(x, "length", .xname)))
  {
    return(ok)
  }
  if(is_na(x))
  {
    return(false("%s is NA.", .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
is_loaded <- function(x, PACKAGE = "", type = c("", "C", "Fortran", "Call", "External"),
  .xname = get_name_in_parent(x))
{
  type <- match.arg(type)
  if(nzchar(PACKAGE))
  {
    if(is.null(getLoadedDLLs()[[PACKAGE]]))
    {
      return(false(gettext("The DLL %s is not loaded."), PACKAGE))
    }
    routines <- getDLLRegisteredRoutines(PACKAGE)
    type <- if(type == "")
    {
      c(".C", ".Fortran", ".Call", ".External")
    } else
    {
      paste0(".", type)
    }
    routine_names <- unlist(lapply(type, function(x) names(routines[[x]])))
    if(!x %in% routine_names)
    {
      return(
        false(
          gettext("The routine %s is not registered with the DLL %s."),
          .xname,
          PACKAGE
        )
      )
    }
  }
  if(!is.loaded(x, PACKAGE = PACKAGE, type = type))
  {
    return(false(gettext("The symbol %s is not loaded."), .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base use_first
is_valid_r_code <- function(x, .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character", .xname)
  x <- use_first(x)
  ok <- is_error_free(parse(text = x))
  if(!ok)
  {
    return(false(
      gettext("%s is not valid R code. %s."),
      .xname,
      cause(ok)
    ))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base set_cause
is_valid_variable_name <- function(x, allow_reserved = TRUE,
  allow_duplicates)
{
  if(!missing(allow_duplicates))
  {
    .Deprecated(
      msg = "The 'allow_duplicates' argument is deprecated and will be ignored."
    )
  }
  x <- coerce_to(x, "character", get_name_in_parent(x))

  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok <- short_enough <- nchar(x) <= max_name_length

  not_missing_and_ok <- !is.na(ok) & ok

  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  not_reserved <- rep.int(TRUE, length(x))
  if(!allow_reserved)
  {
    rx <- "^\\.{2}[[:digit:]]+$"
    ok[not_missing_and_ok] <- not_reserved[not_missing_and_ok] <-
      x[not_missing_and_ok] != "..." & !grepl(rx, x[not_missing_and_ok])
  }

  #are names valid (and maybe unique)
  not_missing_and_ok <- !is.na(ok) & ok

  ok[not_missing_and_ok] <- x[not_missing_and_ok] ==
    make.names(x[not_missing_and_ok])

  names(ok) <- x
  set_cause(
    ok,
    ifelse(
      short_enough,
      ifelse(not_reserved, "bad format", "reserved"),
      "too long"
    )
  )
}

