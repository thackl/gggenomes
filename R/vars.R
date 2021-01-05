#' Require variables in an object
#'
#' @param x object
#' @param vars required variables
#' @param warn_only don't die on missing vars
#' @export
require_vars <- function(x, vars, warn_only=FALSE){
  missing_vars <- vars[!(vars %in% names(x))]

  if(length(missing_vars) > 0)
    if(warn_only)
      warning("Required column(s) missing: ", paste(missing_vars, collapse=","))
    else
      stop("Required column(s) missing: ", paste(missing_vars, collapse=","))
  x
}

#' Check if variables exist in object
#'
#' Returns TRUE if all variables exists. If `any=TRUE` returns TRUE if at least
#' one variable exists.
#'
#' @param x object
#' @param vars variables to test
#' @param any if TRUE not all but at least one variable has to exists
#' @return TRUE/FALSE
#' @export
has_vars <- function(x, vars, any=FALSE){
  if(any)
    any(vars %in% names(x))
  else
    all(vars %in% names(x))
}

#' match.arg defaulting to all choices
#'
#' equivalent to base::match.arg, but returns all choices on arg=NULL if
#' several.ok=TRUE
match_arg <- function (arg, choices, several.ok = FALSE){
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }
  if (is.null(arg))
    if(several.ok)
      return(choices)
    else
      return(choices[1L])
  else if (!is.character(arg))
    stop("'arg' must be NULL or a character vector")
  if (!several.ok) {
    if (identical(arg, choices))
      return(arg[1L])
    if (length(arg) > 1L)
      stop("'arg' must be of length 1")
  }
  else if (length(arg) == 0L)
    stop("'arg' must be of length >= 1")
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L))
    stop(gettextf("'arg' should be one of %s", paste(dQuote(choices),
                                                     collapse = ", ")), domain = NA)
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1)
    stop("there is more than one match in 'match.arg'")
  choices[i]
}
