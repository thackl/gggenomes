#' Introduce non-existing columns
#'
#' Works like [dplyr::mutate()] but without changing existing columns, but only
#' adding new ones. Useful to add possibly missing columns with default values.
#' @inheritParams dplyr::mutate
#' @export
#' @examples
#' # ensure columns "y" and "z" exist
#' tibble::tibble(x=1:3) %>%
#'  introduce(y="a", z=paste0(y, dplyr::row_number()))
#' # ensure columns "y" and "z" exist, but do not overwrite "y"
#' tibble::tibble(x=1:3, y=c("c", "d", "e")) %>%
#'  introduce(y="a", z=paste0(y, dplyr::row_number()))
introduce <- function(.data, ...){
  dots <- quos(...)
  # ignore .data columns
  dots <- dots[setdiff(names(dots), names(.data))]
  mutate(.data, !!!dots)
}

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
      warn(c("Required column(s) missing: ", str_glue("{missing_vars}")))
    else
      abort(c("Required column(s) missing: ", str_glue("{missing_vars}")))
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
#' @keywords internal
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
#' @keywords internal
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
