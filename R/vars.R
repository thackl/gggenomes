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
