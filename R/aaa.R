#' Swap values of two columns based on a condition
#'
#' @param x a tibble
#' @param condition an expression to be evaluated in data context returning a
#' TRUE/FALSE vector
#' @param ... the two columns bewteen which values are to be swapped in
#' dplyr::select-like syntax
#' @examples
#' x <- tibble(start = c(10,100), end=c(30, 50))
#' # ensure start of a range is always smaller than the end
#' gggenomes:::swap_if(x, start > end, start, end)
swap_if <- function(x, condition, ...){
  i <- tidyselect::eval_select(rlang::expr(c(...)), x)
  if(length(i) != 2 || length(unique(i)) != 2)
    rlang::abort("need to select exactly 2 different columns for swapping")
  # eval condition in data context
  j <- transmute(x, j = {{condition}})$j
  # swap
  x[j,rev(i)] <- x[j,i]
  x
}

#' Collapse a string with commas
#'
#' @param x vector (coerced to character)
#' @param collapse character string to separate elements.
#' @keywords internal
comma <- function(x, collapse = ","){
    paste(x, collapse=collapse)
}

#' Get path to gggenomes example files
#' @export
#' @param file name of example file
ex <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "gggenomes"))
  } else {
    system.file("extdata", file, package = "gggenomes", mustWork = TRUE)
  }
}

# are there any arguments in ...
has_dots <- function(env = parent.frame()){
  length(ellipsis:::dots(env)) > 0
}

shared_names <- function(x, ...){
  names <- c(...)
  names[names %in% names2(x)]
}

# split a vector of tuples c(a1,b2,a2,b2) into a list of two vectors
# list(a=c(a1,a2), b=c(b1,b2))
vec_unzip <- function(x, names=NULL, ignore_odd=FALSE){
  if(!ignore_odd && length(x)%%2){
    abort(str_glue("Won't unzip a vector of uneven length {(length(x))}\n",
                   "Disable error with `ignore_odd=TRUE`"))
  }

  i <- c(TRUE, FALSE)
  set_names(list(x[i], x[!i]), names)
}

#' @export
#' @importFrom magrittr %>%
magrittr::`%>%`

#' @export
#' @importFrom magrittr %<>%
magrittr::`%<>%`
