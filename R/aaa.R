#' Swap values of two columns based on a condition
#'
#' @param x a tibble
#' @param condition an expression to be evaluated in data context returning a
#' TRUE/FALSE vector
#' @param ... the two columns bewteen which values are to be swapped in
#' dplyr::select-like syntax
#' @examples
#' x <- tibble::tibble(start = c(10,100), end=c(30, 50))
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

#' Split by key preserving order
#'
#' Split by key column while preserving order according to the first
#' occurence. R base split converts keys to factors, changing default order to
#' alphanumeric.
#'
#' @param key variable to split by
#' @export
#' @examples
#' tibble(x=c(1,1,1,2), y=c("B", "A", "B", "B"), z="foo") %>%
#'   split_by(x)
split_by <- function(.data, key){
  keys <- pull(.data, !!enquo(key))

  if(length(keys) == 0) stop("no keys to index by found\n")

  l <- split(.data, keys)
  l[unique(keys)]
}

#' Create a vector from unquoted words.
#'
#' Similar to perls `qw()`, however, in R spaces between args in function call
#' always cause an error, so `qw(foo bar)` wouldn't work. Workaround is either a
#' single string split at spaces, or unquoted elements, separated by commas.
#'
#' Took inspiration from
#' \href{https://stackoverflow.com/questions/520810/does-r-have-quote-like-operators-like-perls-qw}{stackoverflow/qw}
#' and \href{https://github.com/jebyrnes/multifunc/blob/master/R/qw.R}{github/Jarrett Byrnes}
#' 
#' @param x A single string of elements to be split at whitespace chars.
#' @return A vector of quoted words.
#' @export
#' @examples
#' qw("foo bar") # with a strsplit
#' qc(foo, bar) # or unquoted, but with commas
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

#' @rdname qw
#' @param ... Unquated words, separated by comma.
#' @export
qc <- function(...) sapply(match.call()[-1], deparse)
