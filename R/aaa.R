#' Swap values of two columns based on a condition
#'
#' @param x a tibble
#' @param condition an expression to be evaluated in data context returning a
#' TRUE/FALSE vector
#' @param ... the two columns bewteen which values are to be swapped in
#' dplyr::select-like syntax
#' @examples
#' x <- tibble(start = c(10,100), end=c(30, 50))
#' swap_if(start > end, start, end) # ensure start of a range is always smaller than the end
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

# are there any arguments in ...
has_dots <- function(env = parent.frame()){
  length(ellipsis:::dots(env)) > 0
}
