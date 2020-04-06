null_else <- function(a, b) {
  if (is.null(a)) NULL else b
}

has_dots <- function(env = parent.frame()){
  length(ellipsis:::dots(env)) > 0
}

#' Do numeric values fall into specified ranges?
#' 
#' @param x a numeric vector of values
#' @param left,right boundary values or vectors of same length as x
#' @param closed wether to include (`TRUE`) or exclude (`FALSE`) the
#' endpoints. Provide 2 values for different behaviors for lower and upper
#' boundary, e.g. `c(TRUE, FALSE)` to include only the lower boundary.
#' @return a logical vector of the same length as the input
#' @examples
#' in_range(1:5, 2, 4)
#' in_range(1:5, 2, 4, closed=c(FALSE, TRUE)) # left-open
#' in_range(1:5, 6:2, 3) # vector of boundaries, single values recycle
#'
#' # plays nicely with dplyr
#' mutate(
#'   tibble(x=rep(4,5), left=1:5, right=3:7),
#'   closed=in_range(x, left, right, TRUE),
#'   open=in_range(x, left, right, FALSE)
in_range <- function(x, left, right, closed = TRUE){
  if(length(closed) == 1) closed <- rep(closed, 2)
  n <- length(x)
  if(length(left) == 1) left <- rep(left, n)
  if(length(right) == 1) right <- rep(right, n)

  if(length(right) != n || length(left) != n)
    rlang::abort("left and right need to of length 1 or same length as x")

  # left and right are not necessarily sorted
  swap <- left > right
  if(any(swap)){
    left_tmp <- left
    left[swap] <- right[swap]
    right[swap] <- left_tmp[swap]
  }

  in_range_impl(x, min=left, max=right, closed=closed)
}

in_range_impl <- function(x, min, max, closed = c(TRUE, TRUE)){
  if(!any(closed))      min <  x & x <  max
  else if(all(closed))  min <= x & x <= max
  else if(closed[1])    min <= x & x <  max
  else if(closed[2])    min <  x & x <= max
}
