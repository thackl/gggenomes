#' Do numeric values fall into specified ranges?
#'
#' @param x a numeric vector of values
#' @param left,right boundary values or vectors of same length as x
#' @param closed wether to include (`TRUE`) or exclude (`FALSE`) the
#' endpoints. Provide 2 values for different behaviors for lower and upper
#' boundary, e.g. `c(TRUE, FALSE)` to include only the lower boundary.
#' @return a logical vector of the same length as the input
#' @export
#' @examples
#' in_range(1:5, 2, 4)
#' in_range(1:5, 2, 4, closed=c(FALSE, TRUE)) # left-open
#' in_range(1:5, 6:2, 3) # vector of boundaries, single values recycle
#'
#'
#' # plays nicely with dplyr
#' df <- tibble::tibble(x=rep(4,5), left=1:5, right=3:7)
#' mutate(df,
#'   closed=in_range(x, left, right, TRUE),
#'   open=in_range(x, left, right, FALSE))
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

#' The width of a range
#'
#' Always returns a positive value, even if start > end. `width0` is a short
#' handle for `width(..., base=0)`
#'
#' @param start,end start and end of the range
#' @param base the base of the coordinate system, usually 1 or 0.
#' @return a numeric vector
width <- function(start, end, base=1){
  abs(end-start)+base
}

#' @rdname width
width0 <- function(start, end, base=0){
  width(start=start, end=end, base=base)
}

max_width <- function(..., base=1){
  diff(range(...))+base

}

#' @export
as_tibble.GRanges <- function(data){
  select(as_tibble(as.data.frame(data)), seq_id=seqnames, start, end, strand, everything())
}
