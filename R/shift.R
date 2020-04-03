#' Shift bins left/right
#'
#' Shift bins along the x-axis, i.e. left or right in the default plot
#' layout. This is useful to align features of interest in different bins.
#'
#' @param bins to shift left/right, select-like expression
#' @param by shift each bin by this many bases. Single value or vector of the
#' same length as bins.
#' @export
shift_bin <- function(x, bins, by){
  # split by bin_id and select bins
  s <- seqs(x)
  l <- s %>% thacklr::split_by(bin_id)
  i <- tidyselect::eval_select(expr({{ bins }}), l)
  if(length(i) == 0) rlang::abort("no bins selected")
  if(length(by) > 1 & length(by) != length(i)){
    rlang::abort(paste("by needs to be a single value or a vector of the same length as bins:", length(i)))
  }else if(length(by)==1){
    by <- rep(by, length(i))
  }

  for(j in i)
    l[[j]]$bin_offset <- l[[j]]$bin_offset + by[j]

  seqs(x) <- bind_rows(l)
  layout(x, seqs_keep = c("strand", "bin_offset"))
}
