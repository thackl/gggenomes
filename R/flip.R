#' Flip sequences or entire bins
#'
#' @export
flip <- function(x, ..., bins=everything()){
  if(!has_dots()) return(x)
  flip_impl(x, ..., bins = {{ bins }})
}
#' @rdname flip
#' @export
flip_bins <- function(x, ...){
  if(!has_dots()) return(x)
  flip_impl(x, bins=c(...))
}

flip_impl <- function(x, ..., bins=everything()){
  # split by bin_id and select bins
  s <- seqs(x)
  l <- s %>% thacklr::split_by(bin_id)
  i <- tidyselect::eval_select(expr({{ bins }}), l)
  if(length(i) == 0) rlang::abort("no bins selected")
  s <- bind_rows(l[i])


  # flip seqs in bins
  if(length(ellipsis:::dots()) > 0){
    seq_ids <- s$seq_id %>% set_names(.)
    j <- tidyselect::eval_select(expr(c(...)), seq_ids)
    s$strand[j] <- flip_strand(s$strand[j])
  # flip entire bins
  }else{
    s %<>% mutate(strand = flip_strand(strand)) %>%
      arrange(-row_number())
  }

  # splice modified bins back into rest
  m <- s %>% thacklr::split_by(bin_id)
  l[names(m)] <- m
  s <- bind_rows(l)

  seqs(x) <- s
  layout(x)
}
