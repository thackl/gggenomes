#' Pick seqs and bins by name or position
#'
#' Pick which sequences and bins to show and in what order. Uses
#' dyplr::select-like syntax, which means unquoted genome names, positional
#' arguments and select helper functions, such as `starts_with()` are
#' supported. Renaming is not supported because it would break associations with
#' other tracks.
#'
#' Use the dots to select sequences, and the bins argument to set the scope for
#' positional arguments. For example, `pick(1)` will pick the first sequence
#' from the first bin, while `pick(1, bins=3)` will pick the first sequence from
#' the third bin.
#'
#' `pick_at` keeps bins that are not picked from, as they are.
#'
#' `pick_bins` works on entire bins.
#'
#' @param ... seqs to pick, select-like expression
#' @param bins to pick from, expression, enclose multiple args in c()
#' @export
pick <- function(x, ..., bins=everything()){
  if(!has_dots()) return(x)
  pick_impl(x, ..., bins = {{ bins }}, .keep=FALSE)
}

#' @rdname pick
#' @export
pick_at <- function(x, ..., bins=everything()){
  if(!has_dots()) return(x)
  pick_impl(x, ..., bins = {{ bins }}, .keep=TRUE)
}

#' @rdname pick
#' @export
pick_bins <- function(x, ...){
  if(!has_dots()) return(x)
  pick_impl(x, bins=c(...))
}

pick_impl <- function(x, ..., bins=everything(), .keep=FALSE){
  # split by bin_id and select bins
  s <- seqs(x)
  l <- s %>% thacklr::split_by(bin_id)
  i <- tidyselect::eval_select(expr({{ bins }}), l)
  if(length(i) == 0) rlang::abort("no bins selected")
  s <- bind_rows(l[i])

  # pick seqs from bins
  if(has_dots()){
    seq_ids <- s$seq_id %>% set_names(.)
    j <- tidyselect::eval_select(expr(c(...)), seq_ids)
    s <- s[j,]
    if(isTRUE(.keep)){ # splice modified bins into rest
      m <- s %>% thacklr::split_by(bin_id)
      l[names(m)] <- m
      s <- bind_rows(l)
    }
  }

  seqs(x) <- s
  layout(x)
}
