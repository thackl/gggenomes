#' Focus on features and regions of interest
#'
#' Only show subsequences from the complete data set containing features of
#' interest.
#'
#' @param x a gggenomes object
#' @param ... Logical predicates defined in terms of the variables in the track
#' given by 'track_id'. Multiple conditions are combined with ‘&’. Only rows
#' where the condition evaluates to ‘TRUE’ are kept.
#'
#' The arguments in ‘...’ are automatically quoted and evaluated in the context
#' of the data frame. They support unquoting and splicing. See
#' ‘vignette("programming")’ for an introduction to these concepts.
#' @param track_id the track to filter from
#' @param plus the amount to nucleotides to extend the focus around the target
#' features. Give two values for different up- and downstream extensions.
#' @inheritParams layout_features
#' @param subseqs a dataframe specifying sequence regions (subseqs) of interest
#' directly. Required columns are 'seq_id, start, end'. Superceeds `...`.
#' @export
focus <- function(x, ..., track_id=genes, plus=2000, marginal=c("trim", "drop",
    "keep"), subseqs=NULL){
  if(length(plus==1)) plus <- c(plus,plus)
  marginal <- match.arg(marginal)

  s <- seqs(x)
  if(has_name(s, "start")) s <- select(s, -start)
  if(has_name(s, "end")) s <- select(s, -end)

  if(is.null(subseqs)){
    subseqs <- filter(track(x, {{track_id}}), ...) %>%
      group_by(seq_id, bin_id) %>%
      summarize(
        start = min(start) - plus[1],
        end = max(end) + plus[2]) %>%
      left_join(select(seqs(x) , seq_id, bin_id, .seq_length = length),
                    by=c("seq_id", "bin_id")) %>%
      mutate( # make sure we don't expand outside the seq
        start = if_else(start<0,0,start),
        end = if_else(.seq_length < end, .seq_length, end),
        .seq_length = NULL,
        bin_id = NULL # for cleaner join
      )
  }

  s <- inner_join(s, subseqs, by="seq_id")
  seqs(x) <- s
  layout(x, args_features = list(marginal = marginal))
}
