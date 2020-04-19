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
          select(-bin_id)
  }else{
    # coerce IDs to chars, so we don't get errors in join by mismatched types
    subseqs <- mutate_at(subseqs, vars(seq_id), as.character)
  }

  s <- inner_join(s, subseqs, by="seq_id")
  s <- mutate(s,
    start = ifelse(start < 1, 1, start),
    end = ifelse(length < end, length, end))

  if(any(duplicated(s$seq_id))){
    # NOTE: currently, this would create a clone of the sequence - duplicating
    # sequence and feature_ids in the plot. This breaks access by ids functions
    # (pick, ...) and feature_id-based geom_gene - thinks genes on clones are
    # exons. Not sure, how to best fix that
    warn(paste("focussing in on two or more regions of the same sequence is",
               "currently not supported. Will return only the first region"))
    s <- s[!duplicated(s$seq_id),]
  }

  seqs(x) <- s
  layout(x, args_features = list(marginal = marginal))
}
