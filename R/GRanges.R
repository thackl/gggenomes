#' @export
as_tibble.GRanges <- function(data){
  select(as_tibble(as.data.frame(data)), seq_id=seqnames, start, end, strand, everything())
}
