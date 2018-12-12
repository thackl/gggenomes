#' @export
as_tibble.GRanges <- function(data){
  select(as_tibble(as.data.frame(data)), contig_id=seqnames, start, end, strand, everything())
}
