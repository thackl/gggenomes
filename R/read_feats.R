#' @export
#' @describeIn read_tracks read files as features mapping onto
#'   sequences.
#' @examples
#' # read genes/features from a gff file
#' read_feats(ex("eden-utr.gff"))
#'
#'
#' # read all gff files from a directory
#' read_feats(list.files(ex("emales/"), "*.gff$", full.names=TRUE))
#'
#'
#' # read remote files
#' \dontrun{
#' gbk_phages <- c(
#'   PSSP7 = "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/858/745/GCF_000858745.1_ViralProj15134/GCF_000858745.1_ViralProj15134_genomic.gff.gz",
#'   PSSP3 = "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/904/555/GCF_000904555.1_ViralProj195517/GCF_000904555.1_ViralProj195517_genomic.gff.gz")
#' read_feats(gbk_phages)
#' }
#'
#'
read_feats <- function(files, .id="file_id", format=NULL, parser=NULL, ...){
  read_context(files, "feats", .id=.id, format=format, parser=parser, ...)
}

#' @export
#' @describeIn read_tracks read files as subfeatures mapping onto other features
read_subfeats <- function(files, .id="file_id", format=NULL, parser=NULL, ...){
  feats <- read_context(files, "feats", .id=.id, format=format, parser=parser, ...)
  rename(feats, feat_id=seq_id, feat_id2=seq_id2)
}

#' @export
#' @describeIn read_tracks read files as links connecting sequences
read_links <- function(files, .id="file_id", format=NULL, parser=NULL, ...){
  feats <- read_context(files, "links", .id=.id, format=format, parser=parser, ...)
  rename(feats, seq_id=seq_id, start=start, end=end)
}

#' @export
#' @describeIn read_tracks read files as sublinks connecting features
read_sublinks <- function(files, .id="file_id", format=NULL, parser=NULL, ...){
  feats <- read_context(files, "links", .id=.id, format=format, parser=parser, ...)
  rename(feats, feat_id=seq_id, start=start, end=end, feat_id2=seq_id2)
}

#' Swap query and subject in blast-like feature tables
#'
#' Swap query and subject columns in a table read with [read_feats()] or
#' [read_links()], for example, from blast searches. Swaps columns with
#' name/name2, such as 'seq_id/seq_id2', 'start/start2', ...
#'
#' @param x tibble with query and subject columns
#' @export
#' @return tibble with swapped query/subject columns
#' @examples
#' feats <- tibble::tribble(
#'  ~seq_id, ~seq_id2, ~start, ~end, ~strand, ~start2, ~end2, ~evalue,
#'  "A", "B", 100, 200, "+", 10000, 10200, 1e-5
#' )
#' # make B the query
#' swap_query(feats)
swap_query <- function(x){
  # for every pair seq_id/seq_id2, name/name2 > name2/name
  n <- names(x)
  m <- str_subset(n, "\\D2") %>% stringr::str_remove("2$") %>% intersect(n)
  if(!length(m))
    return(x)

  m2 <- paste0(m, "2")
  i <- which(n %in% m)
  i2 <- which(n %in% m2)
  inform(c("Swapping query/subject-associated columns",
           comma(m, collapse='  '), comma(m2, collapse=' ')))
  x[c(i, i2)] <- x[c(i2, i)]
  x
}
