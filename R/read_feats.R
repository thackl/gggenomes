#' Read features from common file formats
#'
#' Read features from GFF3, Genbank, BED, BLAST tabular output or PAF files.
#'
#' @return tibble
#' @examples
#' # read a file
#' read_feats("data-raw/eden-utr.gff")
#' # read all gffs from a directory
#' read_feats(list.files("some/where", "*.gff$", full.names=TRUE))
#' # read remote files
#' gbk_phages <- c(
#' PSSP7 = "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/858/745/GCF_000858745.1_ViralProj15134/GCF_000858745.1_ViralProj15134_genomic.gff.gz",
#' PSSP3 = "ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/904/555/GCF_000904555.1_ViralProj195517/GCF_000904555.1_ViralProj195517_genomic.gff.gz")
#' read_feats(gbk_phages)
read_feats <- function(files, format=NULL, file_id="bin_id", bin_formats=c("gff3", "gbk"), ...){
  # infer file format from suffix
  format <- (format %||% file_format_unique(files, "feats"))

  # for unnamed files, infer name from filename (used as file_id/bin_id)
  files <- file_label(files)

  # map_df .id = bin_id
  inform(str_glue("Reading as {format}:"))
  feats <- map2_df(files, names(files), read_feat_impl, .id=file_id, format, ...)

  #if(!format %in% bin_formats)
  #  feats <- select(feats, -bin_id)
  # if blast - infer mode: query or subject

  feats
}

read_feat_impl <- function(file, name, format, ...){
  inform(str_glue("* {name} [{file}]"))
  exec(paste0("read_", format), file, ...)
}

read_subfeats <- function(files, format=NULL, ...){
  feats <- read_feats(files=files, format=format)
  rename(feats, feat_id=seq_id, feat_id2=seq_id2)
}

read_links <- function(files, format=NULL, ...){
  feats <- read_feats(files=files, format=format)
  rename(feats, seq_id=seq_id, start=start, end=end)
}

read_sublinks <- function(files, format=NULL, ...){
  feats <- read_feats(files=files, format=format)
  rename(feats, feat_id=seq_id, start=start, end=end, feat_id2=seq_id2)
}
