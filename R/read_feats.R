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
read_feats <- function(files, format=NULL, bin_formats=c("gff3", "gbk"), ...){
  # infer file format from suffix
  format <- (format %||% file_format_unique(files, "feats"))

  # read vector, name == bin_id, name missing - infer form filename
  files <- file_label(files)

  # map_df .id = bin_id
  inform(str_glue("Reading as {format}:"))
  feats <- map2_df(files, names(files), .id="bin_id", ..., .f=function(.x, .y, ...){
            inform(str_glue("* {.y} [{.x}]"))
            read_feats_fun(format)(.x, ...)})

  if(!format %in% bin_formats)
    feats <- select(feats, -bin_id)
  # if blast - infer mode: query or subject

  feats
}

read_feats_fun <- function(format){
  f <- list(
    gff3 = function(...){read_gff3(...)},
    gbk = function(...){read_gbk(...)},
    bed = function(...){read_bed(...)},
    blast = function(...){read_blast(...)}
  )[[format]]
  if(is.null(f))
    abort(str_glue("Unknown format {format}"))
  f
}
