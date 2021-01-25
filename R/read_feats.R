#' Read features from common file formats
#'
#' Read features from GFF3, Genbank, BED, BLAST tabular output or PAF files.
#'
#' @return tibble

read_feats <- function(files, format=NULL, ...){
  # infer file format from suffix
  format <- (format %||% file_format_unique(files, "feats"))

  # read vector, name == bin_id, name missing - infer form filename
  files <- file_label(files)

  # map_df .id = bin_id
  inform(str_glue("Reading as {format}:"))
  feats <- map2_df(files, names(files), .id="bin_id", ..., .f=function(.x, .y, ...){
            inform(str_glue("* {.y} [{.x}]"))
            read_feats_fun(format)(.x, ...)})

  # if blast - infer mode: query or subject


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
