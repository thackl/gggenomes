#' Read features and links from common file formats
#'
#' Read features or links from common formats, such as GFF3, Genbank, BED, BLAST
#' tabular output or PAF files. File formats and the format-specific `read_*()`
#' function are automatically determined based in file extensions, if possible.
#' Can read multiple files in the same format into a single table: useful, for
#' example, to read a folder of gff-files with each containing genes of a
#' different genome.
#'
#' @param files files to reads. Should all be of same format.
#' @param format If NULL, will be guessed from file extension. Else, any format
#'   known to gggenomes ("gff3", "gbk", ... see [file_formats()] for full list)
#'   or any suffix that maps to a known `read_<suffix>` function to be called,
#'   such as [readr::read_tsv()], for example.
#' @param .id When binding output from several files, how to name the column
#'   with the name of the file each record came from. Defaults to "file_id". Set
#'   to "bin_id" if every file represents a different bin.
#' @param ... additional arguments passed on to the specific read function for
#'   the given format.
#'
#' @return a gggenomes compatible feature or link tibble
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
read_feats <- function(files, format=NULL, .id="file_id", ...){
  if(is_connection(files))
    files <- list(files) # weird things happen to pipes in vectors

  # infer file format from suffix
  format <- (format %||% file_format_unique(files, "feats"))

  if(format == 'ambigious'){
    abort(str_glue('Ambigious file extension(s): "', comma(unique(file_ext(files))),
                   '".\nPlease specify `format` explicitly'))
  }

  # for unnamed files, infer name from filename (used as file_id/bin_id)
  files <- file_label(files)

  # map_df .id = bin_id
  inform(str_glue("Reading as {format}:"))
  feats <- map2_df(files, names(files), read_feat_impl, .id=.id, format, ...)

  feats
}

#' @rdname read_feats
#' @export
read_subfeats <- function(files, format=NULL, .id="file_id", ...){
  feats <- read_feats(files=files, format=format, ...)
  rename(feats, feat_id=seq_id, feat_id2=seq_id2)
}

#' @rdname read_feats
#' @export
read_links <- function(files, format=NULL, .id="file_id", ...){
  feats <- read_feats(files=files, format=format, ...)
  rename(feats, seq_id=seq_id, start=start, end=end)
}

#' @rdname read_feats
#' @export
read_sublinks <- function(files, format=NULL, .id="file_id", ...){
  feats <- read_feats(files=files, format=format, ...)
  rename(feats, feat_id=seq_id, start=start, end=end, feat_id2=seq_id2)
}

read_feat_impl <- function(file, name, format, ...){
  inform(str_glue("* {name} [{file}]"))
  exec(paste0("read_", format), file, ...)
}
