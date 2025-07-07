#' Read genbank files
#'
#' Genbank flat files (.gb/.gbk/.gbff) and their ENA and DDBJ equivalents have a
#' particularly gruesome format. That's why [read_gbk()] is just a wrapper
#' around a Perl-based `gb2gff` converter and [read_gff3()].
#'
#' @importFrom readr read_tsv
#' @inheritParams read_gff3
#' @export
#' @return tibble
read_gbk <- function(file, sources = NULL, types = NULL, infer_cds_parents = TRUE) {
  gb2gff <- base::system.file("exec/gb2gff", package = "gggenomes")

  if (file_is_zip(file) && file_ext(file, ignore_zip = FALSE) != "gz") {
    abort(str_glue("Decompressing for genbank only works with gzipped files, not `{suf}`"))
  }

  if (file_is_url(file) && file_is_zip(file)) {
    file <- pipe(str_glue("curl {file} | gzip -cd | {gb2gff} -S"))
  } else if (file_is_url(file)) {
    file <- pipe(str_glue("curl {file} | {gb2gff} -S"))
  } else if (file_is_zip(file)) {
    suf <- file_ext(file, ignore_zip = FALSE)
    if (suf != "gz") {
      abort(str_glue("Decompressing for genbank only works with gzipped files, not `{suf}`"))
    }
    file <- pipe(str_glue("gzip -cd {file} | {gb2gff} -S"))
  } else {
    file <- pipe(str_glue("{gb2gff} -S {file}"))
  }

  read_gff3(file,
    sources = sources, types = types,
    infer_cds_parents = infer_cds_parents
  )
}
