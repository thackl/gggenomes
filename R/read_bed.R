#' Read a BED file
#'
#' @inheritParams readr::read_tsv
#' @param col_names column names to use. Defaults to [def_names("bed")]
#'   compatible with blast tabular output (`--outfmt 6/7` in blast++ and `-m8`
#'   in blast-legacy). [def_names("bed")] can easily be combined with extra
#'   columns: `col_names = c(def_names("bed"), "more", "things")`.
#'
#' @return
#' @export
read_bed <- function (file, col_names = def_names("bed"),
    col_types = def_types("bed"), ...){

    x <- read_tsv(file, comment = "#", col_names = FALSE, col_types = col_types, ...)
  # handle bed files with only some of the default columns (valid)
  i <- seq_len(min(ncol(x), length(col_names)))
  names(x)[i] <- col_names[i]
  x
}
