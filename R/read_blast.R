#' Read BLAST tab-separated output
#'
#' @importFrom readr read_tsv
#' @inheritParams readr::read_tsv
#' @param col_names column names to use. "o6" is a special and expands to the 12
#'   columns present in default blast tabular output (`--outfmt 6/7` in blast++
#'   and `-m8` in blast-legacy). It is also expanded when used with other
#'   columns, so that you can easily add extra columns to the default names,
#'   i.e. `col_names = c("o6", "more", "things")`.
#' @param swap_query if TRUE swap query and subject columns using [swap_query()]
#'   on import.
read_blast <- function (file, col_names = "o6", comment = "# ", swap_query=FALSE,
    ...){
  o6 <- qc(seq_id, seq_id2, pident, length, mismatch, gapopen,
               start, end, start2, end2, evalue, bitscore)
  # insert std at position
  i <- which("o6" == col_names)
  j <- seq_along(col_names)
  if(length(i) == 1){
    col_names <- c(col_names[j<i], o6, col_names[j>i])
  }else if(length(i) >1){
    abort('"o6" cannot be used twice')
  }

  x <- read_tsv(file, col_names = col_names, comment = comment, ...)
  if(swap_query)
    x <- swap_query(x)
  x
}
