#' Read BLAST tab-separated output
#'
#' @importFrom readr read_tsv
#' @inheritParams read_gff3
#' @param col_names column names to use. Defaults to `def_names("blast")`
#'   compatible with blast tabular output (`--outfmt 6/7` in blast++ and `-m8`
#'   in blast-legacy). [def_names()] can easily be combined with extra
#'   columns: `col_names = c(def_names("blast"), "more", "things")`.
#' @param comment character
#' @param swap_query if TRUE swap query and subject columns using [swap_query()]
#'   on import.
#' @param ... additional parameters, passed to `read_tsv`
#' @return a tibble with the BLAST output
#' @export
read_blast <- function(
    file, col_names = def_names("blast"),
    col_types = def_types("blast"), comment = "#", swap_query = FALSE, ...) {
  x <- readr::read_tsv(file,
    col_names = col_names, col_types = col_types,
    comment = comment, ...
  )
  if (swap_query) {
    x <- swap_query(x)
  }
  x
}
