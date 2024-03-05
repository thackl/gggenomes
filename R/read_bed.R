#' Read a BED file
#'
#' BED files use 0-based coordinate starts, while gggenomes uses 1-based start
#' coordinates. BED file coordinates are therefore transformed into 1-based
#' coordinates during import.
#'
#' @inheritParams readr::read_tsv
#' @param col_names column names to use. Defaults to `def_names("bed")`
#'   compatible with canonical bed files. [def_names()] can easily be
#'   combined with extra columns: `col_names = c(def_names("bed"), "more",
#'   "things")`.
#'
#' @param ... additional parameters, passed to `read_tsv`
#' @return tibble
#' @export
read_bed <- function (file, col_names = def_names("bed"),
    col_types = def_types("bed"), ...){

  x <- readr::read_tsv(file, comment = "#", col_names = col_names, col_types = col_types, ...)

  # bed is 0-based
  inform("BED files use 0-based coordinate starts - transforming to 1-based")
  x[[2]] <- x[[2]] + 1;

  x
}
