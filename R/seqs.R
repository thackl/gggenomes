#' Compute a layout for sequence data
#'
#' Read sequence data of multiple genomes (contigs, chromosomes, ...) into a
#' tidy dataframe and augment it with layout information necessary for plotting.
#'
#' Obligatory columns are `seq_id`, `bin_id` and `length`.
#'
#' @param x an object convertible to a sequence layout
#' @param everything set to FALSE to drop optional columns
#' @param ... pass through to `layout_seqs()`
#' @return an tbl_df with plot coordinates
#' @examples
#' chr <- tibble(
#'   seq_id = c("a1", "b1", "b2"),
#'   bin_id = c(rep("A", 1), rep("B",2)),
#'   length = c(5000,3000,1400))
#'
#' as_seqs(chr)
#' @export
as_seqs <- function(x, ...){
    UseMethod("as_seqs")
}

#' @export
as_seqs.default <- function(x, ...) {
    # try to coerce into tbl
    as_seqs(as_tibble(x), ...)
}

#' @export
as_seqs.tbl_df <- function(x, everything=TRUE, ...){
  vars <- c("seq_id","bin_id","length")
  require_vars(x, vars)

  vars <- c(vars, "strand", "bin_offset")
  if(has_name(x, "strand")){
    x$strand <- as_numeric_strand(x$strand)
  }else{
    x$strand <- 1L
  }
  if(!has_name(x, "bin_offset")) x$bin_offset <- 0
  
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- select(x, vars, other_vars())
  layout_seqs(x, ...)
}

#' Layout tbl_contig
#'
#' Augment tbl_feature with all data necessary for plotting
#'
#' @inheritParams as_features
#' @param ... not used
#' @export
layout.tbl_contig <- function(x, ...){
  drop_layout(x) %>%
    layout_seqs(...)
}

#' @export
layout_seqs <- function(x, rubber=0.05,
    rubber_style = c("regular", "center", "spread")){
  rubber_style <- match.arg(rubber_style)
  if(! rubber_style == "regular") stop("Not yet implement")

  # Probably obsolete - simply use y
  x %<>% mutate(y = match(bin_id, unique(.$bin_id))) %>%
    group_by(bin_id)
  
  # infer rubber length from bin lengths
  if(rubber < 1){
    len <- x %>% summarize(length=sum(length))
    rubber <- ceiling(max(len$length) * rubber)
  }

  # compute contig offsets and compose layout
  x %<>% mutate(
    x = bin_offset + lag(cumsum(length + rubber), default=0),
    xend = dplyr::if_else(strand == -1, x, x+length),
    x = dplyr::if_else(strand == -1, x+length, x)
  ) %>%
    select(y, x, xend, strand, everything())
}

#' @export
drop_layout.tbl_contig <- function(x, keep="strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}
