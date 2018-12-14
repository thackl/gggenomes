#' Coerce data into a tidy contig data frame
#'
#' A `tbl_contig` is a data frame for storing contig (or chromosome)
#' information for multiple genomes. Obligatory columns are `genome_id`,
#' `contig_id`, and contig `length`.
#' 
#' @param x an object convertible to a `tbl_contig` 
#' 
#' @examples
#' chr <- tibble(
#'   genome_id = c(rep("A", 1), rep("B",2)),
#'   contig_id = c("a1", "b1", "b2"),
#'   length = c(5000,3000,1400))
#' 
#' as_contigs(chr)
#' @export
as_contigs <- function(x, ...){
    UseMethod("as_contigs")
}

#' @export
as_contigs.default <- function(x, ...) {
    # try to coerce into tbl
    as_contigs(as_tibble(x), ...)
}

#' @export
as_contigs.tbl_df <- function(x, everything=TRUE){
  vars <- c("genome_id","contig_id","length")
  require_vars(x, vars)
  
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- as_tibble(select(x, vars, other_vars()))
  x <- layout(set_class(x, "tbl_contig", "prepend"))

  # do this last to avoid layout modification stripping the attribute
  attr(x, "require_genome_id") <- FALSE
  if(nrow(count(x, genome_id, contig_id)) != nrow(count(x, contig_id))){
    "NOTE: contig_ids are not unique among genomes. So genome_ids are required also for features and links"
    attr(x, "require_genome_id") <- TRUE
  }
  x
}

# recompute layout
#' @export
layout.tbl_contig_layout <- function(x, ...){
    stop("TODO")
}

# backtransform (drop for contigs, features, backtrans for links
#' @export
as_tbl_contig.tbl_contig_layout <- function(x, ...){
    stop("TODO")
}

# compute layout
#' @export
layout.tbl_contig <- function(x, rubber=0.01,
        rubber_style = c("regular", "center", "spread")){
    
    rubber_style <- match.arg(rubber_style)
    if(! rubber_style == "regular") stop("Not yet implement")

    # contig idx
    x %<>%
      mutate(
        .gix = match(genome_id, unique(.$genome_id)),  # TODO: custom g-order
        .gstrand = 1L,                     # TODO: custom g-strand
        .goffset = 0                       # TODO: custom g-offset
      ) %>%
      group_by(.gix) %>%
      mutate(
        .cix = row_number(),               # TODO: custom c-order
        .cstrand = 1L                      # TODO: custom c-strand
      )

    # infer rubber length from genome lengths
    if(rubber < 1){
        rubber <- x %>% summarize(.glength=sum(length)) %>%
            pull(.glength) %>% max %>%
            "*"(rubber) %>% ceiling
    }

    # compute contig offsets and compose layout
    x %<>%
      mutate(
        .offset = .goffset + c(0, cumsum(length + rubber)[-n()]), # offset
        .strand = .gstrand*.cstrand,
        y = .gix, #yend=gix
        x =    dplyr::if_else(.strand < 0, .offset+length, .offset),
        xend = dplyr::if_else(.strand < 0, .offset, .offset+length)
      ) %>%
      select(y, x, xend, .strand, everything())

    # grouping removes tbl_contig
    set_class(x, "tbl_contig", "prepend")
}
