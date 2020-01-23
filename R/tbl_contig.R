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
as_contigs.tbl_df <- function(x, everything=TRUE, ...){
  vars <- c("genome_id","contig_id","length")
  require_vars(x, vars)
  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- select(x, vars, other_vars())
  layout_contigs(x, ...)
}

#' @export
as_tibble.tbl_contig <- function(x, ...){
  # drop all the layout stuff
  drop_layout(x)
  strip_class(x, "tbl_contig")
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
    layout_contigs(...)
}

#' @export
layout_contigs <- function(x, rubber=0.01,
    rubber_style = c("regular", "center", "spread")){
  rubber_style <- match.arg(rubber_style)
  if(! rubber_style == "regular") stop("Not yet implement")

  # contig idx
  # TODO: custom g-order
  if(!has_name(x, ".gix")) x %<>% mutate(.gix = match(genome_id, unique(.$genome_id)))
  # TODO: custom g-strand - flip entire genomes
  if(!has_name(x, "strand")) x$strand <- 1L
  # TODO: different starts for genomes, e.g. center-align genomes of diff. length
  if(!has_name(x, ".goffset")) x$.goffset <- 0

  x %<>%
    group_by(genome_id) %>%
      mutate(.cix = row_number())

  # infer rubber length from genome lengths
  if(rubber < 1){
    rubber <- x %>% summarize(.glength=sum(length)) %>%
      pull(.glength) %>% max %>%
      "*"(rubber) %>% ceiling
  }

  # compute contig offsets and compose layout
  x %<>%
  mutate(
    .offset = .goffset + c(0, cumsum(length + rubber)[-dplyr::n()]), # offset
    y = .gix, #yend=gix
    x =    dplyr::if_else(strand == -1, .offset+length, .offset),
    xend = dplyr::if_else(strand == -1, .offset, .offset+length)
  ) %>%
    select(y, x, xend, strand, everything())

  # do this last to avoid layout modification stripping the attribute
  attr(x, "require_genome_id") <- FALSE
  if(nrow(count(x, genome_id, contig_id)) != nrow(count(x, contig_id))){
    "NOTE: contig_ids are not unique among genomes. So genome_ids are required also for features and links"
    attr(x, "require_genome_id") <- TRUE
  }

  add_class(x, "tbl_contig")
}

#' @export
drop_layout.tbl_contig <- function(x, keep="strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}
