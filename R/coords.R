#' Encode strand as 3-level factor
#'
#' This functions converts common formats for strand encoding ("+/-/.",
#' "TRUE/FALSE/NA") into  a `1,-1,0` numeric vector. This makes it
#' easy to use strandness directly in arthmetic computations
#'
#' @param strand a vector encoding strandness
#' @export
as_numeric_strand <- function(strand){
  if(is.numeric(strand)){ # make sure it's integer
    strand <- as.integer(strand)
  }else if(rlang::is_logical(strand)){
    strand <- as.integer(strand) * 2 - 1
  }else if(rlang::is_character(strand) || is.factor(strand)){
    if(any(!strand %in%  c("-", ".", "+", NA))){
      bad <- unique(strand[!strand %in%  c("-", ".", "+")])
      stop(paste0("Unknown symbols in strand encoding: ", bad))
    }
    strand <- as.integer(match(strand, c("-", ".", "+")) - 2)
  }else{
    stop("Unknown strand encoding")
  }

  strand[is.na(strand)] <- 0L

  if (min(strand) < -1 || max(strand) > 1){
    stop("Unknown strand encoding")
  }
  strand
}

#' Convert numeric `1,-1,0` strand to `+,-,NA`-factor
#'
#' @param strand a vector encoding strandness
#' @export
as_factor_strand <- function(strand){
  if(!is.numeric(strand))
    strand <- as_numeric_strand(strand)
  factor(c("-", NA, "+")[strand+2], levels=c("+","-"))
}

## x <- function(start, end, feature_strand, contig_strand=0,
##               contig_offset, contig_length){
##   contig_offset + ifelse(contig_strand >= 0,
##     ifelse(feature_strand >= 0, start, end),
##     ifelse(feature_strand <0, (end-contig_length)*-1, (start-contig_length)*-1)
##   )
## }

## xend <- function(start, end, feature_strand, contig_strand=0,
##                  contig_offset, contig_length){
##   x(start=end, end=start, feature_strand=feature_strand, contig_strand=contig_strand, contig_offset=contig_offset, contig_length=contig_length)
## }

by_strand <- function(strand, plus, minus){
  if(is.numeric(strand)) strand <- !strand < 0
  if_else(strand, plus, minus)
}

#' The virtual x-start of the full length seq in the plot
#'
#' Together with the seq strand, this is sufficient to project features
anchor <- function(x, start, strand){
  x - (start-1) * as_numeric_strand(strand)
}

#' @param seq_anchor the virtual x-start of the full length sequence in plot
#' space
x <- function(start, end, strand, seq_x, seq_start, seq_strand){
  a <- anchor(seq_x, seq_start, seq_strand)
  b <- by_strand(strand, start-1, end)
  print(tibble(a,start, end, strand, b,seq_strand))
  a + b * seq_strand
}

xend <- function(start, end, strand, seq_x, seq_start, seq_strand){
  a <- anchor(seq_x, seq_start, seq_strand)
  b <- by_strand(strand, end, start-1)#, end + width(start,end))
  a + b * seq_strand
}
