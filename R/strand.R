# check strand  ------------------------------------------------------------
#' @export
check_strand <- function(strand, check, na){
  UseMethod("check_strand")
}

#' @export
check_strand.factor <- function(strand, na=NA){
  check_strand(as.character(strand), na=na)
}

#' @export
check_strand.numeric <- function(strand, na=NA){
  strand <- as.integer(strand)
  if(any(!strand %in% c(1,-1,0,NA)))
    rlang::abort("the only allowed values for numeric strands are [1,-1,0,NA]")
  to_na <- !strand %in% c(1,-1, na)
  strand[to_na] <- as.integer(na)
  strand
}

#' @export
check_strand.character <- function(strand, na=NA){
  if(any(!strand %in% c("+", "-", ".", NA)))
    rlang::abort("the only allowed values for character strands are [+,-,.,NA]")
  to_na <- !strand %in% c("+", "-", na)
  strand[to_na] <- as.character(na)
  strand
}

#' @export
check_strand.logical <- function(strand, na=NA){
  strand[is.na(strand)] <- as.logical(na)
  strand
}


# chr ------------------------------------------------------------
#' @export
strand_chr <- function(strand, na=NA){
  UseMethod("strand_chr")
}

#' @export
strand_chr.character <- function(strand, na=NA){
  check_strand(strand, na=na)
}

#' @export
strand_chr.factor <- function(strand, na=NA){
  strand_chr(as.character(strand), na=na)
}

#' @export
strand_chr.numeric <- function(strand, na=NA){
  strand <- check_strand(strand, na=0)
  strand <- c("-",as.character(na),"+")[strand+2]
  strand
}

#' @export
strand_chr.logical <- function(strand, na=NA){
  # convert to int first
  strand_chr(strand_int(strand, na=NA), na=na)
}


# int ------------------------------------------------------------
#' @export
strand_int <- function(strand, na=NA){
  UseMethod("strand_int")
}

#' @export
strand_int.numeric <- function(strand, na=NA){
  check_strand(strand, na=na)
}

#' @export
strand_int.logical <- function(strand, na=NA){
  strand <- check_strand(strand, na=NA)
  strand <- as.integer(strand) * 2 - 1
  strand[is.na(strand)] <- as.integer(na)
  as.integer(strand)
}

#' @export
strand_int.character <- function(strand, na=NA){
  strand <- check_strand(strand, na=NA)
  strand[strand == "+"] <- 1
  strand[strand == "-"] <- -1
  strand[is.na(strand)] <- as.integer(na)
  as.integer(strand)
}

#' @export
strand_int.factor <- function(strand, na=NA){
  strand_int(as.character(strand), na=na)
}

# lgl ------------------------------------------------------------
#' @export
strand_lgl <- function(strand, na=NA){
  UseMethod("strand_lgl")
}

#' @export
strand_lgl.logical <- function(strand, na=NA){
  check_strand(strand, na=na)
}

#' @export
strand_lgl.numeric <- function(strand, na=NA){
  strand <- as.logical(check_strand(strand, na=NA) +1)
  strand[is.na(strand)] <- as.logical(na)
  strand
}

#' @export
strand_lgl.character <- function(strand, na=NA){
  strand <- check_strand(strand, na=NA)
  strand <- strand == "+"
  strand[is.na(strand)] <- as.logical(na)
  strand
}

#' @export
is_reverse <- function(strand, na=FALSE){
  !strand_lgl(strand, na=na)
}

#' @export
flip_strand <- function(strand, na=NA){
  UseMethod("flip_strand")
}

#' @export
flip_strand.logical <- function(strand, na=NA){
  !strand_lgl(strand, na=as.logical(na))
}

#' @export
flip_strand.numeric <- function(strand, na=NA){
  strand_int(!strand_lgl(strand), na=as.integer(na))
}

#' @export
flip_strand.character <- function(strand, na=NA){
  strand_chr(!strand_lgl(strand), na=as.character(na))
}

#' @export
combine_strands <- function(strand, strand2, ...){
    strands <- c(list(strand, strand2), list(...))
    purrr::reduce(strands, combine_two_strands)
}

combine_two_strands <- function(strand, strand2){
  if(is.character(strand) || is.factor(strand))
    return(strand_chr(strand_int(strand) * strand_int(strand2)))
  if(is.logical(strand))
    return(strand_lgl(strand_int(strand) * strand_int(strand2)))
  if(is.numeric(strand))
    strand * strand_int(strand2)
}

#' @export
if_reverse <- function(strand, reverse, forward){
  ifelse(is_reverse(strand), reverse, forward)
}
