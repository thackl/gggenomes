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

  vars <- c(vars, "strand", "bin_offset", "start", "end")
  if(has_name(x, "strand")){
    x$strand <- strand_chr(x$strand)
  }else{
    x$strand <- "+"
  }
  if(!has_name(x, "bin_offset")) x$bin_offset <- 0
  if(!has_name(x, "start")) x$start <- 1
  if(!has_name(x, "end")) x$end <- x$length

  other_vars <- if(everything) tidyselect::everything else function() NULL;
  x <- select(x, vars, other_vars())
  layout_seqs(x, ...)
}

#' Layout sequences
#'
#' Augment sequences with all data necessary for plotting
#'
#' @inheritParams as_seqs
#' @param spacing between sequences in bases (>1) or relative to longest bin (<1)
#' @param ... not used
#' @export
layout_seqs <- function(x, spacing=0.05, wrap=NULL,
    spacing_style = c("regular", "center", "spread"), keep="strand"){

  spacing_style <- match.arg(spacing_style)
  if(! spacing_style == "regular") stop("Not yet implement")

  x <- drop_seq_layout(x, keep=keep)

  # Index bins by order
  x %<>% mutate(y = match(bin_id, unique(.$bin_id))) %>%
    group_by(bin_id)

  # infer spacing length from bin lengths
  if(spacing < 1){
    len <- x %>% summarize(length=sum(length))
    spacing <- ceiling(max(len$length) * spacing)
  }

  # compute seq starts in layout
  if(is.null(wrap)){
    x %<>% mutate(x = bin_offset + lag(cumsum(end-start+1 + spacing), default=0))
  }else{
    x %<>% wrap(wrap, spacing)
  }

  # fix strands
  x %<>% mutate(
    xend = ifelse(is_reverse(strand), x, x+end-start+1),
    x = ifelse(is_reverse(strand), x+end-start+1, x)
  ) %>%
    select(y, x, xend, strand, everything())
}

#' @export
drop_seq_layout <- function(x, keep="strand"){
  drop <- c("y","x","xend","strand", grep("^\\.", names(x), value=T))
  drop <- drop[!drop %in% keep]
  discard(x, names(x) %in% drop)
}

# layout contigs in rectangle
wrap <- function(.data, xmax, xpad=1000){
  l <- .data %>% thacklr::split_by(bin_id)
  for(i in seq_along(l)){
    ystart <- if(i == 1) 1 else max(l[[i-1]]$y) +2
    xstart <- l[[i]]$bin_offset[1]
    l[[i]] <- wrap_impl(l[[i]], xmax=xmax, xpad=xpad, ystart=ystart, xstart=xstart)
  }
  bind_rows(l)
}



wrap_impl <- function(.data, xmax, xpad, ystart, xstart){
  l <- .data$end - .data$start # subseq length
  n <- length(l)
  x <- 1
  xend <- x + l[1]
  y <- ystart

  if(n>1){
    for(i in 2:n){
      if(xend[i-1] + xpad + l[i] > xmax){
        x[i] <- 1
        xend[i] <- 1 + l[i]
        y[i] <- y[i-1] + 1
      }else{
        x[i] <- xend[i-1] + xpad
        xend[i] <- x[i] + l[i]
        y[i] <- y[i-1]
      }
    }
  }
  .data$x <- x + xstart
  .data$xend <- xend + xstart
  .data$y <- y
  .data
}
