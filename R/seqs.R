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
#' @keywords internal
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

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars(seq_id, bin_id), as.character)

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
#' @param spacing between sequences in bases (>1) or relative to longest bin (<1)
#' @param wrap wrap bins into multiple lines with at most this many nucleotides
#' per lin.
#' @export
layout_seqs <- function(x, spacing=0.05, wrap=NULL,
    spacing_style = c("regular", "center", "spread"), keep="strand"){

  spacing_style <- match.arg(spacing_style)
  if(! spacing_style == "regular") stop("Not yet implement")

  x <- drop_seq_layout(x, keep=keep)

  # Index bins by order
  x %<>% mutate(y = match(bin_id, rev(unique(.$bin_id)))) %>%
    group_by(bin_id)

  #x %<>% mutate(y = match(bin_id, unique(.$bin_id))) %>%
  #  group_by(bin_id)

  # infer spacing length from bin lengths
  if(spacing < 1){
    bins <- x %>%
      group_by(bin_id) %>%
      summarize(
        bin_len=sum(length),
        seq_n=n()) %>%
      ungroup() %>%
      summarize(
        max_bin_len = max(bin_len),
        max_seq_n = max(seq_n))

    if(is.null(wrap)){
      seq_row <- bins$max_seq_n
      bin_len <- bins$max_bin_len
    }else{
      wrap_rows <- bins$max_bin_len/wrap # roughly
      seq_row <- bins$max_seq_n/wrap_rows
      bin_len <- wrap
    }

    # / sqrt(bins$max_seq_n)
    spacing <- ceiling(bin_len/sqrt(seq_row) * spacing)
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

  x <- bind_rows(l)
  # account for bins listed from top-to-bottom but y=0 at plot bottom
  x$y <- max(x$y) - x$y + 1
  x
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

#' @export
add_seqs <- function(x, seqs, ...){
  UseMethod("add_seqs")
}
#' @export
add_seqs.gggenomes <- function(x, seqs, ...){
  x$data <- add_seqs(x$data, seqs, ...)
  x
}
#' @export
add_seqs.gggenomes_layout <- function(x, seqs, ...){
  set_seqs(x, as_seqs(seqs, ...))
}

#' Get/set the seqs track
#'
#' @param x a gggenomes or gggenomes_layout objekt
#' @return a gggenomes_layout track tibble
#' @export
get_seqs <- function(x){
  UseMethod("get_seqs")
}
#' @export
get_seqs.gggenomes <- function(x){
  get_seqs(x$data)
}
#' @export
get_seqs.gggenomes_layout <- function(x){
  x$seqs[["seqs"]]
}

#' @rdname get_seqs
#' @export
set_seqs <- function(x, value){
  UseMethod("set_seqs")
}
#' @export
set_seqs.gggenomes <- function(x, value) {
  x$data <- set_seqs(x$data, value)
  x
}
#' @export
set_seqs.gggenomes_layout <- function(x, value) {
  x$seqs[["seqs"]] <- value
  x
}
