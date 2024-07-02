#' Compute a layout for link data
#'
#' Read link data of pairwise sequence or feat comparisons, such as
#' similarity searches into a tidy dataframe and augment it with layout
#' information based on a sequence layout.
#'
#' Obligatory columns are `seq_id` and `seq_id2`. Also recognized are
#' `start`, `end`,`start2`,`end2`,`strand`, bin_id` and `bin_id2.
#'
#' During layouting, seq_id,start,end will be projected to x,xend,y, while
#' seq_id2,start2,end2 will be projected to xmin,xmax,yend. gggenomes uses these
#' maybe a bit odd names for the variables here, is so that they play nice with
#' ggplots native transformation functions for position aesthetics. Those only
#' work well with a specific set of predefined var names, which include those
#' used above.
#'
#' @param x link data convertible to a link layout
#' @inheritParams as_feats
#' @return a link layout
#' @export
#' @keywords internal
as_links <- function(x, seqs, ..., everything = TRUE) {
  UseMethod("as_links")
}

#' @export
as_links.default <- function(x, seqs, ..., everything = TRUE) {
  # try to coerce into tbl
  as_links(as_tibble(x), seqs, ..., everything = everything)
}

#' @export
as_links.tbl_df <- function(x, seqs, ..., everything = TRUE) {
  vars <- c("seq_id", "seq_id2")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars("seq_id", "seq_id2"), as.character)

  if (!has_vars(x, c("start", "end", "start2", "end2"))) {
    if (has_vars(x, c("start", "end", "start2", "end2"), any = TRUE)) {
      abort("Need either all of start,fend1,start2,end2 or none!")
    }

    x <- x %>%
      left_join(select(ungroup(seqs), seq_id = "seq_id", start = "start", end = "end"), by = "seq_id") %>%
      left_join(select(ungroup(seqs), seq_id2 = "seq_id", start2 = "start", end2 = "end"), by = "seq_id2")
  }
  vars <- c("seq_id", "start", "end", "seq_id2", "start2", "end2")

  other_vars <- if (everything) tidyselect::everything else function() NULL
  x <- as_tibble(select(x, all_of(vars), other_vars()))

  # TODO: mutate_at - if at all
  x %<>% mutate_if(is.factor, as.character)
  if (!has_vars(x, "strand")) {
    # if strand is not given but "-" link strand is encoded as end-strand,
    # add strand and recode start-end
    x <- x %>%
      mutate(x, strand = ifelse((.data$start < .data$end) == (.data$start2 < .data$end2), "+", "-")) %>%
      swap_if(.data$start > .data$end, .data$start, .data$end) %>%
      swap_if(.data$start2 > .data$end2, .data$start2, .data$end2)
  } else {
    x$strand <- strand_chr(x$strand)
  }

  layout_links(x, seqs, ...)
}

#' Layout tbl_link
#'
#' @inheritParams as_links
#' @param ... not used
#' @keywords internal
#' @noRd
layout_links <- function(
    x, seqs, keep = "strand", adjacent_only = TRUE,
    marginal = c("trim", "drop", "keep"), ...) {
  marginal <- match.arg(marginal)
  # get rid of old layout
  x <- drop_link_layout(x, keep)

  # get layout vars necessary for projecting feats from seqs
  x <- add_link_layout_scaffold(x, seqs)

  if (adjacent_only) {
    x <- filter(x, abs(.data$y - .data$yend) == 1)
    if (nrow(x) == 0) {
      warning("No links found between adjacent genomes in provided order of genomes, consider reordering genomes")
      return(tibble())
    }
  }

  # ignore feats outside subseqs
  x <- trim_links_to_subseqs(x, marginal)

  # project feats onto new layout and clean up aux vars (.seq)
  x <- project_links(x) %>%
    select("y", "x", "xend", "yend", "xmin", "xmax", everything(), -starts_with(".seq"))
  x
}


#' @describeIn add_tracks Add links connecting sequences, such as whole-genome
#' alignment data.
#' @order 2
#' @examples
#' # Add all-vs-all whole-genome alignments
#' gggenomes(seqs = emale_seqs) %>%
#'   add_links(links = emale_ava) +
#'   geom_seq() + geom_link()
#'
#' @param .adjacent_only indicate whether links should be drawn only between vertically adjacent tracks
#' @export
add_links <- function(x, ..., .adjacent_only = TRUE) {
  UseMethod("add_links")
}

#' @export
add_links.gggenomes <- function(x, ..., .adjacent_only = TRUE) {
  x$data <- add_links(x$data, ..., .adjacent_only = .adjacent_only)
  x
}

#' @export
add_links.gggenomes_layout <- function(x, ..., .adjacent_only = TRUE) {
  if (length(...) == 0) {
    return(x)
  }
  dot_exprs <- enexprs(...) # defuse before list(...)
  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  # convert to layouts
  add_link_tracks(x, tracks, adjacent_only = .adjacent_only)
}

add_link_tracks <- function(x, tracks, adjacent_only = TRUE) {
  x$links <- c(x$links, purrr::map(tracks, as_links, get_seqs(x),
    adjacent_only = adjacent_only
  )) # this is lossy, so
  x$orig_links <- c(x$orig_links, purrr::map(tracks, as_orig_links, get_seqs(x))) # also store orig links for re-layout
  x
}

# add bin_id to orig links, required for focus
as_orig_links <- function(links, seqs) {
  if (!has_vars("bin_id", "bin_id2")) {
    links <- left_join(links, select(seqs, "bin_id", "seq_id"),
      by = shared_names(links, "seq_id", "bin_id")
    )
    links <- left_join(links, select(seqs, bin_id2 = "bin_id", seq_id2 = "seq_id"),
      by = shared_names(links, "seq_id2", "bin_id2")
    )
  }
  links
}

#' Drop a link layout
#'
#' @param x link_layout
#' @param keep features to keep
#' @return link_layout without unwanted features
#' @export
drop_link_layout <- function(x, keep = "strand") {
  drop <- c("y", "x", "xend", "yend", "xmin", "xmax", "strand", grep("^\\.", names(x), value = T))
  drop <- drop[!drop %in% keep]
  purrr::discard(x, names(x) %in% drop)
}

add_link_layout_scaffold <- function(x, seqs) {
  scaffold <- seqs %>%
    ungroup() %>%
    select(
      seq_id = "seq_id", bin_id = "bin_id", y = "y", .seq_strand = "strand", .seq_x = "x",
      .seq_start = "start", .seq_end = "end"
    )
  scaffold2 <- seqs %>%
    ungroup() %>%
    select(
      seq_id2 = "seq_id", bin_id2 = "bin_id", yend = "y", .seq_strand2 = "strand", .seq_x2 = "x",
      .seq_start2 = "start", .seq_end2 = "end"
    )

  x <- inner_join(x, scaffold, by = shared_names(x, "seq_id", "bin_id"))
  x <- inner_join(x, scaffold2, by = shared_names(x, "seq_id2", "bin_id2"))
  x
}

trim_links_to_subseqs <- function(x, marginal) {
  if (marginal == "drop") {
    x <- mutate(x, .marginal = FALSE, .marginal2 = FALSE)
  } else {
    x <- mutate(x,
      .marginal = is_marginal(.data$start, .data$end, .data$.seq_start, .data$.seq_end),
      .marginal2 = is_marginal(.data$start2, .data$end2, .data$.seq_start2, .data$.seq_end2)
    )
  }

  if (marginal == "trim") {
    x %<>% mutate(
      start = ifelse(.data$.marginal & .data$start < .data$.seq_start, .data$.seq_start, .data$start),
      end = ifelse(.data$.marginal & .data$end > .data$.seq_end, .data$.seq_end, .data$end),
      start2 = ifelse(.data$.marginal2 & .data$start2 < .data$.seq_start2, .data$.seq_start2, .data$start2),
      end2 = ifelse(.data$.marginal2 & .data$end2 > .data$.seq_end2, .data$.seq_end2, .data$end2)
    )
  } # marginals are now also fully contained

  filter(
    x,
    .data$.seq_start <= .data$start & .data$end <= .data$.seq_end | .data$.marginal,
    .data$.seq_start2 <= .data$start2 & .data$end2 <= .data$.seq_end2 | .data$.marginal2,
  )
}

project_links <- function(x) {
  dummy <- rep("+", nrow(x))
  mutate(x,
    x =       x(.data$start, .data$end, dummy, .data$.seq_x, .data$.seq_start, .data$.seq_strand),
    xend = xend(.data$start, .data$end, dummy, .data$.seq_x, .data$.seq_start, .data$.seq_strand),
    xmin =    x(.data$start2, .data$end2, .data$strand, .data$.seq_x2, .data$.seq_start2, .data$.seq_strand2),
    xmax = xend(.data$start2, .data$end2, .data$strand, .data$.seq_x2, .data$.seq_start2, .data$.seq_strand2)
  )
}
