#' @describeIn add_tracks Add links that connect features, such as
#' protein-protein alignments connecting genes.
#' @order 4
#' @examples
#' # Add protein-protein alignments
#' gggenomes(emale_genes) %>%
#'   add_sublinks(emale_prot_ava) +
#'   geom_gene() + geom_link()
#'
#' @export
add_sublinks <- function(x, ..., .track_id = "genes", .transform = "aa2nuc") {
  UseMethod("add_sublinks")
}

#' @export
add_sublinks.gggenomes <- function(
    x, ..., .track_id = "genes",
    .transform = "aa2nuc") {
  x$data <- add_sublinks(x$data, ...,
    .track_id = {{ .track_id }},
    .transform = .transform
  )
  x
}

#' @export
add_sublinks.gggenomes_layout <- function(
    x, ..., .track_id = "genes",
    .transform = c("aa2nuc", "none", "nuc2aa")) {
  if (length(...) == 0) {
    return(x)
  }
  dot_exprs <- enexprs(...) # defuse before list(...)
  .transform <- match_arg(.transform)

  if (.transform != "none") {
    inform(str_glue(
      'Transforming sublinks with "{.transform}".',
      ' Disable with `.transform = "none"`'
    ))
  }

  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  add_sublink_tracks(x, {{ .track_id }}, tracks, .transform)
}

add_sublink_tracks <- function(x, parent_track_id, tracks, transform) {
  feats <- pull_track(x, {{ parent_track_id }})
  links <- purrr::map(tracks, as_sublinks, get_seqs(x), feats,
    transform = transform,
    compute_layout = FALSE
  ) # layout only keeps adjacent
  x$links <- c(x$links, purrr::map(links, layout_links, get_seqs(x)))
  x$orig_links <- c(x$orig_links, purrr::map(links, as_orig_links, get_seqs(x))) # also store orig links for re-layout
  x
}

#' Compute a layout for links linking feats
#'
#' Reads sublinks connecting feats such as all-vs-all protein blasts into a
#' tidy dataframe. sublinks need to be associated with an already added feat
#' track. The sublinks are internally converted into a regular link track by
#' mapping the feat-based `start` and `end` coordinates to coordinates
#' relative to the sequences underlying the linked feats.
#'
#' The only obligatory columns are `feat_id` & `feat_id2`. Also
#' recognized are `start/end`, `start2/end2` and `strand`.
#'
#' Note `start` and `end` for every record will be coerced so that `start <
#' end`. If no `strand` was provided, `strand` will be added and set to "+" for
#' records that initially had `start < end == start2 < end2` and "-"
#' otherwise. If `strand` was provided, `start` and `end` will be reorganized to
#' conform with `start < end` without any additional effect.
#'
#' @param x sublink data convertible to a link layout
#' @param seqs the sequence layout the linked feats map onto.
#' @param feats the feats the sublinks map onto.
#' @param everything set to FALSE to drop optional columns
#' @param ... passed on to `layout_seqs()`
#' spaces, i.e. if matching nucleotide-level annotations to protein level
#' annotations, e.g. genes and protein blast results.
#' @return a tbl_df with plot coordinates
#' @export
#' @keywords internal
as_sublinks <- function(x, seqs, feats, ..., everything = TRUE) {
  UseMethod("as_sublinks")
}

#' @export
as_sublinks.default <- function(x, seqs, feats, ..., everything = TRUE) {
  # try to coerce into tbl
  as_sublinks(as_tibble(x), ...)
}

#' @export
#' @describeIn as_sublinks Convert a list of tibbles into a link layout
#' @param transform use if feats and sublinks are in different coordinate
#' @param compute_layout set to FALSE to skip layout computation
as_sublinks.tbl_df <- function(
    x, seqs, feats, ..., everything = TRUE,
    transform = c("none", "aa2nuc", "nuc2aa"), compute_layout = TRUE) {
  transform <- match.arg(transform)
  # TODO - bad transform, not none,aa2nuc,nuc2aa

  vars <- c("feat_id", "feat_id2")
  require_vars(x, vars)
  require_vars(feats, "feat_id")

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars("feat_id", "feat_id2"), as.character)
  if (!has_vars(x, c("start", "end", "start2", "end2"))) {
    if (has_vars(x, c("start", "end", "start2", "end2"), any = TRUE)) {
      abort("Need either all of start,fend1,start2,end2 or none!")
    }

    x <- x %>%
      left_join(select(feats,
         "feat_id", "seq_id", .feat_start = "start",
        .feat_end = "end", .feat_strand = "strand"
      ), by = shared_names(x, "seq_id", "feat_id")) %>%
      mutate(
        start = .data$.feat_start, end = .data$.feat_end,
        .feat_start = NULL, .feat_end = NULL
      ) %>%
      left_join(select(feats,
        feat_id2 = "feat_id", seq_id2 = "seq_id", .feat_start = "start",
        .feat_end = "end", .feat_strand2 = "strand"
      ), by = shared_names(x, "seq_id2", "feat_id2")) %>%
      mutate(
        start2 = .data$.feat_start, end2 = .data$.feat_end,
        strand = strand_chr(.data$.feat_strand == .data$.feat_strand2),
        .feat_start = NULL, .feat_end = NULL, .feat_strand = NULL, .feat_strand2 = NULL
      )

    vars <- c("feat_id", "start", "end", "feat_id2", "start2", "end2")
    other_vars <- if (everything) tidyselect::everything else function() NULL
    x <- as_tibble(select(x, all_of(vars), other_vars()))
  } else {
    vars <- c("feat_id", "start", "end", "feat_id2", "start2", "end2")
    other_vars <- if (everything) tidyselect::everything else function() NULL
    x <- as_tibble(select(x, all_of(vars), other_vars()))

    x %<>% mutate_if(is.factor, as.character)
    if (!has_name(x, "strand")) {
      x$strand <- strand_chr((x$start < x$end) == (x$start2 < x$end2))
    } else {
      x$strand <- strand_chr(x$strand)
    }

    x <- x %>% swap_if(.data$start > .data$end, .data$start, .data$end)
    x <- x %>% swap_if(.data$start2 > .data$end2, .data$start2, .data$end2)

    if (transform != "none") {
      transform <- switch(transform,
        aa2nuc = ~ 3 * .x - 2,
        nuc2aa = ~ (.x + 2) / 3
      )
      x <- mutate(x, across(c("start", "end", "start2", "end2"), transform))
    }

    # map start/end from features to seqs
    feats <- select(feats, "feat_id", "seq_id", "bin_id",
      .feat_start = "start", .feat_end = "end", .feat_strand = "strand"
    )
    x <- x %>%
      inner_join(feats, by = shared_names(x, "seq_id", "bin_id", "feat_id")) %>%
      mutate(
        start = if_reverse(.data$.feat_strand, .data$.feat_end - .data$start, .data$.feat_start + .data$start),
        end = if_reverse(.data$.feat_strand, .data$.feat_end - .data$end, .data$.feat_start + .data$end),
        .feat_start = NULL, .feat_end = NULL, .feat_strand = NULL
      )

    feats <- rename_with(feats, ~ paste0(.x, "2"))
    x <- x %>%
      inner_join(feats, by = shared_names(x, "seq_id2", "bin_id2", "feat_id2")) %>%
      mutate(
        start2 = if_reverse(.data$.feat_strand2, .data$.feat_end2 - .data$start2, .data$.feat_start2 + .data$start2),
        end2 = if_reverse(.data$.feat_strand2, .data$.feat_end2 - .data$end2, .data$.feat_start2 + .data$end2),
        .feat_start2 = NULL, .feat_end2 = NULL, .feat_strand2 = NULL
      )

    # this seems redundant but it works - that's because initially strand
    # indicates if two features align in the same or opposite direction. But
    # from here on out it has a new meaning - the actual strand of the
    # sublink-converted-link - which is a combo of link, feature, and seq strands...
    x$strand <- strand_chr((x$start < x$end) == (x$start2 < x$end2))
    x <- x %>% swap_if(.data$start > .data$end, .data$start, .data$end)
    x <- x %>% swap_if(.data$start2 > .data$end2, .data$start2, .data$end2)
  }
  if (compute_layout) {
    layout_links(x, seqs, ...)
  } else {
    x
  }
}
