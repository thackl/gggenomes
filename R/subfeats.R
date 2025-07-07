#' @describeIn add_tracks Add features of features, such as gene/protein
#' domains, blast hits to genes/proteins, etc.
#' @order 3
#' @export
#' @examples
#' # Add domains to genes
#' genes <- tibble::tibble(seq_id = "A", start = 100, end = 200, feat_id = "gene1")
#' domains <- tibble::tibble(feat_id = "gene1", start = 40, end = 80)
#' gggenomes(genes = genes) %>% add_subfeats(domains, .transform = "none") +
#'   geom_gene() + geom_feat()
#'
add_subfeats <- function(x, ..., .track_id = "genes", .transform = "aa2nuc") {
  UseMethod("add_subfeats")
}

#' @export
add_subfeats.gggenomes <- function(
    x, ..., .track_id = "genes",
    .transform = "aa2nuc") {
  x$data <- add_subfeats(x$data, ...,
    .track_id = {{ .track_id }},
    .transform = .transform
  )
  x
}

#' @export
add_subfeats.gggenomes_layout <- function(
    x, ..., .track_id = "genes",
    .transform = c("none", "aa2nuc", "nuc2aa")) {
  if (...length() == 0) {
    return(x)
  }
  dot_exprs <- enexprs(...) # defuse before list(...)
  .transform <- match_arg(.transform)

  if (.transform != "none") {
    inform(str_glue(
      'Transforming subfeats with "{.transform}".',
      ' Disable with `.transform = "none"`'
    ))
  }

  tracks <- as_tracks(list(...), dot_exprs, track_ids(x))
  add_subfeat_tracks(x, {{ .track_id }}, tracks, .transform)
}

add_subfeat_tracks <- function(x, parent_track_id, tracks, transform) {
  feats <- pull_track(x, {{ parent_track_id }})
  x$feats <- c(x$feats, purrr::map(
    tracks, as_subfeats, get_seqs(x), feats,
    transform = transform
  ))
  x
}

#' Compute a layout for subfeat data
#'
#' Read subfeat data such as domains or blast hits on genes into a tidy
#' dataframe. Subfeats need to be associated with an already added feat
#' track. The subfeat track itself is internally converted into a new,
#' regular feat track by mapping the `start` and `end` coordinates provided
#' relative to their parent feat into coordinates relative to the sequences
#' underlying the parent feats.
#'
#' Obligatory columns are `feat_id`, `start` and `end`. Also recognized are
#' `strand` and `bin_id`.
#'
#' Note `start` and `end` for every record will be coerced so that `start <
#' end`. If no `strand` was provided, `strand` will be added and set to "+" for
#' records that initially had `start < end` and "-" for `end < start` inputs. If
#' `strand` was provided, `start` and `end` will be reorganized to conform with
#' `start < end` without any additional effect.
#'
#' @param x subfeat data convertible to a feat layout
#' @param seqs the sequence layout the parent feats map onto.
#' @param feats the parent feats the subfeats map onto.
#' @param everything set to FALSE to drop optional columns
#' @param ... passed on to `layout_seqs()`
#' spaces, i.e. if matching nucleotide-level annotations to protein level
#' annotations, e.g. genes and protein blast results.
#' @return a tbl_df with plot coordinates
#' @export
#' @keywords internal
as_subfeats <- function(x, seqs, feats, ..., everything = TRUE) {
  UseMethod("as_subfeats")
}

#' @export
as_subfeats.default <- function(x, seqs, feats, ..., everything = TRUE) {
  # try to coerce into tbl
  as_subfeats(as_tibble(x), ...)
}

#' @export
#' @describeIn as_subfeats Convert a list of tibbles into a feat layout
#' @param transform use if feats and subfeats are in different coordinate
as_subfeats.tbl_df <- function(
    x, seqs, feats, ..., everything = TRUE,
    transform = c("none", "aa2nuc", "nuc2aa")) {
  transform <- match_arg(transform)
  # TODO - bad transform, not none,aa2nuc,nuc2aa

  vars <- c("feat_id", "start", "end")
  require_vars(x, vars)

  # coerce IDs to chars, so we don't get errors in join by mismatched types
  x <- mutate_at(x, vars("feat_id"), as.character)

  other_vars <- if (everything) tidyselect::everything else function() NULL
  x <- as_tibble(select(x, vars, other_vars()))

  # TODO: mutate_at - if at all
  x %<>% mutate_if(is.factor, as.character)
  if (!has_name(x, "strand")) {
    x$strand <- strand_chr(x$start < x$end)
  } else {
    x$strand <- strand_chr(x$strand)
  }

  x <- x %>% swap_if(.data$start > .data$end, .data$start, .data$end)
  if (transform == "aa2nuc") x <- mutate(x, start = 3 * .data$start - 2, end = 3 * .data$end - 2)
  if (transform == "nuc2aa") x <- mutate(x, start = (.data$start + 2) / 3, end = (.data$end + 2) / 3)

  x <- x %>%
    left_join(select(feats, "feat_id", "seq_id",
      .feat_start = "start",
      .feat_end = "end", .feat_strand = "strand"
    ), by = shared_names(x, "seq_id", "bin_id", "feat_id")) %>%
    mutate(
      start = ifelse(is_reverse(.data$.feat_strand), .data$.feat_end - .data$start, .data$.feat_start + .data$start),
      end = ifelse(is_reverse(.data$.feat_strand), .data$.feat_end - .data$end, .data$.feat_start + .data$end),
      .feat_start = NULL, .feat_end = NULL, .feat_strand = NULL
    )

  layout_feats(x, seqs, ...)
}
