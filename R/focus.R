#' Show features and regions of interest
#'
#' Show loci containing features of interest. Loci can either be provided
#' as predefined regions directly (`loci=`), or are constructed automatically
#' based on pre-selected features (via `...`). Features within `max_dist` are
#' greedily combined into the same locus. `locate()` adds these loci as new
#' track so that they can be easily visualized. `focus()` extracts those loci
#' from their parent sequences making them the new sequence set. These sequences
#' will have their `locus_id` as their new `seq_id`.
#' @param x A gggenomes object
#' @param ... Logical predicates defined in terms of the variables in the track
#'   given by `.track_id`. Multiple conditions are combined with ‘&’. Only rows
#'   where the condition evaluates to ‘TRUE’ are kept.
#'
#'   The arguments in ‘...’ are automatically quoted and evaluated in the
#'   context of the data frame. They support unquoting and splicing. See
#'   ‘vignette("programming")’ for an introduction to these concepts.
#' @param .track_id the track to filter from - defaults to first feature track,
#'   usually "genes". Can be a quoted or unquoted string or a positional
#'   argument giving the index of a track among all tracks (seqs, feats &
#'   links).
#' @param .max_dist Maximum distance between adjacent features to be included
#'   into the same locus, default 10kb.
#' @param .expand The amount to nucleotides to expand the focus around the
#'   target features. Default 2kb. Give two values for different up- and
#'   downstream expansions.
#' @param .overhang How to handle features overlapping the locus boundaries
#'   (including expand). Options are to "keep" them, "trim" them exactly at the
#'   boundaries, or "drop" all features not fully included within the
#'   boundaries.
#' @param .locus_id,.locus_id_group How to generate the ids for the new loci
#'   which will eventually become their new `seq_id`s.
#' @param .locus_bin What bin to assign new locus to. Defaults to keeping the
#'   original binning, but can be set to the "seq" to bin all loci originating
#'   from the same parent sequence, or to "locus" to separate all loci into
#'   individual bins.
#' @param .locus_score An expression evaluated in the context of all features
#'   that are combined into a new locus. Results are stored in the column
#'   `locus_score`. Defaults to the `n()`, i.e. the number of features per
#'   locus. Set, for example, to `sum(bitscore)` to sum over all blast hit
#'   bitscore of per locus. Usually used in conjunction with `.locus_filter`.
#' @param .locus_filter An predicate expression used to post-filter identified
#'   loci. Set `.locus_filter=locus_score >= 3` to only return loci comprising
#'   at least 3 target features.
#' @param .loci A data.frame specifying loci directly. Required columns are
#'   `seq_id,start,end`. Supersedes `...`.
#' @return A gggenomes object focused on the desired loci
#' @export
#' @examples
#'
#' # Let's hunt some defense systems in marine SAGs
#' # read the genomes
#' s0 <- read_seqs(ex("gorg/gorg.fna.fai"))
#' s1 <- s0 %>%
#'   # strip trailing number from contigs to get bins
#'   dplyr::mutate(bin_id = stringr::str_remove(seq_id, "_\\d+$"))
#' # gene annotations from prokka
#' g0 <- read_feats(ex("gorg/gorg.gff.xz"))
#'
#' # best hits to the PADS Arsenal database of prokaryotic defense-system genes
#' # $ mmseqs easy-search gorg.fna pads-arsenal-v1-prf gorg-pads-defense.o6 /tmp \
#' #     --greedy-best-hits
#' f0 <- read_feats(ex("gorg/gorg-pads-defense.o6"))
#' f1 <- f0 %>%
#'   # parser system/gene info
#'   tidyr::separate(seq_id2, into = c("seq_id2", "system", "gene"), sep = ",") %>%
#'   dplyr::filter(
#'     evalue < 1e-10, # get rid of some spurious hits
#'     # and let's focus just on a few systems for this example
#'     system %in% c("CRISPR-CAS", "DISARM", "GABIJA", "LAMASSU", "THOERIS")
#'   )
#'
#' # plot the distribution of hits across full genomes
#' gggenomes(g0, s1, f1, wrap = 2e5) +
#'   geom_seq() + geom_bin_label() +
#'   scale_color_brewer(palette = "Dark2") +
#'   geom_point(aes(x = x, y = y, color = system), data = feats())
#'
#' # hilight the regions containing hits
#' gggenomes(g0, s1, f1, wrap = 2e5) %>%
#'   locate(.track_id = feats) %>%
#'   identity() +
#'   geom_seq() + geom_bin_label() +
#'   scale_color_brewer(palette = "Dark2") +
#'   geom_feat(data = feats(loci), color = "plum3") +
#'   geom_point(aes(x = x, y = y, color = system), data = feats())
#'
#' # zoom in on loci
#' gggenomes(g0, s1, f1, wrap = 5e4) %>%
#'   focus(.track_id = feats) +
#'   geom_seq() + geom_bin_label() +
#'   geom_gene() +
#'   geom_feat(aes(color = system)) +
#'   geom_feat_tag(aes(label = gene)) +
#'   scale_color_brewer(palette = "Dark2")
#' @describeIn focus Identify regions of interest and zoom in on them
focus <- function(
    x, ..., .track_id = 2, .max_dist = 10e3, .expand = 5e3,
    .overhang = c("drop", "trim", "keep"),
    .locus_id = str_glue("{seq_id}_lc{row_number()}"), .locus_id_group = seq_id,
    .locus_bin = c("bin", "seq", "locus"),
    .locus_score = n(), .locus_filter = TRUE, .loci = NULL) {
  if (length(.expand == 1)) .expand <- c(.expand, .expand)
  marginal <- match.arg(.overhang)
  bin_id <- paste0(match.arg(.locus_bin), "_id")

  # construct loci from predicate hits
  if (is.null(.loci)) {
    loci <- locate_impl(x, ...,
      .track_id = {{ .track_id }}, .max_dist = .max_dist,
      .expand = .expand, .locus_id = {{ .locus_id }},
      .locus_id_group = {{ .locus_id_group }}, .locus_bin = {{ .locus_bin }},
      .locus_score = {{ .locus_score }}, .locus_filter = {{ .locus_filter }}
    )
  } else {
    # coerce IDs to chars, so we don't get errors in join by mismatched types
    loci <- mutate(.loci, seq_id = as.character(.data$seq_id))
    if (!has_name(loci, "locus_id")) {
      loci <- loci %>%
        dplyr::group_by({{ .locus_id_group }}) %>%
        dplyr::mutate(locus_id = {{ .locus_id }}) %>%
        dplyr::ungroup()
    }
  }

  if (any(duplicated(loci$locus_id))) {
    dups <- loci$locus_id[duplicated(loci$locus_id)]
    abort(c(str_glue(
      "`locus_id`s need to be unique, you might need to revise the",
      " pattern you are using to build them."
    ), dups))
  }

  # overwrite old loci
  s <- select(ungroup(get_seqs(x)), -any_of(c("start", "end", "locus_length")))
  s <- inner_join(s, loci, by = "seq_id")
  s <- mutate(s,
    start = ifelse(.data$start < 1, 1, .data$start),
    end = ifelse(.data$length < .data$end, .data$length, .data$end),
    locus_length = width(.data$start, .data$end)
  )

  qs <- floor(stats::quantile(s$locus_length, c(0, .25, .5, .75, 1)))
  qs_lab <- c("min", "q25", "med", "q75", "max")

  inform(c(
    str_glue("Showing {nrow(loci)} loci with the following size distribution"),
    str_glue("{qs_lab}: {qs}")
  ))


  # extract for each track, for each locus
  x$data$feats <- purrr::map(x$data$feats, focus_feats, s, bin_id)
  x$data$orig_links <- purrr::map(x$data$orig_links, focus_links, s, bin_id)

  # rename seqs/bins to loci
  s <- mutate(s, bin_id = .data[[bin_id]], orig_seq_id = .data$seq_id, seq_id = .data$locus_id)

  if (FALSE) { # any(duplicated(s$seq_id))){
    # NOTE: currently, this would create a clone of the sequence - duplicating
    # sequence and feat_ids in the plot. This breaks access by ids functions
    # (pick, ...) and feat_id-based geom_gene - thinks genes on clones are
    # exons. Not sure, how to best fix that
    warn(paste(
      "focussing in on two or more loci of the same sequence is",
      "currently not supported. Will return only the first locus"
    ))

    s <- s[!duplicated(s$seq_id), ]
  }

  x <- set_seqs(x, s)
  layout(x, args_feats = list(marginal = marginal))
}

#' @export
#' @param .locus_track The name of the new track containing the identified loci.
#' @return A gggenomes object with the new loci track added
#' @describeIn focus Identify regions of interest and add them as new feature track
locate <- function(x, ..., .track_id = 2, .max_dist = 10e3, .expand = 5e3,
                   .locus_id = str_glue("{seq_id}_lc{row_number()}"), .locus_id_group = .data$seq_id,
                   .locus_bin = c("bin", "seq", "locus"),
                   .locus_score = n(), .locus_filter = TRUE, .locus_track = "loci") {
  loci <- locate_impl(x, ...,
    .track_id = {{ .track_id }}, .max_dist = .max_dist,
    .expand = .expand, .locus_id = {{ .locus_id }},
    .locus_id_group = {{ .locus_id_group }}, .locus_bin = {{ .locus_bin }},
    .locus_score = {{ .locus_score }}, .locus_filter = {{ .locus_filter }}
  )

  # odd construct to name an argument in a call based on a variable
  # essentially doing: add_feats(x, !!.locus_track=loci) - which doesn't work
  inform(str_glue(
    "Adding '{.locus_track}' track. Plot with `geom_feat(data=feats({.locus_track}))`"
  ))
  args <- set_names(list(loci), .locus_track)
  exec(add_feats, x, !!!args)
}

locate_impl <- function(
    x, ..., .track_id = 2, .max_dist = 10e3, .expand = 5e3,
    .locus_id = str_glue("{seq_id}_lc{row_number()}"), .locus_id_group = .data$seq_id,
    .locus_bin = c("bin", "seq", "locus"),
    .locus_score = n(), .locus_filter = TRUE, .loci = NULL) {
  if (length(.expand == 1)) .expand <- c(.expand, .expand)
  bin_id <- paste0(match.arg(.locus_bin), "_id")

  targets <- pull_track(x, {{ .track_id }}, ...)
  if (nrow(targets) < 1) {
    abort("Found no targets to build loci from")
  }

  # compute_loci only looks at [seq_id,start,end]-coords,
  # not [seq_id2,start2,end2]-coords.
  # => mirror links to seq1 to ensure seq2 links are accounted for
  if (track_type(x, {{ .track_id }}) == "links") {
    targets2 <- dplyr::rename(targets,
      seq_id = .data$seq_id2, seq_id2 = .data$seq_id,
      start = .data$start2, start2 = .data$start, end = .data$end2, end2 = .data$end
    )
    targets <- bind_rows(targets, targets2)
  }

  loci <- targets %>%
    compute_loci(
      max_dist = .max_dist, locus_score = {{ .locus_score }}, locus_filter = {{ .locus_filter }},
      locus_id = {{ .locus_id }}, locus_id_group = {{ .locus_id_group }}
    ) %>%
    arrange(.data$locus_id) %>%
    mutate(
      start = start - .expand[1],
      end = end + .expand[2],
      locus_length = width(start, end)
    )

  loci
}


focus_feats <- function(track, seqs, bin_id) {
  # !! is loci - rest is feature
  track <- add_feat_focus_scaffold(track, seqs)
  track <- track %>%
    filter(.data$end > .data$.seq_start & .data$start < .data$.seq_end) %>%
    mutate(bin_id = .data[[bin_id]], seq_id = .data$locus_id) %>%
    select(-starts_with(".seq"))
  track
}

add_feat_focus_scaffold <- function(track, seqs) {
  scaffold <- seqs %>%
    ungroup() %>%
    dplyr::select(
      .data$seq_id, .data$bin_id, .data$locus_id, .data$y,
      .seq_strand = .data$strand, .seq_x = .data$x, .seq_start = .data$start, .seq_end = .data$end
    )

  inner_join(track, scaffold, by = shared_names(track, "seq_id", "bin_id", "locus_id"))
}

focus_links <- function(track, seqs, bin_id) {
  bin_id2 <- paste0(bin_id, "2")
  track <- add_link_focus_scaffold(track, seqs)
  track <- track %>%
    filter(
      .data$end > .data$.seq_start & .data$start < .data$.seq_end &
        .data$end2 > .data$.seq_start2 & .data$start2 < .data$.seq_end2
    ) %>%
    mutate(
      bin_id = .data[[bin_id]], seq_id = .data$locus_id,
      bin_id2 = .data[[bin_id2]], seq_id2 = .data$locus_id2
    ) %>%
    select(-starts_with(".seq"))
  track
}

add_link_focus_scaffold <- function(track, seqs) {
  scaffold <- seqs %>%
    ungroup() %>%
    select(
      seq_id = .data$seq_id, bin_id = .data$bin_id, locus_id = .data$locus_id, y = .data$y, .seq_strand = .data$strand, .seq_x = .data$x,
      .seq_start = .data$start, .seq_end = .data$end
    )
  scaffold2 <- seqs %>%
    ungroup() %>%
    select(
      seq_id2 = .data$seq_id, bin_id2 = .data$bin_id, locus_id2 = .data$locus_id, yend = .data$y, .seq_strand2 = .data$strand, .seq_x2 = .data$x,
      .seq_start2 = .data$start, .seq_end2 = .data$end
    )

  track <- inner_join(track, scaffold, by = shared_names(track, "seq_id", "bin_id", "locus_id"))
  track <- inner_join(track, scaffold2, by = shared_names(track, "seq_id2", "bin_id2", "locus_id2"))
  track
}

compute_loci <- function(x, locus_id, locus_id_group, locus_score, locus_filter, ...) {
  index_loci(x, ...) %>%
    dplyr::group_by(.data$seq_id, .data$i) %>%
    dplyr::summarize(
      start = min(c(.data$start, .data$end)), end = max(c(.data$start, .data$end)),
      locus_score = {{ locus_score }}
    ) %>%
    ungroup() %>%
    filter({{ locus_filter }}) %>%
    dplyr::group_by({{ locus_id_group }}) %>%
    dplyr::mutate(
      i = row_number(),
      locus_id = {{ locus_id }}
    )
}

index_loci <- function(x, max_dist = 10e3) {
  x <- x %>%
    dplyr::arrange(.data$start) %>%
    dplyr::group_by(.data$seq_id) %>%
    dplyr::mutate(
      i = cumsum(pmin(.data$start, .data$end) - dplyr::lag(pmax(.data$start, .data$end), default = FALSE) > max_dist),
      i = if (min(.data$i) < 1) {
        .data$i + 1
      } else {
        .data$i
      }, # this can start at 0 or 1
    )
  x
}
