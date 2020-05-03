#' Plot genomes, features and synteny maps
#'
#' Sequence data: `read_fai`
#'
#' Feature data: `read_gff`
#'
#' Link data: `read_paf`
#' @inheritParams layout_genomes
#' @inheritParams ggplot2::ggplot
#' @param ... layout parameters passed on to `layout_genomes()`
#' @param theme choose a gggenomes default theme, NULL to omit.
#' @param .layout a pre-computed layout from `layout_genomes()` (advanced)
#' @import ggplot2 grid rlang
#' @export
#' @return gggenomes-flavored ggplot object
gggenomes <- function(seqs=NULL, genes=NULL, features=NULL, links=NULL, ...,
    theme = c("clean", NULL), .layout=NULL){

  # parse track_args to tracks - some magic for a convenient api
  genes_exprs <- enexpr(genes)
  features_exprs <- enexpr(features)
  links_exprs <- enexpr(links)

  genes <- as_tracks(genes, genes_exprs, "seqs")
  features <- as_tracks(features, features_exprs, c("seqs", names2(genes)))
  features <- c(genes, features) # genes are just features
  links <- as_tracks(links, links_exprs, c("seqs", names2(features)))


  layout <- .layout %||% layout_genomes(seqs=seqs, genes=genes, features=features,
                                        links=links, ...)

  p <- ggplot(data = layout)
  class(p) <- c('gggenomes', class(p))

  p <- p + scale_y_continuous("", expand = expand_scale(add=.5),
      trans = scales::reverse_trans())

  theme_name <- theme[[1]] %||% match.arg(theme[[1]], c("clean"))
  if(!is.null(theme_name)){ # add theme
    theme_args <- if(is.list(theme) && length(theme) >1) theme[-1] else list()
    p <- p + do.call(paste0("theme_gggenomes_", theme), theme_args)
  }

  p
}

#' ggplot.default tries to `fortify(data)` and we don't want that here
#'
#' @export
ggplot.gggenomes_layout <- function(data, mapping = aes(), ...,
                               environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes() or `aes_()`.", call. = FALSE)
  }

  p <- structure(list(
    data = data,
    layers = list(),
    scales = ggplot2:::scales_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(default = TRUE),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot"))

  p$labels <- ggplot2:::make_labels(mapping)

  ggplot2:::set_last_plot(p)
  p
}

#' Compute a genomes layout from sequences, features and links
#'
#' See `gggenomes::gggenomes()` for more info.
#'
#' @param seqs a table with sequence data (seq_id, bin_id, length)
#' @param genes a table or a list of table with gene data to be added as feature
#' tracks. Required columns: seq_id, bin_id, start, end.
#'
#' For a single table, adds the track_id will be "genes". For a list, track_ids
#' are parsed from the list names, or if names are missing from the name of the
#' variable containing each table.
#' @param features same as genes, but the single table track_id will default to
#' "features".
#' @param links a table or a list of tables with link data to be added as link
#' tracks (columns: from, to, from_start, from_end, to_start, to_end). Same
#' naming scheme as for features.
#' @param infer_bin_id,infer_start,infer_end,infer_length used to infer pseudo
#' seqs if only features or links are provided. The expressions are evaluated in
#' the context of the first feature track, or the first links track.
#'
#' By default subregions of sequences from the first to the last feature/link
#' are generated. Set `infer_start` to 0 to show all sequences from their
#' true beginning.
#' @param features_track_id default ID for first features track
#' @param links_track_id default ID for first links track
#' @param ... layout parameters passed on to `layout_seqs()`
#' @export
layout_genomes <- function(seqs=NULL, genes=NULL, features=NULL, links=NULL,
    infer_bin_id = seq_id, infer_start = min(start,end), infer_end = max(start,end),
    infer_length = max(start,end), ...){

  # check seqs / infer seqs if not provided
  if(!is.null(seqs)){
    if(!has_name(seqs, "bin_id"))
      seqs <- mutate(seqs, bin_id = {{ infer_bin_id }})
  }else{
    if(is.null(features) & is.null(links))
      stop("Need at least one of: seqs, genes, features or links")

    # infer dummy seqs
    if(!is.null(features)){
      write("No seqs provided, inferring seqs from features", stderr())
      seqs <- infer_seqs_from_features(features[[1]], {{infer_bin_id}}, {{infer_start}},
                                     {{infer_end}}, {{infer_length}})
    }else if(!is.null(links)){
      write("No seqs or features provided, inferring seqs from links", stderr())
      seqs <- infer_seqs_from_links(links[[1]],  {{infer_bin_id}}, {{infer_start}},
                                     {{infer_end}}, {{infer_length}})
    }
  }

  # init the gggenomes_layout object
  x <- list(seqs = NULL, features = list(), links = list(), orig_links = list(),
            args_seqs = list(...))
  x %<>% set_class("gggenomes_layout", "prepend")

  # add track data to layout
  x %<>% add_seqs(seqs, ...) # layout seqs
  if(!is.null(features)) x <- add_feature_tracks(x, features)
  if(!is.null(links)) x <- add_link_tracks(x, links)

  x
}

#' `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes
#' and error because dim(gggenome_layout) is undefined. Return dim of primary
#' table instead
#' @export
dim.gggenomes_layout <- function(x) dim(x$seqs)


infer_seqs_from_features <- function(features, infer_bin_id = seq_id, infer_start = min(start,end),
    infer_end = max(start,end), infer_length = max(start,end)){
  if(!has_name(features, "bin_id"))
    features <- mutate(features, bin_id = {{ infer_bin_id }})

  seqs <- features %>%
    group_by(bin_id, seq_id) %>%
    summarize(
      length = {{ infer_length }},
      .start = {{ infer_start }},
      .end = {{ infer_end }}
    ) %>%
    dplyr::rename(start=.start, end=.end) # this is necessary, so {{ infer_end }} does
                                 # not already use the "start" from {{ infer_start }}

  ungroup(seqs)
}

infer_seqs_from_links <- function(links, infer_bin_id = seq_id, infer_start = min(start,end),
    infer_end = max(start,end), infer_length = max(start,end)){

  seqs <- bind_rows(
    select_at(links, vars(ends_with("1")), str_replace, "1", ""),
    select_at(links, vars(ends_with("2")), str_replace, "2", "")
  )

  if(!has_name(seqs, "bin_id"))
    seqs <- mutate(seqs, bin_id = {{ infer_bin_id }})

  seqs %<>%
    mutate(bin_id = {{ infer_bin_id }}) %>%
    group_by(seq_id, bin_id) %>%
    summarize(
      length = {{ infer_length }},
      .start = {{ infer_start }},
      .end = {{ infer_end }}
    ) %>%
      dplyr::rename(start=.start, end=.end)

  ungroup(seqs)
}

#' gggenomes default theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @inheritParams ggplot2::theme_bw
#' @export
theme_gggenomes_clean <- function(base_size = 24, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22){
  theme_bw(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank()
  )
}
