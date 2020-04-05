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
#' @importFrom ggplot2 ggplot
#' @export
#' @return gggenomes-flavored ggplot object
gggenomes <- function(seqs=NULL, features=NULL, links=NULL, ...,
    theme = c("clean", NULL), .layout=NULL){

  layout <- .layout %||% layout_genomes(seqs=seqs, features=features, links=links, ...)

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
#' @param features a table (or names list of tables) with feature data (seq_id,
#' bin_id, start, end)
#' @param links a table with link data (from, to, from_start, from_end,
#' to_start, to_end)
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
layout_genomes <- function(seqs=NULL, features=NULL, links=NULL, features_track_id = "genes",
    links_track_id = "links", infer_bin_id = seq_id, infer_start = min(start), infer_end = min(end),
    infer_length = max(end), ...){

  x <- list(seqs = NULL, features = list(), links = list(), orig_links = list(),
            args_seqs = list(...))
  x %<>% set_class("gggenomes_layout", "prepend")

  if(!is.null(features) & is.data.frame(features))
    features <- set_names(list(features), features_track_id)
  if(!is.null(links) & is.data.frame(links))
    links <- set_names(list(links), links_track_id)

  if(!is.null(seqs)){
    if(!has_name(seqs, "bin_id"))
      seqs <- mutate(seqs, bin_id = {{ infer_bin_id }})
  }else{
    if(is.null(features) & is.null(links))
      stop("Need at least one of: contigs, genes or links")

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

  x %<>% add_seqs(seqs, ...) # layout seqs
  if(!is.null(features)) x <- exec(add_features, x, !!!features)
  if(!is.null(links)) x <- exec(add_links, x, !!!links)
  x
}

#' `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes
#' and error because dim(gggenome_layout) is undefined. Return dim of primary
#' table instead
#' @export
dim.gggenomes_layout <- function(x) dim(x$seqs)


infer_seqs_from_features <- function(features, infer_bin_id = seq_id, infer_start = min(start),
    infer_end = max(end), infer_length = max(end)){
  if(!has_name(features, "bin_id"))
    features <- mutate(features, bin_id = {{ infer_bin_id }})

  seqs <- features %>%
    group_by(bin_id, seq_id) %>%
    summarize(
      length = {{ infer_length }},
      start = {{ infer_start }},
      end = {{ infer_end }}
    )

  seqs
}

infer_seqs_from_links <- function(links, infer_bin_id = seq_id, infer_start = min(start),
    infer_end = max(end), infer_length = max(end)){
  seqs <- bind_rows(
    select_at(links, vars(starts_with("from_")), str_replace, "from_", ""),
    select_at(links, vars(starts_with("to_")), str_replace, "to_", "")
  )

  seqs %<>% dplyr::rename(seq_id = id) %>%
    mutate(bin_id = {{ infer_bin_id }}) %>%
    group_by(seq_id, bin_id) %>%
    summarize(
      length = {{ infer_length }},
      start = {{ infer_start }},
      end = {{ infer_end }}
    )
  seqs
}

#' Use a specific track table inside a `geom_*` call.
#'
#' Use this function inside `geom_*` calls to set the track table of the
#' gggenomes layout you want to use, e.g. seqs, genes or links. Also allows you
#' to pass on filter arguments to subset the data.
#'
#' @inheritParams expose
#' @param ... filter arguments passed through to [dplyr::filter].
#' @export
use <- function(track_id=seqs, ...) {
  dots <- quos(...)
  function(x, ...){
    filter(track(x, {{track_id}}), !!! dots)
  }
}

#' Collapse seq data to bin data for geom_bin_label()
use_bins <- function(){
  function(x){
    seqs(x) %>% group_by(bin_id, y) %>%
      summarize(x = min(x), xend = max(xend)) %>%
      ungroup()
  }
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
