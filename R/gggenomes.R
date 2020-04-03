#' Plot genomes, features and synteny maps
#'
#' Sequence data: `read_fai`
#'
#' Feature data: `read_gff`
#'
#' Link data: `read_paf`
#' @param seqs a table with sequence data (seq_id, bin_id, length) or a layout
#' from `layout_genomes()`
#' @param theme choose a gggenomes default theme, NULL to omit.
#' @param scale choose a gggenomes dafault y-scale, NULL to omit.
#' @inheritParams gggenomes::tbl_genome
#' @inheritParams ggplot2::ggplot
#' @param ... layout parameters passed on to `layout_genomes()`
#' @importFrom ggplot2 ggplot
#' @export
#' @return gggenomes-flavored ggplot object
gggenomes <- function(seqs=NULL, features=NULL, links=NULL, ...,
    scale = c("lab", "numlab", NULL), theme = c("clean", NULL),
    mapping = aes(), environment = parent.frame()){

  # layout options need to be saved to also be applied on re-layout - temp solution
  layout <- layout_genomes(seqs=seqs, features=features, links=links, ...)
  p <- ggplot(data = layout, mapping = mapping, environment = environment)
  class(p) <- c('gggenomes', class(p))

  scale_name <- scale[[1]] %||% match.arg(scale[[1]], c("lab", "numlab"))
  if(!is.null(scale_name)){ # add gggenomes scale
    scale_args <- if(is.list(scale) && length(scale) >1) scale[-1] else list()
    p$data[["ggargs_"]]$scale_args <- scale_args # store for recomputation on re-layout
    p$data[["ggargs_"]]$scale_name <- scale_name
    scale_args$data <- layout
    p <- p + do.call(paste0("scale_gggenomes_", scale_name), scale_args)
  }

  theme_name <- theme[[1]] %||% match.arg(theme[[1]], c("clean"))
  if(!is.null(theme_name)){ # add theme
    theme_args <- if(is.list(theme) && length(theme) >1) theme[-1] else list()
    p <- p + do.call(paste0("theme_gggenomes_", theme), theme_args)
  }

  p
}

#' @export
layout.gggenomes <- function(x){
  x$data <- layout(x$data)
  scale_name <- x$data[["ggargs_"]]$scale_name
  scale_args <- x$data[["ggargs_"]]$scale_args
  scale_args$data <- x$data
  x <- x + do.call(paste0("scale_gggenomes_", scale_name), scale_args)
  x
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
#' @rdname as_genomes
#' @param seqs a table with sequence data (seq_id, bin_id, length)
#' @param features a table (or names list of tables) with feature data (seq_id,
#' bin_id, start, end)
#' @param links a table with link data (from, to, from_start, from_end,
#' to_start, to_end)
#' @param infer_bin without seqs infer bin_ids from features/links
#' @param infer_bin without seqs infer seq length from features/links
#' @param ... layout parameters passed on to `layout_seqs()`
#' @export
layout_genomes <- function(seqs=NULL, features=NULL, links=NULL, .feature_id = "genes", .link_id = "links", infer_bin = seq_id, infer_length = max(end), ...){

  x <- list(seqs = NULL, features = list(), links = list(), orig_links = list(),
            params = list())
  x %<>% set_class("gggenomes_layout", "prepend")

  if(!is.null(features) & is.data.frame(features))
    features <- set_names(list(features), .feature_id)
  if(!is.null(links) & is.data.frame(links))
    links <- set_names(list(links), .link_id)

  if(!is.null(seqs)){
    if(!has_name(seqs, "bin_id"))
      seqs <- mutate(seqs, bin_id = {{ infer_bin }})
  }else{
    if(is.null(features) & is.null(links))
      stop("Need at least one of: contigs, genes or links")

    # infer dummy seqs
    if(!is.null(features)){
      write("No seqs provided, inferring seqs from features", stderr())
      seqs <- infer_seqs_from_features(features[[1]], {{infer_bin}}, {{infer_length}})
    }else if(!is.null(links)){
      write("No seqs or features provided, inferring seqs from links", stderr())
      seqs <- infer_seqs_from_links(links[[1]], infer_bin={{infer_bin}}, {{infer_length}})
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


infer_seqs_from_features <- function(features, infer_bin = seq_id, infer_length = max(end)){
  if(!has_name(features, "bin_id"))
    features <- mutate(features, bin_id = {{ infer_bin }})

  seqs <- features %>%
    group_by(bin_id, seq_id) %>%
    summarize(length = {{ infer_length }})
}

infer_seqs_from_links <- function(links, infer_bin = seq_id, infer_length = max(end)){
  seqs <- bind_rows(
    select_at(links, vars(starts_with("from_")), str_replace, "from_", ""),
    select_at(links, vars(starts_with("to_")), str_replace, "to_", "")
  ) %>%
    rename(seq_id = id) %>%
    mutate(bin_id = {{ infer_bin }}) %>%
    group_by(bin_id, seq_id) %>%
    summarize(length = {{ infer_length }})
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
use <- function(track=seqs, ...) {
  dots <- quos(...)
    function(x, ...){
      track_names <- c("seqs", names(x$features), names(x$links))
      # more useful error
      track_name <- tryCatch(
        tidyselect::vars_pull(track_names, {{track}}),
        error = function(err){
          rlang::abort(paste("in use()", err$message))})

      if(track_name == "seqs"){
        filter(x[[track_name]], !!! dots)
      }else{
        track_types <- c("seqs", rep("features", length(x$features)),
                         rep("links", length(x$links)))
        track_type <- track_types[track_name == track_names]
        filter(x[[track_type]][[track_name]], !!! dots)
      }
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
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank())
}
#' y scale with genome labels
#'
#' @name scale_gggenomes
NULL

#' @rdname scale_gggenomes
#' @import ggplot2
#' @export
scale_gggenomes_lab <- function(data, limits = NULL, ...){
  dd <- unique(select(data$seqs, y, bin_id))
  breaks <- dd$y
  labels <- dd$bin_id

  limits <- limits %||% c(max(breaks) + .5, min(breaks) -.5)
  scale_y_continuous("", limits = limits, breaks = breaks,
    labels = labels, trans = scales::reverse_trans(), ...)
}
#' @rdname scale_gggenomes
#' @export
scale_gggenomes_numlab <- function(data, limits = NULL, ...){
    breaks <- unique(data$y)
    labels <- paste("[", breaks, "] ", unique(data$bin_id), sep="")
    limits <- limits %||% c(max(breaks) + .5, min(breaks) -.5)
    scale_y_continuous("", limits = limits, breaks = breaks,
        labels = labels, trans = scales::reverse_trans(), ...)
}
