#' Plot genomes, features and synteny maps
#' 
#' @description
#' `gggenomes()` initializes a gggenomes-flavored ggplot object. 
#' It is used to declare the input data for gggenomes' track system.
#' 
#' (*See for more details on the track system, gggenomes vignette or the Details/Arguments section*)
#' 
#' 
#' @details
#' `gggenomes::gggenomes()` resembles the functionality of `ggplot2::ggplot()`. 
#' It is used to construct the initial plot object, and is often followed by "+" to add components to the plot (*e.g. "+ geom_gene()"*).
#' 
#' A big difference between the two is that gggenomes has a multi-track setup (*`'seqs'`, `'feats'`, `'genes'` and `'links'`*).
#' `gggenomes()` pre-computes a layout and adds coordinates (`y,x,xend`) to each data frame prior to the actual plot construction.
#' This has some implications for the usage of gggenomes:
#' - **Data frames for tracks have required variables.** These predefined variables are used during import 
#' to compute x/y coordinates (*see arguments*).
#' - **gggenomes' geoms can often be used without explicit `aes()` mappings**  This works because 
#' we always know the names of the plot variables ahead of time: they originate from the pre-computed layout, 
#' and we can use that information to set sensible default aesthetic mappings for most cases.
#'
#' @param genes,feats A data.frame, a list of data.frames, or a character vector
#'   with paths to files containing gene data. Each item is added as feature
#'   track.
#'
#'   For a single data.frame the track_id will be "genes" and "feats",
#'   respectively. For a list, track_ids are parsed from the list names, or if
#'   names are missing from the name of the variable containing each data.frame.
#'   Data columns:
#'
#'   - required: `seq_id,start,end`
#'   - recognized: `strand,bin_id,feat_id,introns`
#'
#' @param seqs A data.frame or a character vector with paths to files containing
#'   sequence data. Data columns:
#'
#'   - required: `seq_id,length`
#'   - recognized: `bin_id,start,end,strand`
#'
#' @param links A data.frame or a character vector with paths to files
#'   containing link data. Each item is added as links track. Data columns:
#'
#'   - required: `seq_id,seq_id2`
#'   - recognized: `start,end,bin_id,start2,end2,bin_id2,strand`
#'
#' @param .id The name of the column for file labels that are created when
#'   reading directly from files. Defaults to "file_id". Set to "bin_id" if
#'   every file represents a different bin.
##' @param infer_length,infer_start,infer_end,infer_bin_id used to infer pseudo
#' seqs if only feats or links are provided, or if no bin_id column was
#' provided. The expressions are evaluated in the context of the first feat
#' or link track.
#'
#' By default subregions of sequences from the first to the last feat/link
#' are generated. Set `infer_start` to 0 to show all sequences from their
#' true beginning.
#' @param adjacent_only Indicates whether links should be created between adjacent sequences/chromosomes only. 
#' By default it is set to `adjacent_only = TRUE`. If `FALSE`, links will be created between all sequences     
#'  
#' (*not recommended for large data sets*)
#' @inheritParams layout_seqs
#' @param theme choose a gggenomes default theme, NULL to omit.
#' @param .layout a pre-computed layout from [layout_genomes()]. Useful for
#'   developmental purposes.
#' @import rlang
#' @import ggplot2 dplyr tidyr stringr
#' @import grid
#' @export
#' @return gggenomes-flavored ggplot object
#' @examples
#' # Compare the genomic organization of three viral elements
#' # EMALEs: endogenous mavirus-like elements (example data shipped with gggenomes)
#' gggenomes(emale_genes, emale_seqs, emale_tirs, emale_ava) +
#'   geom_seq() + geom_bin_label() +                  # chromosomes and labels
#'   geom_feat(size=8) +                              # terminal inverted repeats
#'   geom_gene(aes(fill=strand), position="strand") + # genes
#'   geom_link(offset = 0.15)                         # synteny-blocks
#'
#' # with some more information
#' gggenomes(emale_genes, emale_seqs, emale_tirs, emale_ava) %>%
#'   add_feats(emale_ngaros, emale_gc) %>%
#'   add_clusters(emale_cogs) %>%
#'   sync() +
#'   geom_link(offset = 0.15, color="white") +                        # synteny-blocks
#'   geom_seq() + geom_bin_label() +                                  # chromosomes and labels
#'   # thistle4, salmon4, burlywood4
#'   geom_feat(size=6, position="identity") +                         # terminal inverted repeats
#'   geom_feat(data=feats(emale_ngaros), color="turquoise4", alpha=.3,
#'             position="strand", size=16) +
#'   geom_feat_note(aes(label=type), data=feats(emale_ngaros),
#'                  position="strand", nudge_y = .3) +
#'   geom_gene(aes(fill=cluster_id), position="strand") + # genes
#'   geom_wiggle(aes(z=score, linetype="GC-content"), feats(emale_gc),
#'               fill="lavenderblush4", position=position_nudge(y=-.2), height = .2) +
#'   scale_fill_brewer("Conserved genes", palette="Dark2", na.value = "cornsilk3")
#'
#' # initialize plot directly from files
#' gggenomes(
#'   ex("emales/emales.gff"),
#'   ex("emales/emales.gff"),
#'   ex("emales/emales-tirs.gff"),
#'   ex("emales/emales.paf")
#' ) + geom_seq() + geom_gene() + geom_feat() + geom_link()
#'
#' # multi-contig genomes wrap to fixed width
#' s0 <- read_seqs(list.files(ex("cafeteria"), "Cr.*\\.fa", full.names = TRUE))
#' s1 <- s0 %>% filter(length > 5e5)
#' gggenomes(seqs=s1, infer_bin_id=file_id, wrap=5e6) +
#'   geom_seq() + geom_bin_label() + geom_seq_label()
gggenomes <- function(genes=NULL, seqs=NULL, feats=NULL, links=NULL,
    .id="file_id", spacing=0.05, wrap=NULL, adjacent_only=TRUE,
    infer_bin_id = seq_id, infer_start = min(start,end),
    infer_end = max(start,end), infer_length = max(start,end),
    theme = c("clean", NULL), .layout=NULL, ...){

  # parse track_args to tracks - some magic for a convenient api
  genes_exprs <- enexpr(genes)
  feats_exprs <- enexpr(feats)
  links_exprs <- enexpr(links)

  genes <- as_tracks(genes, genes_exprs, "seqs", context="feats")
  feats <- as_tracks(feats, feats_exprs, c("seqs", names2(genes)), context="feats")
  feats <- c(genes, feats) # genes are just feats
  links <- as_tracks(links, links_exprs, c("seqs", names2(feats)), context="links")

  if(is_character(seqs))
    seqs <- read_seqs(seqs, .id=.id)

  layout <- .layout %||% layout_genomes(seqs=seqs, genes=genes, feats=feats,
      links=links, spacing=spacing, wrap=wrap, adjacent_only=adjacent_only,
      infer_bin_id={{infer_bin_id}}, infer_start={{infer_start}},
      infer_end={{infer_end}}, infer_length={{infer_length}}, ...)

  p <- ggplot(data = layout)
  class(p) <- c('gggenomes', class(p))

  p <- p + scale_y_continuous(expand = expansion(add=.7, mult=0.01))

  theme_name <- theme[[1]] %||% match.arg(theme[[1]], c("clean"))
  if(!is.null(theme_name)){ # add theme
    theme_args <- if(is.list(theme) && length(theme) >1) theme[-1] else list()
    p <- p + do.call(paste0("theme_gggenomes_", theme), theme_args)
  }

  p <- p + geom_blank(data=seqs())

  p
}

#' ggplot.default tries to `fortify(data)` and we don't want that here
#'
#' @export
#' @keywords internal
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
#' Layout genomes
#' @inheritParams gggenomes
#' @keywords internal
#' @return gggenomes_layout object
#' @export
layout_genomes <- function(seqs=NULL, genes=NULL, feats=NULL, links=NULL,
    infer_bin_id = seq_id, infer_start = min(start,end), infer_end = max(start,end),
    infer_length = max(start,end), adjacent_only=TRUE, ...){

  # check seqs / infer seqs if not provided
  if(!is.null(seqs)){
    if(!has_name(seqs, "bin_id"))
      seqs <- mutate(seqs, bin_id = {{ infer_bin_id }})
  }else{
    if(is.null(feats) & is.null(links))
      abort("Need at least one of: seqs, genes, feats or links")

    # infer dummy seqs
    if(!is.null(feats)){
      inform("No seqs provided, inferring seqs from feats")
      seqs <- infer_seqs_from_feats(feats[[1]], {{infer_bin_id}}, {{infer_start}},
                                     {{infer_end}}, {{infer_length}})
    }else if(!is.null(links)){
      inform("No seqs or feats provided, inferring seqs from links")
      seqs <- infer_seqs_from_links(links[[1]],  {{infer_bin_id}}, {{infer_start}},
                                     {{infer_end}}, {{infer_length}})
    }
  }

  # init the gggenomes_layout object
  x <- list(seqs = NULL, feats = list(), links = list(), orig_links = list(),
      args_seqs = list(...), args_links = list(adjacent_only=adjacent_only))
  x %<>% set_class("gggenomes_layout", "prepend")

  # add track data to layout
  x %<>% add_seqs(seqs, ...) # layout seqs
  if(!is.null(feats)) x <- add_feat_tracks(x, feats)
  if(!is.null(links)) x <- add_link_tracks(x, links, adjacent_only = adjacent_only)

  x
}

#' `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes
#' and error because dim(gggenome_layout) is undefined. Return dim of primary
#' table instead
#' @export
#' @keywords internal
dim.gggenomes_layout <- function(x) dim(get_seqs(x))

#' @export
print.gggenomes_layout <- function(x) track_info(x)

infer_seqs_from_feats <- function(feats, infer_bin_id = seq_id, infer_start = min(start,end),
    infer_end = max(start,end), infer_length = max(start,end)){
  if(!has_name(feats, "bin_id"))
    feats <- mutate(feats, bin_id = {{ infer_bin_id }})
  else
    warn("bin_id found in feats, won't overwrite")

  seqs <- feats %>%
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
theme_gggenomes_clean <- function(base_size = 12, base_family = "", base_line_size = base_size/30, base_rect_size = base_size/30){
  theme_bw(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    # x-axis
    axis.line.x = element_line(color = "black", size=.4),
    axis.title.x = element_blank(),
    axis.text.x= element_text(color = "black", size=7),
    axis.ticks.length.x = unit(.7, "mm"),
    # y-axis
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank()
  )
}


