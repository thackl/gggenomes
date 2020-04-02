#' Re-layout a genome layout
#'
#' Re-layout the tracks and update the scales after seqs have been modified
#' @export
layout <- function(x, ...){
    UseMethod("layout")
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

#' @export
layout.gggenomes_layout <- function(x){
  rlang::inform("")
  x$features %<>% map(layout_features, x$seqs)
  x$links <- map(x$orig_links, as_links, x$seqs)
  x
}

#' @export
drop_layout <- function(data, ...){
    UseMethod("drop_layout")
}

#' Select sets (genomes) by name or position
#'
#' Choose which genomes to show and in which order. Uses dyplr::select syntax,
#' which means unquoted genome names, positional arguments and select helper
#' functions, such as `starts_with()` are supported. Renaming is not supported
#' at this point because changing the genome_id might break associations with
#' other tracks.
#'
#' @export
#' @param x a gggenomes object
#' @inheritParams dplyr::select
select_set <- function(x, ...){
  # split by genome_id
  l <- x$data$contigs %>%
  drop_layout(keep="strand") %>%
    thacklr::split_by(genome_id)

  n <- tidyselect::vars_select(names(l), ...)
  l <- l[n]
  # names(l) <- names(n) # we don't want rename because changing the genome_id
  # would break connection to genes

  # recompute layout
  x$data$contigs <- as_contigs(bind_rows(l), rubber=x$data[["ggargs_"]]$rubber)
  layout(x)
}

#' @export
#' @param x a gggenomes object
#' @param set a genome to select seqs from
#' @inheritParams dplyr::select
select_seq <- function(x, set, ...){
  # split by genome_id
  l <- x$data$contigs %>%
  drop_layout(keep="strand") %>%
    thacklr::split_by(genome_id)
  # get the current genome (set)
  n <- tidyselect::vars_select(names(l), {{set}})
  s <- l[[n]]
  # select contigs of choice
  ids <- tidyselect::vars_select(s$contig_id, ...)
  i <- match(ids, s$contig_id)
  s <- s[i,]
  l[[n]] <- s

  # recompute layout
  x$data$contigs <- as_contigs(bind_rows(l), rubber=x$data[["ggargs_"]]$rubber)
  layout(x)
}


#' Flip sets (genomes) by name or position
#'
#' Invert the order and orientation of all sequences (contigs) of the specified
#' sets. Uses dyplr::select syntax, which means unquoted names, positional
#' arguments and select helper functions, such as `starts_with()` are supported.
#'
#' @export
#' @param x a gggenomes object
#' @inheritParams dplyr::select
flip_set <- function(x, ...){
  l <- x$data$contigs %>%
  drop_layout(keep="strand") %>%
    thacklr::split_by(genome_id)
  # get the current genome (set)
  sets <- tidyselect::vars_select(names(l), ...)
  for (set in sets){
    l[[set]] <- l[[set]][rev(seq_len(nrow(l[[set]]))),]
    l[[set]]$strand <- l[[set]]$strand *-1
  }
  # recompute layout
  x$data$contigs <- as_contigs(bind_rows(l), rubber=x$data[["ggargs_"]]$rubber)

  layout(x)
}

#' Flip sequences (contigs) by name or position
#'
#' Invert the order and orientation of all specified sequences (contigs). Uses
#' dyplr::select syntax, which means unquoted names, positional arguments and
#' select helper functions, such as `starts_with()` are supported.
#'
#' @export
#' @param x a gggenomes object
#' @inheritParams dplyr::select
#' @export
flip_seq <- function(x, set, ...){
  # split by genome_id
  l <- x$data$contigs %>%
  drop_layout(keep="strand") %>%
    thacklr::split_by(genome_id)
  # get the current genome (set)
  n <- tidyselect::vars_select(names(l), {{set}})
  s <- l[[n]]
  # select contigs of choice
  ids <- tidyselect::vars_select(s$contig_id, ...)
  i <- match(ids, s$contig_id)
  s$strand[i] <- s$strand[i] * -1
  l[[n]] <- s

  # recompute layout
  x$data$contigs <- as_contigs(bind_rows(l), rubber=x$data[["ggargs_"]]$rubber)
  layout(x)
}


## flip_seq <- function(x, ...){
##   contigs <- x$data$contigs %>% drop_layout(keep="strand")
##   seq_ids <- tidyselect::vars_select(contigs$contig_id, ...)
##   i <- contigs$contig_id %in% seq_ids
##   contigs$strand[i] <- contigs$strand[i] * -1

##   # recompute layout
##   x$data$contigs <- as_contigs(contigs)
##   layout(x)
## }


#' Shift set (genomes) by a certain offset
#'
#' @export
#' @param x a gggenomes object
#' @param set the set to shift
#' @param by shift by this much
shift_set <- function(x, set, by){
  set_id <- tidyselect::vars_select(unique(x$data$contigs$genome_id), {{set}})
  x$data$contigs %<>%
    left_join(tibble(genome_id = set_id, by =by)) %>%
    mutate(.goffset=.goffset+ifelse(is.na(by), 0, by),by=NULL) %>%
    add_class("tbl_contig") %>%
    drop_layout(keep = c("strand", ".goffset")) %>%
    layout_contigs(rubber = x$data[["ggargs_"]]$rubber)

  layout(x)
}



center <- function(min, max, strand, length, center=c("center", "left", "right")){
  center <- match.arg(center)
  if(center == "center") ifelse(strand < 0, round(length-mean(c(min, max))), round(mean(c(min, max))))
  else if(center == "left") ifelse(strand < 0, length-max, min)
  else if(center == "right") ifelse(strand < 0, length-min, max)
  else stop(glue("unknown center parameter {center}"))
}

#' @export
focus <- function(x, ..., track_id="genes", plus=2000, center=c("center", "left", "right"), restrict_to_contig=TRUE){
  if(length(plus==1)) plus <- c(plus,plus)

  # Specifu 'ID' column - so far not required for features
  bounds <- filter(x$data[[track_id]], ...) %>%
    group_by(genome_id, contig_id) %>%
    summarize(
      min=min(start), min_plus=min - plus[1],
      max=max(end), max_plus=max + plus[2]) %>%
     left_join(select(x$data$contigs, genome_id, contig_id, length, .strand=strand), by=c("genome_id", "contig_id")) %>%
    mutate(
      center = center(min, max, .strand, length, center))

  if(restrict_to_contig){
    bounds <- mutate(bounds,
      min_plus = if_else(min_plus<0,0,min_plus),
      max_plus = if_else(length < max_plus, length, max_plus)
    )
  }

  # make sure plus is >0, <contig_length
  for (track_id in names(x$data)[-1]){ # first is contigs
    if(inherits(x$data[[track_id]], "tbl_feature")){
      print(paste(track_id, "- recomputing layout"))
      x$data[[track_id]] %<>% left_join(bounds, by=c("genome_id", "contig_id")) %>%
        filter(end >= min_plus & start <= max_plus) %>%
        #mutate(start = start-center, end = end-center, min=NULL, max=NULL) %>%
        add_class("tbl_feature")
    }
  }

  x$data$contigs %<>%
    inner_join(select(bounds, center)) %>%
      mutate(.goffset=0-center, center=NULL) %>%
      add_class("tbl_contig") %>%
      drop_layout(keep = c("strand", ".goffset")) %>%
      layout_contigs(rubber=x$data[["ggargs_"]]$rubber)

  layout(x)
}
