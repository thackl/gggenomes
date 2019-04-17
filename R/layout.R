#' @export
layout <- function(data, ...){
    UseMethod("layout")
}

#' @export
drop_layout <- function(data, ...){
    UseMethod("drop_layout")
}

#' Reorder genomes and contigs
#'
#' contigs can be char or numeric (current pos)
#'
#' @export
reorder <- function(x, genomes=NULL, ...){
  TODO("partial matches a la match.arg")
  contigs <- list(...)
  # nothing to do
  if(is.null(genomes) && is_empty(contigs)) return(x)

  # split by contig id
  l <- x$data$contigs %>%
    drop_layout(keep="strand") %>%
    thacklr::split_by(genome_id)

  # reorder by genomes
  if(!is.null(genomes))
    l <- l[str_which_unique(names(l), genomes)]

  # reorder contigs
  if(!is_empty(contigs)){
    if(length(names(contigs)) < length(contigs))
      stop("All contig vectors need to be named")

    names(contigs) <- str_subset_unique(names(l), names(contigs))
    for (gid in names(contigs)){
      cids <- contigs[[gid]]
      if(is.numeric(cids))
        cidx <- cids
      else
        cidx <- na.omit(match(l[[gid]]$contig_id, cids))

      l[[gid]] <- l[[gid]][cidx,]
    }
  }

  # recompute layout
  x$data$contigs <- as_contigs(bind_rows(l))
  layout(x)
}

#' @export
flip <- function(x, genomes=NULL, ...){
  contigs <- list(...)
  # nothing to do
  if(is.null(genomes) && is_empty(contigs)) return(x)
  # split by contig id
  l <- x$data$contigs %>%
    drop_layout(keep="strand") %>%
    thacklr::split_by(genome_id)
  # flip entire genomes
  if(!is.null(genomes)){
    for(gid in str_subset_unique(names(l), genomes)){
      l[[gid]]$strand <- l[[gid]]$strand * -1
    }
  }

  if(!is_empty(contigs)){
    if(length(names(contigs)) < length(contigs))
      stop("All contig vectors need to be named")

    names(contigs) <- str_subset_unique(names(l), names(contigs))
    for (gid in names(contigs)){
      cids <- contigs[[gid]]
      if(is.numeric(cids)){
        l[[gid]]$strand[cids] <- l[[gid]]$strand[cids] * -1
      }else{
        cidx <- str_which_unique(l[[gid]]$contig_id, cids)
        l[[gid]]$strand[cidx] <- l[[gid]]$strand[cidx] * -1
      }
    }
  }

  # recompute layout
  x$data$contigs <- as_contigs(bind_rows(l))
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
      layout_contigs

  layout(x)
}
