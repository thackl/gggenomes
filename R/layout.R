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
  print(names(l))
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
