#' @export
add_seqs <- function(x, seqs, ...){
  UseMethod("add_seqs")
}
#' @export
add_seqs.gggenomes <- function(x, seqs, ...){
  x$data <- add_seqs(x$data, seqs, ...)
  x
}
#' @export
add_seqs.gggenomes_layout <- function(x, seqs, ...){
  x$seqs <- as_seqs(seqs, ...)
  x
}

#- features --------------------------------------------------------------------
#' Add features
#' @param ... feature tables with names, i.e. genes=gene_df, snps=snp_df
#' @export
#'
#' @examples
#' gggenomes %>%
#'   add_features(genes=gene_df, snps=snp_df)
add_features <- function(x, ...){
  UseMethod("add_features")
}

#' @export
add_features.gggenomes <- function(x, ...){
  x$data <- add_features(x$data, ...)
  x
}

#' @export
add_features.gggenomes_layout <- function(x, ..., .auto_prefix="features"){
  tracks <- list(...)
  # track ids need to be unique
  if(any(names(tracks) %in% names(x$data$features)))
    stop(paste("track_id already in use:", names(tracks)[names(tracks) %in% names(x$data$features)]))

  # convert to feature layouts
  x$data$features <- c(x$data$features, map(tracks, as_features(x$seqs)))

  # make sure every track has an id
  no_names <- which(names(x$data$features) == "")
  names(x$data$features)[no_names] <- paste0(.auto_prefix, no_names)

  x
}

#- links --------------------------------------------------------------------
#' @export
add_links <- function(x, ...){
  UseMethod("add_links")
}

#' @export
add_links.gggenomes <- function(x, ...){
  x$data <- add_links(x$data, ...)
  x
}

#' @export
add_links.gggenomes_layout <- function(x, ..., .auto_prefix="links"){
  tracks <- list(...)
  # track ids need to be unique
  if(any(names(tracks) %in% names(x$data$links)))
    stop(paste("track_id already in use:", names(tracks)[names(tracks) %in% names(x$data$links)]))

  # convert to feature layouts
  x$data$links <- c(x$data$links, map(tracks, as_links(x$seqs))) # this is lossy, so
  x$data$orig_links <- c(x$data$links, tracks) # also store orig links for re-layout

  # make sure every track has an id
  no_names <- which(names(x$data$links) == "")
  names(x$data$links)[no_names] <- paste0(.auto_prefix, no_names)
  names(x$data$orig_links)[no_names] <- paste0(.auto_prefix, no_names)

  x
}
