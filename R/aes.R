# thomasp85/ggraph https://github.com/thomasp85/ggraph/blob/master/R/aaa.R
aes_intersect <- function(aes1, aes2) {
    structure(
        c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)]),
        class = 'uneval'
    )
}

aes_nudge_by_strand <- function(mapping, nudge_by_strand, keys = c("y", "yend")){
  if(is.null(nudge_by_strand))
    return(mapping)
  
  # modify y/yend aes
  is_integer(nudge_by_strand) || is_double(nudge_by_strand) ||
    stop("nudge_by_strand needs to be numeric")

  for(k in keys){
    if(is.null(mapping[[k]])) stop("'", k, "' not defined, cannot modify")
    
    mapping[[k]] <- rlang::parse_expr(paste0(rlang::quo_text(mapping[[k]]),'+ strand * ',nudge_by_strand))
  }
  mapping
}
