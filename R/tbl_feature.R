#' @export
as_tbl_feature <- function(data, ...){
    UseMethod("as_tbl_feature")
}

#' @export
as_tbl_feature.default <- function(data, ...) {
    # try to coerce into tbl
    as_tbl_feature(as_tibble(data), ...)
}

#' @export
as_tbl_feature.tbl_df <- function(data, cid = 1, start = 2, end = 3, strand = 4, ..., everything=TRUE){

    if(ncol(data) < 3)
        stop("At least 3 columns required: chromosome ID, start, end")

    if(is.null(strand)) data$strand <- 0
   
    data <- if(everything)
              select(data, cid = cid, start = start, end = end, strand = strand, ..., everything())
            else
              select(data, cid = cid, start = start, end = end, strand = strand, ...)
    
    data %<>% mutate_if(is.factor, as.character)

    if(is_integer(data$strand) || is_double(data$strand)){
      # do nothing
    }else if(rlang::is_character(data$strand)){
      data$strand <- match(data$strand, c("-", "*", "+")) - 2
    }else{
      stop("Unknown strand encoding")
    }

    class(data) <- c("tbl_feature", class(data))
    data
}
