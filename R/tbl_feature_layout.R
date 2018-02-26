#' @export
as_tbl_feature_layout <- function(data, ...){
    UseMethod("as_tbl_feature_layout")
}

#' @export
as_tbl_feature_layout.default <- function(data, ...){
    as_tbl_feature_layout(as_tbl_feature(data), ...)
}

# recompute layout
#' @export
as_tbl_feature_layout.tbl_feature_layout <- function(data, ...){
    stop("TODO")
}

# backtransform (drop for features, features, backtrans for links
#' @export
as_tbl_feature.tbl_feature_layout <- function(data, ...){
    stop("TODO")
}


# compute layout
#' @export
as_tbl_feature_layout.tbl_feature <- function(data, chromosome_layout){
    layout <- chromosome_layout %>% ungroup() %>%
      select(cid, gid, y, offset, gcstrand=strand) %>%
      inner_join(data, .) %>%
      mutate(
        strand = strand*gcstrand,
        x =    dplyr::if_else(strand < 0, offset+end, offset+start),
        xend = dplyr::if_else(strand < 0, offset+start, offset+end)
      ) %>%
      select(y, x, xend, everything())

    class(layout)[class(layout) == 'tbl_feature'] <- 'tbl_feature_layout'

    layout
}
