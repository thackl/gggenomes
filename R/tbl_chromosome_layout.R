#' @export
as_tbl_chromosome_layout <- function(data, ...){
    UseMethod("as_tbl_chromosome_layout")
}

#' @export
as_tbl_chromosome_layout.default <- function(data, ...){
    as_tbl_chromosome_layout(as_tbl_chromosome(data, ...))
}

# recompute layout
#' @export
as_tbl_chromosome_layout.tbl_chromosome_layout <- function(data, ...){
    stop("TODO")
}

# backtransform (drop for chromosomes, features, backtrans for links
#' @export
as_tbl_chromosome.tbl_chromosome_layout <- function(data, ...){
    stop("TODO")
}


# compute layout
#' @export
as_tbl_chromosome_layout.tbl_chromosome <- function(data, rubber=0.01,
        rubber_style = c("regular", "center", "spread")){
    
    rubber_style <- match.arg(rubber_style)
    if(! rubber_style == "regular") stop("Not yet implement")

    class(data)[class(data) == 'tbl_chromosome'] <- 'tbl_chromosome_layout'
    
    # chromosome idx
    layout <- data %>%
        mutate(
            gix = match(gid, unique(.$gid)),  # TODO: custom g-order
            goffset = 0,                      # TODO: custom g-offset
            gstrand = 1                       # TODO: custom g-strand
        ) %>%
        group_by(gix) %>%
        mutate(
            cix = row_number(),               # TODO: custom c-order
            cstrand = 1                       # TODO: custom c-strand
        )

    # infer rubber length from genome lengths
    if(rubber < 1){
        rubber <- layout %>% summarize(glength=sum(length)) %>%
            pull(glength) %>% max %>%
            "*"(rubber) %>% ceiling
    }

    # compute chromosome offsets and compose layout
    layout %>%
        mutate(
            offset = goffset + c(0, cumsum(length + rubber)[-n()]), # offset
            strand = gstrand*cstrand,
            y = gix, #yend=gix
            x =    dplyr::if_else(strand < 0, offset+length, offset),
            xend = dplyr::if_else(strand < 0, offset, offset+length)
        ) %>%
        select(y, x, xend, everything())
}
