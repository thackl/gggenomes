#' Layout contigs
#'
#' @param rubber white space between contigs in bases, or relative to largest
#' genomes if <1.
#' @param  rubber_style regular spread aligned
#' @export
tbl_layout_contig <- function(contigs, rubber=0.01,
    rubber_style = c("regular", "center", "spread")){

    rubber_style <- match.arg(rubber_style)
    if(! rubber_style == "regular") stop("Not yet implement")

    # contig idx
    layout <- contigs %>%
        as_tibble %>%
        select(gid=1,cid=2,clength=3) %>%
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
        rubber <- layout %>% summarize(glength=sum(clength)) %>%
            pull(glength) %>% max %>%
            "*"(rubber) %>% ceiling
    }

    # compute contig offsets and compose tbl_layout_contig
    layout %<>%
        mutate(coffset = c(0, cumsum(clength + rubber)[-n()])) %>%
        # * glength? not necessary atm but maybe later for labelling etc.
        #   easy to compute sum(clength ?+ rubber?)
        # * rubber? for computations
        select(gid, gix, goffset, gstrand,
               cid, cix, coffset, clength, cstrand,
               everything())

    class(layout) <- c(class(layout), "tbl_layout_contig")
    layout
}
