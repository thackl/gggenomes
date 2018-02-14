#' @export
tbl_layout_feature <- function(features, contig_layout){
    print("TODO: check start < end")
    layout <- features %>%
        select(cid=1,fstart=2,fend=3,fstrand=4)

    contig_info <- contig_layout %>%
        mutate(
            gcoffset=goffset+coffset,
            gcstrand=gstrand*cstrand) %>%
        select(cid, gid, gix, gcoffset, gcstrand)
        
    layout %>% inner_join(contig_info) %>%
        mutate(
            fstrand=fstrand*gcstrand,
            foffset=gcoffset) %>%
        select(cid, gid, gix, foffset, fstart, fend, fstrand)
}
