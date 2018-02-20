#' layout links
#' 
#' @param links links
#' @param contig_layout contig layout
#' @importFrom magrittr %<>%
#' @export
#' @return link_layout
tbl_layout_link <- function(links, contig_layout){
    links %<>%
        inner_join(contig_layout %>% ungroup %>% select(q_cid=cid, q_gix=gix)) %>%
        inner_join(contig_layout %>% ungroup %>% select(t_cid=cid, t_gix=gix)) %>%
        filter(abs(t_gix-q_gix)==1) # filter links between non-adjacent contig_layout
    if(nrow(links)==0){
        stop("No links found between adjacent genomes in provided contig_layout, consider reordering genomes")
    }
    
    links %<>% mutate(
        lix=row_number(),
        strand=ifelse(strand=='+', 1, -1)
    ) %>%
        gather("pid", "x", q_start, q_end, t_start, t_end) %>%
        mutate(cid=ifelse(pid %in% c("q_start", "q_end"), q_cid, t_cid)) %>%
        select(-starts_with("q_"), -starts_with("t_")) %>%
        arrange(lix) %>%
        inner_join(contig_layout %>% ungroup %>% select(cid, gid, gix, coffset)) %>%
        mutate(x=x+coffset) %>%
        arrange(lix, gix) 

    links$pix <- rep(1:4, nrow(links)/4)
    links$pix[links$pix==3 & links$strand=="-"] <- 5
    links$nudge_sign <- rep(c(1,1,-1,-1), nrow(links)/4)

    links %>% arrange(lix,pix) %>%
        select(gid, gix, cid, lix, pid, pix, x, coffset, everything())
}
