
#    if(!is.null(chromosome_order)){
#        if(is.list(chromosome_order)) chromosome_order %<>%
#        map_df(as_tibble, .id="gid") %>% rename(cid=value)
#        contigs %<>% full_join(chromosome_order, .)
#    }
