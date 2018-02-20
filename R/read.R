#' Read .gff files using `rtracklayer::import`
#'
#' @export
#' @return tibble
read_gffs <- function(gff_files, gids = NULL){
    if (!requireNamespace("rtracklayer", quietly = TRUE)) {
        stop("Package 'rtracklayer' needed for this function to work. Please install it.",
             call. = FALSE)
    }
    data <- map(gff_files, function(gff){
        as.data.frame(rtracklayer::import(gff))
    }) %>% bind_rows %>% as_tibble %>%
        select(cid=seqnames,start,end,strand,everything()) %>%
        mutate(strand = match(strand, c("-", "*", "+")) - 2)
    write("TODO: list types, suggest filter", stderr())
    
    data
}

#' Read genome ID, contig ID and contig lengths from .gff files. Parses
#' `##sequence-region` annotation (`rtracklayer` ignores those)
#'
#' @param gids genome IDs to use with each file. If `NULL` infer from file name.
#' @export
#' @return A tibble with columns: 'gid', 'cid', 'length'.
read_gffs_as_contigs <- function(gff_files, gids = NULL){
    data <- map(gff_files, function(gff){
        data <- read_table(pipe(paste('grep ^##sequence-region ', gff)), col_names = c("cid", "from", "to"), col_types = "-cnn") %>%
        mutate(length = to - from + 1) %>%
            select(-from, -to)
    })
    
    # genome ids
    if(is.null(gids)){
        gids <- sapply(gff_files, basename) %>%
            stringr::str_replace(".gff", "")
        if(any(duplicated(gids))) stop("Filenames gave non-unique genome IDs, usee `gid=` to specify manually")
    }
    names(data) <- gids

    write("TODO: print summary info: read X genomes with y contigs, ...",
stderr())

    # bind
    bind_rows(data, .id="gid") %>% as_tibble
}

#' read a .paf file (minimap/minimap2). Only the first 12 canonical
#' columns. Ignores tagged extra fields.
#'
#' @inheritParams readr::read_tsv
#' @importFrom readr read_tsv
#' @export
#' @return tibble
read_paf <- function(file){
    read_tsv(
        pipe(paste("cut -f1-12", paf_file)),
        col_names = c("q_cid","q_clen","q_start","q_end","strand","t_cid","t_clen","t_start","t_end","match","bases","qual"))
}
