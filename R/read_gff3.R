#' Read features from GFF3 files
#'
#' Files with `##FASTA` section work but result in parsing problems for all
#' lines of the fasta section. Just ignore those warnings, or strip the fasta
#' section ahead of time from the file.
#'
#' @importFrom readr read_tsv
#' @param file toread
#' @param sources only return features from these sources
#' @param types only return features of these types, e.g. gene, CDS, ...
#' @param infer_cds_parents infer the mRNA parent for CDS features based on
#'   overlapping coordinates. In most GFFs this is properly set, but sometimes
#'   this information is missing. Generally, this is not a problem, however,
#'   geom_gene calls parse the parent information to determine which CDS and
#'   mRNAs are part of the same gene model. Without the parent info, mRNA and
#'   CDS are plotted as individual features.
#' @export
#' @return tibble
read_gff3 <- function(file, sources=NULL, types=NULL, infer_cds_parents=FALSE){
  col_names <- c("seq_id", "source", "type", "start", "end", "score", "strand", "phase", "attributes" )
  col_types <- "ccciiccic"
  x <- read_tsv(file, col_names = col_names, col_types = col_types, na=".", comment="#")

  # ignore FASTA block - dirty fix because all seqs are read into x first and
  # also create parsing warnings
  i <- which(x[[1]] == "##FASTA")
  if(length(i) > 0)
    x <- slice_head(i-1)

  reserved_names <- c(col_names[1:8], c("name", "feat_id", "parent_ids", "introns"))
  x_attrs <- tidy_attributes(x[["attributes"]], reserved_names)

  x <- bind_cols(x[,1:8], x_attrs)

  # collapse multi-line CDS (and cds_match)
  x <- x %>% group_by(type, feat_id) %>% summarize(
    introns = list(coords2introns(start, end)),
    start = min(start), end = max(end),
    parent_ids = list(first(parent_ids)), # special treat for lst_col
    across(c(-start, -end, -introns, -parent_ids), first)
  ) %>% ungroup


  if(infer_cds_parents)
    x <- infer_cds_parent(x)

  # mRNA introns from exons
  mrna_exon_introns <- filter(x, type=="exon") %>%
    select(exon_id=feat_id, start, end, feat_id=parent_ids) %>%
    unchop(feat_id) %>% group_by(feat_id) %>%
    summarize(introns = list(coords2introns(start, end)))

  # for mRNAs w/o exons: mrna_introns == cds_introns + length(five_prime_UTR)
  mrna_cds_five_prime <- filter(x, type=="five_prime_UTR") %>%
    transmute(feat_id=parent_ids, width=width(start, end)) %>% unchop(feat_id)

  mrna_cds_introns <- filter(x, type=="CDS") %>%
    select(feat_id=parent_ids, introns) %>% unchop(feat_id) %>%
    filter(!feat_id %in% mrna_exon_introns$feat_id) %>%
    left_join(mrna_cds_five_prime, by="feat_id") %>% replace_na(list(width=0)) %>%
    transmute(feat_id, introns = map2(introns, width, ~as.integer(.x+.y)))

  mrna_introns <- bind_rows(mrna_exon_introns, mrna_cds_introns)

  # unsert mRNA introns into data
  x <- left_join(x, rename(mrna_introns, mrna_introns..=introns), by="feat_id") %>%
    mutate(introns = ifelse(map_lgl(mrna_introns.., is.null), introns, mrna_introns..)) %>%
    select(-mrna_introns..)

  # make one mRNA per CDS (except operons), connect with 'geom_id'
  # 1-1 ratio makes it easy to plot mRNA-CDS gene models
  # (reasonable requirement also enforced by NCBI GFF import)
  mrna_ids <- filter(x, type=="mRNA" & !is.na(feat_id)) %>% pull(feat_id)
  # TODO single geom_id for operon mRNAs with multiple CDS kids
  cds_geom_ids <- filter(x, type=="CDS") %>% transmute(geom_id=feat_id, feat_id=feat_id)
  mrna_geom_ids <- filter(x, type=="CDS") %>% select(geom_id=feat_id, feat_id=parent_ids) %>%
    unchop(feat_id) %>% filter(feat_id %in% mrna_ids)
  # multiplies mRNAs that have multiple CDS kids (intended)
  x <- left_join(x, bind_rows(cds_geom_ids, mrna_geom_ids), by="feat_id")

  # nice order of things
  x <- relocate(x, seq_id, start, end, strand)

  # print a summary of the feats
  inform("Features read")
  x_types <- count(x, `source`, type)
  inform(paste0(format(x_types), collapse="\n"))
  x
}

infer_cds_parent <- function(x){
  i <- which(x$type == "CDS" & is.na(x$parent_ids))
  j <- which(x$type == "mRNA")

  o <- IRanges::findOverlaps(type="within",
      IRanges::IRanges(x$start[i], x$end[i]),
      IRanges::IRanges(x$start[j], x$end[j]))

  # matched orphans <- parents ID
  x$parent_ids[i[o@from]] <- x$feat_id[j[o@to]]
  x
}

tidy_attributes <- function(x, reserved_names){
  d <- map_df(str_split(x, ";"), function(r){
    r <- r[r!=""] # ignore empty elements
    z <- str_split(r, "=")
    z <- as_tibble(set_names(map(z,2), map(z,1)))
    return(z)
  })

  # make sure these columns always exist in gff-based table
  req <- tibble(ID=NA, Parent=NA, Name=NA)
  req_miss <- setdiff(names(req), names(d))
  if(length(req_miss > 0))
    d <- bind_cols(d, req[req_miss])

  # make Parent a list col (one feature can have multiple parents)
  d <- mutate(d, Parent=str_split(Parent, ","))

  # rename attributes matching reserved column names
  renames <- list(feat_id="ID", parent_ids="Parent", name="Name")
  for(name in intersect(names(d),reserved_names)){
    renames[[paste0(name, ".1")]] <- name
  }
  inform(c("Harmonizing column names",
    renames %>% unlist %>% enframe %>% str_glue_data("{value} -> {name}")))
  rename(d, !!!renames)
}


coords2introns <- function(starts, ends){
  n <- length(starts)
  if(n < 2)
    return(NULL)
  i <- 2:n
  # introns: start, end, start2, end2, ...
  c(rbind(ends[i-1], starts[i])) - starts[1]
}
