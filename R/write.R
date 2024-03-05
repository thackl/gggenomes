#' Write a gff3 file from a tidy table
#'
#' @importFrom stats na.omit
#' @param feats tidy feat table
#' @param file name of output file
#' @param seqs a tidy sequence table to generate optional `##sequence-region`
#'   directives in the header
#' @param type if no type column exists, use this as the default type
#' @param source if no source column exists, use this as the default source
#' @param score if no score column exists, use this as the default score
#' @param strand if no strand column exists, use this as the default strand
#' @param phase if no phase column exists, use this as the default phase
#' @param id_var the name of the column to use as the GFF3 `ID` tag
#' @param parent_var the name of the column to use as GFF3 `Parent` tag
#' @param ignore_attr attributes not to be included in GFF3 tag list. Defaults
#'   to internals: `introns, geom_id`
#' @param head additional information to add to the header section
#' @examples
#' \dontrun{
#' write_gff3(emale_genes, "emales.gff", emale_seqs, id_var="feat_id")
#' }
#' @export
write_gff3 <- function(feats, file, seqs=NULL, type=NULL, source=".", score=".", strand=".", phase=".",
  id_var="feat_id", parent_var="parent_ids", head="##gff-version 3", ignore_attr=c("introns", "geom_id")){
  if(!is.null(seqs)){
    if(!all(has_name(seqs, c("start", "end")))){
      if(!has_name(seqs, "length"))
        rlang::abort("eigher start/end or length required")
      seqs <- mutate(seqs, start = 1, end = length)
    }
    seqs <- mutate(seqs, directive="##sequence-region", end=end-start+1, start=start-start+1) %>%
      select(.data$directive, .data$seq_id, .data$start, .data$end) %>% unite(.data$seq_reg, 1:4, sep=" ")
  }

  require_vars(feats, c("seq_id", "start", "end"))
  if(!has_name(feats, "type")){
    if(is.null(type)) rlang::abort("type required")
    feats$type <- type
  }
  if(!has_name(feats, "score")) feats$score <- score
  if(!has_name(feats, "phase")) feats$phase <- phase
  if(!has_name(feats, "source")) feats$source <- source
  if(!has_name(feats, "strand")) feats$strand <- strand
  else feats$strand <- strand_chr(feats$strand, na=".")

  # arrange so that predefined gff attributes come first and in fixed order
  if(id_var %in% names(feats)){
    names(feats)[names(feats) == id_var] <- "ID"
    id_tag <- "ID"
  }else{
    id_tag <- NULL
    rlang::warn(paste("No ID variable detected. An ID tag is not required, but recommended.",
                      "Use `id_var` to specify an ID column with a different name", sep="\n"))
  }

  if(parent_var %in% names(feats)){
    names(feats)[names(feats) == parent_var] <- "Parent"
  }

  gff3_attr <- c("ID", "Parent", "Name", "Alias", "Target", "Gap", "Derives_from", "Note", "Ontology_term")
  gff3_cols <- c("seq_id", "source", "type", "start", "end", "score", "strand", "phase")

  # compute starts/ends for CDS and cDNA_match from introns
  feats <- unchop_cds(feats)
  feats <- select(feats, -all_of(ignore_attr))
  feats <- feats %>% mutate(
    across(all_of(gff3_cols), ~replace_na(.x, ".")),
    across(where(is_list), function(l) purrr::map_chr(l, stringr::str_c, collapse=","))
  )

  attr <- setdiff(names(feats), c(gff3_cols, id_tag))

  if(!is.null(id_tag) || length(attr)){
    # convert attributes tags to title case, gff convention
    attr_predef <- attr[na.omit(match(gff3_attr, str_to_title(attr)))]
    attr_predef_ii <- names(feats) %in% attr_predef
    names(feats)[attr_predef_ii] <- str_to_title(names(feats)[attr_predef_ii])
    attr_custom <- setdiff(attr, attr_predef)
    attr <- c(id_tag, str_to_title(attr_predef), attr_custom)

    for(att in attr){
      feats[[att]] <- ifelse(
        is.na(feats[[att]]) | !length(feats[[att]]),
          NA, paste0(att, "=", feats[[att]]))
    }

    feats <- unite(feats, "attr", all_of(attr), sep=";", na.rm=TRUE)
    body <- feats[,c(gff3_cols, "attr")]
  }else{
    body <- feats[,gff3_cols]
  }

  write(head, file)
  if(!is.null(seqs)) readr::write_tsv(seqs, file, append=T, col_names=F, quote="none", escape="none")
  readr::write_tsv(body, file, append=T, col_names=F, quote="none", escape="none")
}

unchop_cds <- function(x){
  # expand collapsed CDS from start/end/introns to starts/ends list cols
  coords <- purrr::pmap_df(x, function(type, start, end, introns, ...){
    if(!type %in% c("CDS", "cDNA_match")){
      tibble(start=list(start), end=list(end))
    }else{
      se <- c(start, start+introns-c(2,0), end)
      tibble(
        start=list(se[c(TRUE, FALSE)]),
        end=list(se[c(FALSE, TRUE)]))
    }
  })

  mutate(x, coords) %>% tidyr::unchop(c(start, end))
}
