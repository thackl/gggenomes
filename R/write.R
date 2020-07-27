#' Write a gff3 file from a tidy table
#'
#' @param features tidy feature table
#' @param file name of output file
#' @param seqs a tidy sequence table to generate optional `##sequence-region`
#'     directives in the header
#' @param type if no type column exists, use this as the default type
#' @param source if no source column exists, use this as the default source
#' @param score if no score column exists, use this as the default score
#' @param phase if no phase column exists, use this as the default phase
#' @param id_var the name of the column to use as the GFF3 `ID` tag
#' @param head additional information to add to the header section
#' @examples
#' \dontrun{
#' write_gff3(emale_genes, "emales.gff", emale_seqs, id_var="feature_id")
#' }
#' @export
write_gff3 <- function(features, file, seqs=NULL, type=NULL, source=".", score=".", strand=".", phase=".",
  id_var = "ID", head="##gff-version 3"){
  if(!is.null(seqs)){
    if(!all(has_name(seqs, c("start", "end")))){
      if(!has_name(seqs, "length"))
        rlang::abort("eigher start/end or length required")
      seqs <- mutate(seqs, start = 1, end = length)
    }
    seqs <- mutate(seqs, directive="##sequence-region", end=end-start+1, start=start-start+1) %>%
      select(directive, seq_id, start, end) %>% unite(seq_reg, 1:4, sep=" ")
  }

  require_vars(features, c("seq_id", "start", "end"))
  if(!has_name(features, "type")){
    if(is.null(type)) rlang::abort("type required")
    features$type <- type
  }
  if(!has_name(features, "score")) features$score <- score
  if(!has_name(features, "phase")) features$phase <- phase
  if(!has_name(features, "source")) features$source <- source
  if(!has_name(features, "strand")) features$strand <- strand
  else features$strand <- strand_chr(features$strand, na=".")

  # arrange so that predefined gff attributes come first and in fixed order
  if(id_var %in% names(features)){
    names(features)[names(features) == id_var] <- "ID"
    id_tag <- "ID"
  }else{
    id_tag <- NULL
    rlang::warn(paste("No ID variable detected. An ID tag is not required, but recommended.",
                      "Use `id_var` to specify an ID column with a different name", sep="\n"))
  }

  gff3_attr <- c("ID", "Name", "Alias", "Parent", "Target", "Gap", "Derives_from", "Note", "Ontology_term")
  cols <- c("seq_id", "source", "type", "start", "end", "score", "strand", "phase")

  features <- features %>% mutate(across(all_of(cols), ~replace_na(.x, ".")))
  attr <- setdiff(names(features), c(cols, id_tag))

  if(!is.null(id_tag) || length(attr)){
    # convert attributes tags to title case, gff convention
    attr_predef <- attr[na.omit(match(gff3_attr, str_to_title(attr)))]
    attr_predef_ii <- names(features) %in% attr_predef
    names(features)[attr_predef_ii] <- str_to_title(names(features)[attr_predef_ii])
    attr_custom <- setdiff(attr, attr_predef)
    attr <- c(id_tag, str_to_title(attr_predef), attr_custom)
    
    for(att in attr){
      features[[att]] <- ifelse(is.na(features[[att]]), "", paste0(att, "=", features[[att]]))
    }
    
    features <- unite(features, "attr", all_of(attr), sep=";") %>%
      mutate(attr = str_replace_all(attr, ";{2,}", ";"))
    body <- features[,c(cols, "attr")]
  }else{
    body <- features[,cols]
  }
  
  write(head, file)
  if(!is.null(seqs)) write_tsv(seqs, file, append=T, col_names=F)
  write_tsv(body, file, append=T, col_names=F)
}
