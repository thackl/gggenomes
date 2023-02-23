#' Helper to extract seqlengths from DECIPHER::FindSyntney Object
#' @details Extracts the sequence lengths 
#' @param synteny_obj A object produced by DECIPHER::FindSynteny
#' @keywords internal
get_seqlen <- function(synteny_obj) {
  seqlengths <- c()
  for(i in 1:ncol(synteny_obj)) {
    seqlengths <- c(seqlengths,unlist(synteny[i,i]))
  }
  seqlength_df <- data.frame(length = seqlengths,
             fasta_name = names(seqlengths)) %>%
    tibble::remove_rownames()
  seqlength_df 
}

#' Format synteny-class into wide tibble This is for use with gggenomes
#' @details This function takes a object of class synteny and converts it into a data.frame containing all synteny pairs
#' @param synteny_obj A object produced by DECIPHER::FindSynteny

synteny_to_link <- function(synteny_obj) {
  if(class(synteny_obj) != "Synteny") {
    stop("synetny_obj is not of class Synteny")
  }
  seqlengths <- get_seqlen(synteny_obj)
    out_df <- tibble()
  for (iter in c(1:(ncol(synteny_obj)-1))) {
    tmp_df <- synteny_obj[iter+1,iter][[1]] %>% 
      as.data.frame() %>% 
      dplyr::mutate(strand = case_when(strand == 0 ~ "+",
                                       TRUE ~ "-"),
                    seq_id = (seqlengths[iter,]$fasta_name %>% str_extract("[^ ]+")),
                    start = start1,
                    end = end1,
                    length = seqlengths[iter,]$length,
                    seq_id2 = (seqlengths[iter+1,]$fasta_name %>% str_extract("[^ ]+")),
                    start2 = start2,
                    end2 = end2,
                    length2 = seqlengths[iter+1,]$length) %>% 
      dplyr::select(seq_id, start, end,length, strand, seq_id2, start2, end2, length2)
  out_df <- bind_rows(out_df, tmp_df)
  }
  out_df 
}
