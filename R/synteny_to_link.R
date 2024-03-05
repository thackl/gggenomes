#' Helper to extract seqlengths from DECIPHER::FindSyntney Object
#' @keywords internal
#' @param synteny_obj A object produced by DECIPHER::FindSynteny
get_seqlen <- function(synteny_obj) {
  seqlengths <- c()
  for(i in 1:ncol(synteny_obj)) {
    seqlengths <- c(seqlengths,unlist(.data$synteny[i,i]))
  }
  seqlength_df <- data.frame(length = seqlengths,
             fasta_name = names(seqlengths)) %>%
    tibble::remove_rownames()
  seqlength_df 
}

#' Format synteny-class into wide tibble This is for use with gggenomes
#' @importFrom methods is
#' @details This function takes a object of class synteny and converts it into a data.frame containing all synteny pairs
#' @param synteny_obj A object produced by DECIPHER::FindSynteny

synteny_to_link <- function(synteny_obj) {
  if(!is(synteny_obj,"Synteny")) {
    stop("synetny_obj is not of class Synteny")
  }
  seqlengths <- get_seqlen(synteny_obj)
    out_df <- tibble()
  for (iter in c(1:(ncol(synteny_obj)-1))) {
    tmp_df <- synteny_obj[iter+1,iter][[1]] %>% 
      as.data.frame() %>% 
      dplyr::mutate(strand = case_when(strand == 0 ~ "+",
                                       TRUE ~ "-"),
                    seq_id = (seqlengths[iter,]$fasta_name %>% stringr::str_extract("[^ ]+")),
                    start = .data$start1,
                    end = .data$end1,
                    length = seqlengths[iter,]$length,
                    seq_id2 = (seqlengths[iter+1,]$fasta_name %>% stringr::str_extract("[^ ]+")),
                    start2 = .data$start2,
                    end2 = .data$end2,
                    length2 = seqlengths[iter+1,]$length) %>% 
      dplyr::select(.data$seq_id, .data$start, .data$end,.data$length, .data$strand, .data$seq_id2, .data$start2, .data$end2, .data$length2)
  out_df <- bind_rows(out_df, tmp_df)
  }
  out_df 
}
