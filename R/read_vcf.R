#' Read a VCF file
#' 
#' VCF (Variant Call Format) file format is used to store variation data and its metadata. 
#' Based on the used analysis program (e.g. GATK, freebayes, etc...), details within the VCF file can slightly differ. 
#' For example, type of mutation is not mentioned as output for certain variant analysis programs.    
#' the "read_vcf" function, ignores the first header/metadata lines and directly converts the data into a tidy dataframe. 
#' The function will extract the type of mutation. By absence, it will derive the type of mutation from the "ref" and "alt" column.
#' 
#' @inheritParams readr::read_tsv
#' @param col_names column names to use. Defaults to `def_names("vcf")` (see [`def_names`]).
#' @param col_types column types to use. Defaults to `def_types("vcf")` (see [`def_types`]).
#' @param parse_info if set to 'TRUE', the read_vcf function will split all the metadata stored in the "info" column and stores it into separate columns. 
#' By default it is set to 'FALSE'.
#'
#' @return dataframe
#'
read_vcf <- function(file, parse_info=FALSE, col_names = def_names("vcf"), col_types = def_types("vcf")){
  x <- readr::read_tsv(file, col_names = col_names, col_types = col_types, na=".",
                comment = "#")
  
  if(parse_info){
    x <- tidy_tax(x)
  }else{
    x <- mutate(x, type = stringr::str_extract(info, "TYPE=[^;]+"), type = gsub("TYPE=", "", type))
  }
     
#Add feat_id  
  if(sum(is.na(x$feat_id))){x$feat_id <- paste0("f", seq_len(nrow((x))))}
#calculate end  
    x <- mutate(x, end = start + nchar(alt))

    
    
#Manually finding type  
  if(sum(is.na(x$type))){
    warn("Type of mutation was not found within the 'info' column. 
         It will be derived based on the 'ref' and 'alt' column.")
    x <- extract_type(x)
  }
    x
}


tidy_tax <- function(x){
  ss <- str_match_all(x$info, "([^;=]+)=([^;]+)") #or maybe x[8]
  d <- tibble(.rows= length(ss))
  ii <- rep(seq_along(ss), purrr::map_int(ss, nrow))
  mm <- list_c(ss)
  kk <- factor(mm[,2])
  vv <- mm[,3]
  for (k in levels(kk)){
    d[[k]] <- NA
    d[[k]][ii[kk==k]] <- vv[kk==k]
  }

  if(sum(colnames(d) == "TYPE")) {
       d <- rename(d, type="TYPE")
       inform(c("Harmonizing info names",
          str_glue_data(tibble(orig="TYPE", new="type"), "{orig} -> {new}")))
  }
  
  bind_cols(x, d)
}


extract_type <- function(x){
x <- mutate(x, type = case_when(
                nchar(ref) > nchar(alt) ~ "del", 
                nchar(alt) > nchar(ref) ~ "ins", 
                nchar(alt) == nchar(ref) & nchar(alt) == 1 ~ "snp",
                nchar(alt) == nchar(ref) & nchar(alt) > 1 ~ "mnp"))
}
