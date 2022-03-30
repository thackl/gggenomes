#' Read a .paf file (minimap/minimap2).
#'
#' Read a minimap/minimap2 .paf file including optional tagged extra fields. The
#' optional fields will be parsed into a tidy format, one column per tag.
#'
#' Because `readr::read_tsv` expects a fixed number of columns, but in .paf the
#' number of optional fields can differ among records, `read_paf` tries to read
#' at least as many columns as the longest record has (`max_tags`). The
#' resulting warnings for each record with fewer fields of the form "32 columns
#' expected, only 22 seen" should thus be ignored.
#'
#' From the minimap2 manual
#'
#' +----+--------+---------------------------------------------------------+
#' |Col |  Type  |                       Description                       |
#' +----+--------+---------------------------------------------------------+
#' |  1 | string | Query sequence name                                     |
#' |  2 |  int   | Query sequence length                                   |
#' |  3 |  int   | Query start coordinate (0-based)                        |
#' |  4 |  int   | Query end coordinate (0-based)                          |
#' |  5 |  char  | ‘+’ if query/target on the same strand; ‘-’ if opposite |
#' |  6 | string | Target sequence name                                    |
#' |  7 |  int   | Target sequence length                                  |
#' |  8 |  int   | Target start coordinate on the original strand          |
#' |  9 |  int   | Target end coordinate on the original strand            |
#' | 10 |  int   | Number of matching bases in the mapping                 |
#' | 11 |  int   | Number bases, including gaps, in the mapping            |
#' | 12 |  int   | Mapping quality (0-255 with 255 for missing)            |
#' +----+--------+---------------------------------------------------------+
#'
#' +----+------+-------------------------------------------------------+
#' |Tag | Type |                      Description                      |
#' +----+------+-------------------------------------------------------+
#' | tp |  A   | Type of aln: P/primary, S/secondary and I,i/inversion |
#' | cm |  i   | Number of minimizers on the chain                     |
#' | s1 |  i   | Chaining score                                        |
#' | s2 |  i   | Chaining score of the best secondary chain            |
#' | NM |  i   | Total number of mismatches and gaps in the alignment  |
#' | MD |  Z   | To generate the ref sequence in the alignment         |
#' | AS |  i   | DP alignment score                                    |
#' | ms |  i   | DP score of the max scoring segment in the alignment  |
#' | nn |  i   | Number of ambiguous bases in the alignment            |
#' | ts |  A   | Transcript strand (splice mode only)                  |
#' | cg |  Z   | CIGAR string (only in PAF)                            |
#' | cs |  Z   | Difference string                                     |
#' | dv |  f   | Approximate per-base sequence divergence              |
#' +----+------+-------------------------------------------------------+
#'
#' From https://samtools.github.io/hts-specs/SAMtags.pdf
#' type may be one of A (character), B (general array), f (real number),
#' H (hexadecimal array), i (integer), or Z (string).
#'
#' @inheritParams read_gff3
#' @importFrom readr read_tsv
#' @param max_tags maximum number of optional fields to include
#' @export
#' @return tibble
read_paf <- function (file, max_tags = 20, col_names = def_names("paf"),
    col_types = def_types("paf"), ...){

  if(max_tags > 0){
    col_names <- c(col_names, paste0("tag_", seq_len(max_tags)))
    col_types <- paste0(col_types, paste(rep("?", max_tags), collapse=""))
  }

  read_tsv(file, col_names = col_names, col_types = col_types, ...) %>%
    tidy_paf_tags
}

tidy_paf_tags <- function(.data){
  tag_df <- tibble(.rows=nrow(.data))
  tag_types <- c()
  seen_empty_tag_col <- FALSE

  for (x in select(.data, starts_with("tag_"))){
    tag_mx <- str_split(x, ":", 3, simplify=T)
    tag_mx_nr <- na.omit(unique(tag_mx[,1:2]))
    if(nrow(tag_mx_nr) == 0){
      seen_empty_tag_col <- TRUE
      break; # empty col -> seen all tags
    }
    tags <- tag_mx_nr[,1]
    tag_type <- tag_mx_nr[,2]
    names(tag_type) <- tags
    # add to global tag_type vec
    tag_types <- c(tag_types, tag_type)
    tag_types <- tag_types[unique(names(tag_types))]
    # sort tag values into tidy tag tibble
    for (tag in tags){
      if(!has_name(tag_df, tag)){ # init tag
        tag_df[[tag]] <- NA
      }
      tag_idx <- tag_mx[,1] %in% tag
      tag_df[[tag]][tag_idx] <- tag_mx[tag_idx,3]
    }
  }

  tag_df <- tag_df %>%
    mutate_at(names(tag_types)[tag_types == "i"], as.integer) %>%
    mutate_at(names(tag_types)[tag_types == "f"], as.numeric)

  if(!seen_empty_tag_col)
    rlang::warn("Found tags in max_tags column, you should increase max_tags to",
                " ensure all tags for all entries got included")

  rlang::inform(str_glue(
    "Read and tidied up a .paf file with {n_tags} optional tag fields:\n{s_tags}",
    s_tags = toString(names(tag_types)), n_tags = length(tag_types),
    "\nNote: warnings about fewer than expected columns are expected for this format."))

  # paf is 0-based
  inform("Note: .paf files use 0-based coordinate starts - transforming to 1-based")
  .data <- mutate(.data, start=start+1, start2=start2+1)
  
  bind_cols(select(.data, -starts_with("tag_")), tag_df)
}
