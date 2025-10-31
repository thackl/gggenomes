#' Read features from GFF3 (and with some limitations GFF2/GTF) files
#'
#' Files with `##FASTA` section work but result in parsing problems for all
#' lines of the fasta section. Just ignore those warnings, or strip the fasta
#' section ahead of time from the file.
#'
#' @importFrom readr read_tsv
#' @inheritParams readr::read_tsv
#' @param sources only return features from these sources
#' @param types only return features of these types, e.g. gene, CDS, ...
#' @param infer_cds_parents infer the mRNA parent for CDS features based on
#'   overlapping coordinates. Default TRUE for gff2/gtf, FALSE for gff3. In most
#'   GFFs this is properly set, but sometimes this information is missing.
#'   Generally, this is not a problem, however, geom_gene calls parse the parent
#'   information to determine which CDS and mRNAs are part of the same gene
#'   model. Without the parent info, mRNA and CDS are plotted as individual
#'   features.
#' @param sort_exons make sure that exons/introns appear sorted. Default TRUE.
#'   Set to FALSE to read CDS/exon order exactly as present in the file, which
#'   is less robust, but faster and allows non-canonical splicing
#'   (exon1-exon3-exon2).
#' @param col_names column names to use. Defaults to `def_names("gff3")` (see [`def_names`]).
#' @param col_types column types to use. Defaults to `def_types("gff3")` (see [`def_types`]).
#' @param keep_attr keep the original attributes column also after parsing
#'   tag=value pairs into tidy columns.
#' @param fix_augustus_cds If true, assume Augustus gff with bad CDS IDs that
#'   need fixing
#' @param is_gff2 set if file is in gff2 format
#' @export
#' @return tibble
read_gff3 <- function(
    file, sources = NULL, types = NULL, infer_cds_parents = is_gff2,
    sort_exons = TRUE, col_names = def_names("gff3"),
    col_types = def_types("gff3"), keep_attr = FALSE, fix_augustus_cds = TRUE, is_gff2 = NULL) {
  # there seems to be an issue with 'na="."' in readr::read_tsv
  # https://github.com/tidyverse/readr/issues/1279
  x <- readr::read_tsv(file,
    col_names = col_names, col_types = col_types, na = ".",
    comment = "#"
  )

  # ignore FASTA block - dirty fix because all seqs are read into x first and
  # create parsing warnings
  i <- str_which(x[[1]], "^>")[1]
  if (!is.na(i)) {
    x <- slice_head(x, n = i - 1)
    warn(str_glue(
      "Note: File contains ##FASTA section starting at line {i}.\n",
      "You can ignore any parsing failures starting from that row."
    ))
  }

  if (!is.null(types)) {
    x <- filter(x, .data$type %in% types)
  }

  if (!is.null(sources)) {
    x <- filter(x, source %in% sources)
  }

  # guess if gff2/gtf - " " instead of "=" as sep for attribute "tag value" pairs
  if (is.null(is_gff2)) {
    is_gff2 <- guess_is_gff2(x)
  }

  if (is_gff2) {
    warn(str_glue(
      "This looks like a gff2/gtf file. This is usually fine, ",
      "but given the ambigious definition of this format, it is not ",
      "guaranteed that gene models are always captured correctly. ",
      "exons/CDS might not be recognized as belonging to the same gene, etc. ",
      "Also note: types and attributes are as far as possible converted to ",
      "match gff3 standards (transcript -> mRNA, 5'/3'UTR -> five/three_prime_UTR, ...)"
    ))

    # make this consistent with gff3
    x[[3]][x[[3]] == "transcript"] <- "mRNA"
    x[[3]][x[[3]] == "5'UTR"] <- "five_prime_UTR"
    x[[3]][x[[3]] == "3'UTR"] <- "three_prime_UTR"
    x[[3]][x[[3]] == "cds"] <- "CDS"
  }

  x <- tidy_attributes(x,
    is_gff2 = is_gff2, keep_attr = keep_attr,
    fix_augustus_cds = fix_augustus_cds
  )

  # collapse multi-line CDS (and cds_match)
  x <- dplyr::mutate(x, .row_index = row_number()) # helper for robust order
  x <- x %>%
    dplyr::group_by(.data$type, .data$feat_id) %>%
    dplyr::summarize(
      introns = list(coords2introns(start, end, sort_exons)),
      start = min(start), end = max(end),
      parent_ids = list(first(.data$parent_ids)), # special treat for lst_col
      dplyr::across(c(-"start", -"end", -"introns", -"parent_ids"), first)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$.row_index) %>%
    dplyr::select(-".row_index")

  # band aid fix for collapsed CDS/cDNA_match - set score and phase NA because
  # it differs for different spans and we don't store this as a list col
  cds_collapsed_i <- x$type %in% c("CDS", "cDNA_match") & !purrr::map_lgl(x$introns, is.null)
  if (any(cds_collapsed_i)) {
    x$score[cds_collapsed_i] <- NA
    x$phase[cds_collapsed_i] <- NA
  }

  if (is_gff2) {
    x <- add_mrna_for_exons(x, col_names)
  }

  if (infer_cds_parents) {
    # this is a fallback primarily to pass tests in r-devel where IRanges is not always available
    if (!requireNamespace("IRanges", quietly = TRUE)) {
      warn("Bioconductor IRanges missing but required for CDS parent inference. Skipping inference.")
    }else{
      x <- infer_cds_parent(x)
    }
  }

  # mRNA introns from exons
  mrna_exon_introns <- filter(x, .data$type == "exon") %>%
    select(exon_id = "feat_id", "start", "end", feat_id = "parent_ids") %>%
    tidyr::unchop(feat_id) %>%
    dplyr::group_by(.data$feat_id) %>%
    summarize(introns = list(coords2introns(.data$start, .data$end, sort_exons)))

  # for mRNAs w/o exons: mrna_introns == cds_introns + length(five_prime_UTR)
  mrna_cds_five_prime <- filter(x, .data$type == "five_prime_UTR") %>%
    transmute(feat_id = .data$parent_ids, width = width(start, end)) %>%
    tidyr::unchop(feat_id)

  mrna_cds_introns <- filter(x, .data$type == "CDS") %>%
    select(feat_id = "parent_ids", "introns") %>%
    tidyr::unchop(feat_id) %>%
    filter(!.data$feat_id %in% mrna_exon_introns$feat_id) %>%
    left_join(mrna_cds_five_prime, by = "feat_id") %>%
    replace_na(list(width = 0)) %>%
    transmute(.data$feat_id, introns = (purrr::map2(.data$introns, width, ~ as.integer(.x + .y))))

  mrna_introns <- bind_rows(mrna_exon_introns, mrna_cds_introns) %>%
    mutate(feat_id = as.character(.data$feat_id))

  # unsert mRNA introns into data
  x <- left_join(x, rename(mrna_introns, mrna_introns.. = "introns"), by = "feat_id") %>%
    mutate(introns = ifelse(purrr::map_lgl(.data$mrna_introns.., is.null), .data$introns, .data$mrna_introns..)) %>%
    select(-"mrna_introns..")

  # make one mRNA per CDS (except operons), connect with 'geom_id'
  # 1-1 ratio makes it easy to plot mRNA-CDS gene models
  # (reasonable requirement also enforced by NCBI GFF import)
  mrna_ids <- filter(x, .data$type == "mRNA" & !is.na(.data$feat_id)) %>% pull(.data$feat_id)
  # TODO single geom_id for operon mRNAs with multiple CDS kids
  cds_geom_ids <- filter(x, .data$type == "CDS") %>% transmute(geom_id = .data$feat_id, feat_id = .data$feat_id)
  mrna_geom_ids <- filter(x, .data$type == "CDS") %>%
    select(geom_id = "feat_id", feat_id = "parent_ids") %>%
    tidyr::unchop(feat_id) %>%
    filter(.data$feat_id %in% mrna_ids)
  # multiplies mRNAs that have multiple CDS kids (intended)
  x <- left_join(x, bind_rows(cds_geom_ids, mrna_geom_ids), by = "feat_id")

  # nice order of things
  x <- relocate(x, "seq_id", "start", "end", "strand")

  # print a summary of the feats
  inform("Features read")
  x_types <- count(x, .data$source, .data$type)
  inform(paste0(format(x_types), collapse = "\n"))
  x
}

add_mrna_for_exons <- function(x, col_names) {
  # add one mRNA for each exon w/ parent_id that doesn't exist
  mrna_ids <- filter(x, .data$type == "mRNA")[["feat_id"]]
  # orfan exons
  x2 <- mutate(x, .row_index = row_number())
  exons <- filter(x2, .data$type == "exon") %>%
    tidyr::unchop(parent_ids) %>%
    filter(!.data$parent_ids %in% mrna_ids)

  if (nrow(exons) == 0) {
    return(x)
  }

  mrnas <- exons %>%
    dplyr::select(all_of(col_names[1:8]), feat_id = "parent_ids", ".row_index") %>%
    dplyr::group_by(.data$feat_id) %>%
    dplyr::summarize(
      dplyr::across(c(-"start", -"end"), first),
      start = min(.data$start), end = max(.data$end)
    ) %>%
    dplyr::select(all_of(col_names[1:8]), "feat_id", ".row_index") %>%
    dplyr::mutate(
      type = "mRNA", parent_ids = NA_character_, name = NA_character_,
      parent_ids = as.list(.data$parent_ids)
    )

  # insert mrnas right before exons
  x2 <- bind_rows(mrnas, x2) %>%
    arrange(.data$.row_index) %>%
    select(-".row_index")
  x2
}

infer_cds_parent <- function(x) {
  i <- which(x$type == "CDS" & is.na(x$parent_ids))
  j <- which(x$type == "mRNA")

  o <- IRanges::findOverlaps(
    type = "within",
    IRanges::IRanges(x$start[i], x$end[i]),
    IRanges::IRanges(x$start[j], x$end[j])
  )

  # matched orphans <- parents ID
  x$parent_ids[i[o@from]] <- x$feat_id[j[o@to]]
  x
}

tidy_attributes <- function(x, is_gff2 = FALSE, keep_attr = FALSE, fix_augustus_cds = TRUE) {
  d <- purrr::map_df(str_split(x[[9]], "; *"), function(r) {
    # handle missing comments
    if (all(is.na(r) | str_length(r) == 0)) {
      return(tibble(.rows = 1))
    } # make sure this df has at least 1 row!

    # ignore empty elements caused by trailing or duplicated ";"
    r <- r[r != ""]
    z <- str_split(r, "[= ]", 2)
    k <- as.list(make.unique(purrr::map_chr(z, 1), sep = "_"))
    v <- purrr::map(z, 2)
    z <- as_tibble(set_names(v, k))
    return(z)
  })

  if (is_gff2) {
    d <- mutate(d, across(where(is.character), stringr::str_remove_all, '^"|"$'))
  }

  orig <- names(d)
  harmonized <- snakecase::to_any_case(orig) %>%
    str_replace("^id$", "feat_id") %>%
    str_replace("^parent$", "parent_ids")

  names(d) <- make.unique(c(names(x), harmonized))[-seq_along(names(x))]
  renamed <- tibble(orig, new = names(d))[orig != names(d), ]

  inform(c(
    "Harmonizing attribute names",
    str_glue_data(renamed, "{orig} -> {new}")
  ))

  # make sure these columns always exist in gff-based table
  d <- introduce(d, feat_id = NA_character_, parent_ids = NA_character_, name = NA_character_) %>%
    relocate("feat_id", "parent_ids", "name")

  if (keep_attr) {
    x <- bind_cols(x[, 1:8], d, x[, 9])
  } else {
    x <- bind_cols(x[, 1:8], d)
  }

  if (is_gff2 && has_vars(x, c("transcript_id"))) {
    # make sure this always there
    x <- introduce(x, protein_id = NA_character_) %>%
      dplyr::group_by(.data$type, .data$transcript_id) # need this for numbering exons

    # mRNA feat_id=transcript_id
    # CDS feat_id="cds-"transcript_id|protein_id;parent_ids=transcript_id;
    # exon feat_id="exon-"transcript_id.#;parent_ids=transcript_id;
    x <- dplyr::mutate(x,
      feat_id = ifelse(.data$type == "mRNA" & is.na(.data$feat_id), .data$transcript_id, .data$feat_id),
      feat_id = ifelse(.data$type == "CDS" & is.na(.data$feat_id),
        stringr::str_c("cds-", coalesce(.data$transcript_id, .data$protein_id)), .data$feat_id
      ),
      feat_id = ifelse(.data$type == "exon" & is.na(.data$feat_id),
        stringr::str_c("exon-", .data$transcript_id, "-", row_number()), .data$feat_id
      ),
      parent_ids = ifelse(.data$type %in% c("CDS", "exon") & is.na(.data$parent_ids),
        .data$transcript_id, .data$parent_ids
      )
    ) %>% dplyr::ungroup()
  }

  # make Parent a list col (one feature can have multiple parents)
  x <- mutate(x, parent_ids = str_split(.data$parent_ids, ","))

  # set a dummy feat_id
  x <- mutate(x, feat_id = coalesce(.data$feat_id, paste0("feat_", row_number())))

  # fix augustus CDS
  if (fix_augustus_cds) {
    x <- mutate(x, feat_id = ifelse(.data$type == "CDS", str_replace(.data$feat_id, "CDS\\d+$", "CDS"), .data$feat_id))
  }

  x
}


coords2introns <- function(starts, ends, sort_exons = TRUE) {
  n <- length(starts)
  if (n < 2) {
    return(NULL)
  }

  if (sort_exons && is.unsorted(starts)) {
    o <- order(starts)
    starts <- starts[o]
    ends <- ends[o]
  }

  i <- 2:n
  # introns: start, end, start2, end2, ...
  # +2 corrects of 1[s,e] coord issues
  c(rbind(ends[i - 1] + 2, starts[i])) - starts[1]
}

# guess gff version based on key/value delimiter of 9th column (= for 3, " " for 2)
guess_is_gff2 <- function(x) {
  attr <- x[[9]]
  attr <- attr[which(str_length(attr) > 0)]
  if (length(attr) == 0) {
    rlang::warn("Failed to guess gff version, assuming gff3, overwrite with `is_gff2=TRUE`")
    return(FALSE)
  }

  is_gff2 <- str_match(attr[1], "[= ]") == " "
  if (is.na(is_gff2)) {
    rlang::warn("Failed to guess gff version, assuming gff3, overwrite with `is_gff2=TRUE`")
    return(FALSE)
  }
  is_gff2
}
