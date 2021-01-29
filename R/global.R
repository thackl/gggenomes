# Environment that holds various global variables and settings for gggenomes,
# such as known file formats. It is not exported and should not be directly
# manipulated by other packages.
gggenomes_global  <- new.env(parent = emptyenv())

# list of contexts of dictionaryish vectors mapping suffixes to file formats
gggenomes_global$file_formats <- map(list(
  feats = list(
    gff3 = qc(gff, gff3),
    gbk = qc(gbk, gb, gbff),
    bed = qc(bed),
    fasta = qc(fa, fas, fasta, ffn, fna, faa),
    blast = qc(m8, o6, o7),
    paf = qc(paf),
    ambigious = qc(txt, tsv, csv)
  ),
  seqs = list(
    fai = qc(fai),
    seq_len = qc(fa, fas, fasta, ffn, fna, faa, gff, gbk),
    ambigious = qc(txt, tsv, csv)
  ),
  zips = list(
    bz2 = qc(bz, bz2),
    gz = qc(gz),
    xz = qc(xz),
    zip = qc(zip))
), ~deframe(stack(.x) %>% mutate(ind=as.character(ind))))

# Default column names for different formats
gggenomes_global$def_names <- list(
  gff3 = qc(seq_id, source, type, start, end, score, strand, phase, attributes),
  paf =  qc(seq_id, length, start, end, strand, seq_id2, length2, start2, end2,
      map_match, map_length, map_quality),
  blast = qc(seq_id, seq_id2, pident, length, mismatch, gapopen, start, end,
      start2, end2, evalue, bitscore),
  bed = qc(seq_id, start, end, name, score, strand),
  fai = qc(seq_id, seq_desc, length),
  seq_len = qc(seq_id, seq_desc, length)
)

gggenomes_global$def_types <- list(
  gff3 = "ccciiccic",
  paf =  "ciiicciiiiid",
  blast = "ccdiiiiiiidd",
  bed = "ciicdc",
  fai = "ci---",
  seq_len = "cci"
)

