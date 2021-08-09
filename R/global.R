# Environment that holds various global variables and settings for gggenomes,
# such as known file formats. It is not exported and should not be directly
# manipulated by other packages.
gggenomes_global  <- new.env(parent = emptyenv())

# Mapping of file formats, extensions, contexts, and parsers
#
# parser is a function name, like "read_tsv", ...
#
# context=NA defines fallback parser that is used if no parser is defined for
# the specific context
gggenomes_global$def_formats <- tribble(
  ~format, ~ext, ~context, ~parser,
  "ambigious", qc(txt,tsv,csv), NA, "read_ambigious",
  "fasta", qc(fa,fas,fasta,ffn,fna,faa), qc(seqs), qc(read_seq_len),
  "fai", qc(fai), qc(seqs), qc(read_fai),
  "gff3", qc(gff,gff3,gff2,gtf), qc(feats, seqs), qc(read_gff3, read_seq_len),
  "gbk", qc(gbk,gb,gbff,gpff), qc(feats, seqs), qc(read_gbk, read_seq_len),
  "bed", qc(bed), "feats", "read_bed",
  "blast", qc(m8,o6,o7), qc(feats, links), qc(read_blast, read_blast),
  "paf", qc(paf), qc(feats, links), qc(read_paf, read_paf),
  "alitv", qc(json), qc(feats, seqs, links),
     qc(read_alitv_genes, read_alitv_seqs, read_alitv_links)
)

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
  gff3 = "ccciicccc",
  paf =  "ciiicciiiiid",
  blast = "ccdiiiiiiidd",
  bed = "ciicdc",
  fai = "ci---",
  seq_len = "cci"
)

