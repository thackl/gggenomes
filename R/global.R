# Environment that holds various global variables and settings for gggenomes,
# such as known file formats. It is not exported and should not be directly
# manipulated by other packages.
gggenomes_global  <- new.env(parent = emptyenv())

# list of families of dictionaryish vectors mapping suffixes to file formats
gggenomes_global$file_formats <- map(list(
  feats = list(
    gff3 = qc(gff, gff3),
    gbk = qc(gbk, gb, gbff),
    bed = qc(bed),
    fasta = qc(fa, fas, fasta, ffn, fna, faa),
    blast = qc(tsv)),
  seqs = list(
    fasta = qc(fa, fas, fasta, ffn, fna, faa),
    fai = qc(fai)),
  zips = list(
    bz2 = qc(bz, bz2),
    gz = qc(gz),
    xz = qc(xz),
    zip = qc(zip))
), ~deframe(stack(.x) %>% mutate(ind=as.character(ind))))
