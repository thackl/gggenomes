library(gggenomes)

context("contigs")

ctg_df <- tibble(genome_id="A", contig_id="foo", length=10, other="stuff")
test_that("create a tbl_contig", {
  expect_error(as_contigs(data.frame()), "Required")
  expect_is(as_contigs(ctg_df), "tbl_contig")
  expect_is(as_contigs(as.data.frame(ctg_df)), "tbl_contig")
  expect_equivalent(as_contigs(ctg_df, everything=FALSE), tibble(
    .y=1L, .x=0, .xend=10, .strand=1L,
    genome_id="A", contig_id="foo", length=10,
    .gix=1L, .gstrand=1L, .goffset=0,
    .cix=1L, .cstrand=1L, .offset=0))

  expect_equivalent(as_contigs(ctg_df), tibble(
    .y=1L, .x=0, .xend=10, .strand=1L,
    genome_id="A", contig_id="foo", length=10, other="stuff",
    .gix=1L, .gstrand=1L, .goffset=0,
    .cix=1L, .cstrand=1L, .offset=0))

})

context("features")
ctg_tbl <- as_contigs(ctg_df)
feat_df <- tibble(contig_id="foo", start=3, end=6, strand='+', other="stuff")

test_that("create/layout a tbl_feature", {
  expect_error(as_features(feat_df, ctg_df), "expecting")
  feat_tbl <- as_features(feat_df, ctg_tbl)
  expect_is(feat_tbl, "tbl_feature")
  x <- tibble(
    .y=1L, .x=3, .xend=6, .strand=1L,
    genome_id="A", contig_id="foo", start=3, end=6, strand='+', other="stuff",
    .offset=0, .gcstrand=1L)
  expect_equivalent(feat_tbl,x) 
})

context("genomes")
test_that("create/layout a tbl_genome", {
  as_features(feat_df, ctg_tbl)
  as_genomes(contigs=ctg_df, genes=feat_df)

  gggenomes(ctg_df, feat_df) + geom_contig() +
    geom_gene() +
    geom_feature(data=use(genes), nudge_by_strand=.01) 

})

