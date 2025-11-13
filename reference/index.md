# Package index

## Manipulate your plot & data

Set up the plot, rearrange stuff, zoom in on what matters

- [`gggenomes()`](https://thackl.github.io/gggenomes/reference/gggenomes.md)
  : Plot genomes, features and synteny maps
- [`pick()`](https://thackl.github.io/gggenomes/reference/pick.md)
  [`pick_seqs()`](https://thackl.github.io/gggenomes/reference/pick.md)
  [`pick_seqs_within()`](https://thackl.github.io/gggenomes/reference/pick.md)
  [`pick_by_tree()`](https://thackl.github.io/gggenomes/reference/pick.md)
  : Pick bins and seqs by name or position
- [`flip()`](https://thackl.github.io/gggenomes/reference/flip.md)
  [`flip_seqs()`](https://thackl.github.io/gggenomes/reference/flip.md)
  [`sync()`](https://thackl.github.io/gggenomes/reference/flip.md) :
  Flip bins and sequences
- [`shift()`](https://thackl.github.io/gggenomes/reference/shift.md) :
  Shift bins left/right
- [`align()`](https://thackl.github.io/gggenomes/reference/align.md) :
  Align genomes relative to target genes, feats, seqs, etc.
- [`focus()`](https://thackl.github.io/gggenomes/reference/focus.md)
  [`locate()`](https://thackl.github.io/gggenomes/reference/focus.md) :
  Show features and regions of interest

## Draw all those pretty things

- [`geom_seq()`](https://thackl.github.io/gggenomes/reference/geom_seq.md)
  : draw seqs
- [`geom_gene()`](https://thackl.github.io/gggenomes/reference/geom_gene.md)
  : Draw gene models
- [`geom_feat()`](https://thackl.github.io/gggenomes/reference/geom_feat.md)
  : Draw feats
- [`geom_link()`](https://thackl.github.io/gggenomes/reference/geom_link.md)
  [`geom_link_curved()`](https://thackl.github.io/gggenomes/reference/geom_link.md)
  [`geom_link_line()`](https://thackl.github.io/gggenomes/reference/geom_link.md)
  : Draw links between genomes
- [`geom_coverage()`](https://thackl.github.io/gggenomes/reference/geom_wiggle.md)
  [`geom_wiggle()`](https://thackl.github.io/gggenomes/reference/geom_wiggle.md)
  : Draw wiggle ribbons or lines
- [`geom_feat_text()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
  [`geom_feat_tag()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
  [`geom_feat_note()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
  [`geom_gene_text()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
  [`geom_gene_tag()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
  [`geom_gene_note()`](https://thackl.github.io/gggenomes/reference/geom_feat_text.md)
  : Add text to genes, features, etc.
- [`geom_bin_label()`](https://thackl.github.io/gggenomes/reference/geom_bin_label.md)
  : Draw bin labels
- [`geom_gene_label()`](https://thackl.github.io/gggenomes/reference/geom_gene_label.md)
  [`geom_feat_label()`](https://thackl.github.io/gggenomes/reference/geom_gene_label.md)
  [`geom_link_label()`](https://thackl.github.io/gggenomes/reference/geom_gene_label.md)
  : Draw feat/link labels
- [`geom_seq_break()`](https://thackl.github.io/gggenomes/reference/geom_seq_break.md)
  : Decorate truncated sequences
- [`geom_seq_label()`](https://thackl.github.io/gggenomes/reference/geom_seq_label.md)
  : Draw seq labels
- [`geom_variant()`](https://thackl.github.io/gggenomes/reference/geom_variant.md)
  : Draw place of mutation
- [`position_strand()`](https://thackl.github.io/gggenomes/reference/position_strand.md)
  [`position_pile()`](https://thackl.github.io/gggenomes/reference/position_strand.md)
  [`position_strandpile()`](https://thackl.github.io/gggenomes/reference/position_strand.md)
  [`position_sixframe()`](https://thackl.github.io/gggenomes/reference/position_strand.md)
  : Stack features
- [`position_variant()`](https://thackl.github.io/gggenomes/reference/position_variant.md)
  : Plot types of mutations with different offsets
- [`scale_x_bp()`](https://thackl.github.io/gggenomes/reference/scale_x_bp.md)
  [`label_bp()`](https://thackl.github.io/gggenomes/reference/scale_x_bp.md)
  : X-scale for genomic data
- [`scale_color_variant()`](https://thackl.github.io/gggenomes/reference/scale_color_variant.md)
  [`scale_shape_variant()`](https://thackl.github.io/gggenomes/reference/scale_color_variant.md)
  : Default colors and shapes for mutation types.
- [`theme_gggenomes_clean()`](https://thackl.github.io/gggenomes/reference/theme_gggenomes_clean.md)
  : gggenomes default theme

## Read data from common files

- [`read_feats()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)
  [`read_subfeats()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)
  [`read_links()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)
  [`read_sublinks()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)
  [`read_seqs()`](https://thackl.github.io/gggenomes/reference/read_tracks.md)
  : Read files in various standard formats (FASTA, GFF3, GBK, BED,
  BLAST, ...) into track tables
- [`read_alitv()`](https://thackl.github.io/gggenomes/reference/read_alitv.md)
  : Read AliTV .json file
- [`read_bed()`](https://thackl.github.io/gggenomes/reference/read_bed.md)
  : Read a BED file
- [`read_blast()`](https://thackl.github.io/gggenomes/reference/read_blast.md)
  : Read BLAST tab-separated output
- [`read_context()`](https://thackl.github.io/gggenomes/reference/read_context.md)
  : Read files in different contexts
- [`read_gbk()`](https://thackl.github.io/gggenomes/reference/read_gbk.md)
  : Read genbank files
- [`read_gff3()`](https://thackl.github.io/gggenomes/reference/read_gff3.md)
  : Read features from GFF3 (and with some limitations GFF2/GTF) files
- [`read_paf()`](https://thackl.github.io/gggenomes/reference/read_paf.md)
  : Read a .paf file (minimap/minimap2).
- [`read_seq_len()`](https://thackl.github.io/gggenomes/reference/read_seq_len.md)
  [`read_fai()`](https://thackl.github.io/gggenomes/reference/read_seq_len.md)
  : Read sequence index
- [`read_vcf()`](https://thackl.github.io/gggenomes/reference/read_vcf.md)
  : Read a VCF file
- [`swap_query()`](https://thackl.github.io/gggenomes/reference/swap_query.md)
  : Swap query and subject in blast-like feature tables
- [`ex()`](https://thackl.github.io/gggenomes/reference/ex.md) : Get
  path to gggenomes example files
- [`def_formats()`](https://thackl.github.io/gggenomes/reference/def_formats.md)
  : Defined file formats and extensions
- [`def_names()`](https://thackl.github.io/gggenomes/reference/def_names.md)
  [`def_types()`](https://thackl.github.io/gggenomes/reference/def_names.md)
  : Default column names and types for defined formats
- [`write_gff3()`](https://thackl.github.io/gggenomes/reference/write_gff3.md)
  : Write a gff3 file from a tidy table

## Work with gggenomes’ data tracks

- [`track_info()`](https://thackl.github.io/gggenomes/reference/track_info.md)
  : Basic info on tracks in a gggenomes object

- [`add_seqs()`](https://thackl.github.io/gggenomes/reference/add_seqs.md)
  : Add seqs

- [`add_feats()`](https://thackl.github.io/gggenomes/reference/add_tracks.md)
  [`add_links()`](https://thackl.github.io/gggenomes/reference/add_tracks.md)
  [`add_subfeats()`](https://thackl.github.io/gggenomes/reference/add_tracks.md)
  [`add_sublinks()`](https://thackl.github.io/gggenomes/reference/add_tracks.md)
  [`add_clusters()`](https://thackl.github.io/gggenomes/reference/add_tracks.md)
  : Add different types of tracks

- [`feats()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`feats0()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`genes()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`links()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`seqs()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`bins()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`track()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`pull_feats()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`pull_genes()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`pull_links()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`pull_seqs()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`pull_bins()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  [`pull_track()`](https://thackl.github.io/gggenomes/reference/pull_track.md)
  :

  Use tracks inside and outside `geom_*` calls

## Take a look under the hood

Functions that power gggenomes under the hood. Useful for developers and
power-users.

- [`layout()`](https://thackl.github.io/gggenomes/reference/layout.md) :
  Re-layout a genome layout
- [`layout_genomes()`](https://thackl.github.io/gggenomes/reference/layout_genomes.md)
  : Layout genomes
- [`layout_seqs()`](https://thackl.github.io/gggenomes/reference/layout_seqs.md)
  : Layout sequences
- [`get_seqs()`](https://thackl.github.io/gggenomes/reference/get_seqs.md)
  [`set_seqs()`](https://thackl.github.io/gggenomes/reference/get_seqs.md)
  : Get/set the seqs track
- [`track_ids()`](https://thackl.github.io/gggenomes/reference/track_ids.md)
  : Named vector of track ids and types
- [`track_info()`](https://thackl.github.io/gggenomes/reference/track_info.md)
  : Basic info on tracks in a gggenomes object
- [`require_vars()`](https://thackl.github.io/gggenomes/reference/require_vars.md)
  : Require variables in an object
- [`vars_track()`](https://thackl.github.io/gggenomes/reference/vars_track.md)
  : Tidyselect track variables
- [`swap_if()`](https://thackl.github.io/gggenomes/reference/swap_if.md)
  : Swap values of two columns based on a condition
- [`split_by()`](https://thackl.github.io/gggenomes/reference/split_by.md)
  : Split by key preserving order
- [`in_range()`](https://thackl.github.io/gggenomes/reference/in_range.md)
  : Do numeric values fall into specified ranges?
- [`width()`](https://thackl.github.io/gggenomes/reference/width.md)
  [`width0()`](https://thackl.github.io/gggenomes/reference/width.md) :
  The width of a range
- [`introduce()`](https://thackl.github.io/gggenomes/reference/introduce.md)
  : Introduce non-existing columns
- [`qw()`](https://thackl.github.io/gggenomes/reference/qw.md)
  [`qc()`](https://thackl.github.io/gggenomes/reference/qw.md) : Create
  a vector from unquoted words.
- [`GeomFeatText`](https://thackl.github.io/gggenomes/reference/GeomFeatText.md)
  : Geom for feature text
- [`check_strand()`](https://thackl.github.io/gggenomes/reference/check_strand.md)
  : Check strand
- [`combine_strands()`](https://thackl.github.io/gggenomes/reference/combine_strands.md)
  : Combine strands
- [`drop_feat_layout()`](https://thackl.github.io/gggenomes/reference/drop_feat_layout.md)
  : Drop feature layout
- [`drop_layout()`](https://thackl.github.io/gggenomes/reference/drop_layout.md)
  : Drop a genome layout
- [`drop_link_layout()`](https://thackl.github.io/gggenomes/reference/drop_link_layout.md)
  : Drop a link layout
- [`drop_seq_layout()`](https://thackl.github.io/gggenomes/reference/drop_seq_layout.md)
  : Drop a seq layout
- [`flip_strand()`](https://thackl.github.io/gggenomes/reference/flip_strand.md)
  : Flip strand
- [`if_reverse()`](https://thackl.github.io/gggenomes/reference/if_reverse.md)
  : Vectorised if_else based on strandedness
- [`is_reverse()`](https://thackl.github.io/gggenomes/reference/is_reverse.md)
  : Check whether strand is reverse
- [`strand_chr()`](https://thackl.github.io/gggenomes/reference/strand_chr.md)
  : Convert strand to character
- [`strand_int()`](https://thackl.github.io/gggenomes/reference/strand_int.md)
  : Convert strand to integer
- [`strand_lgl()`](https://thackl.github.io/gggenomes/reference/strand_lgl.md)
  : Convert strand to logical
- [`unnest_exons()`](https://thackl.github.io/gggenomes/reference/unnest_exons.md)
  : Unnest exons
- [`set_class()`](https://thackl.github.io/gggenomes/reference/set_class.md)
  [`add_class()`](https://thackl.github.io/gggenomes/reference/set_class.md)
  [`strip_class()`](https://thackl.github.io/gggenomes/reference/set_class.md)
  : Modify object class attributes

## Data sets

- [`emale_ava`](https://thackl.github.io/gggenomes/reference/emale_ava.md)
  : All-versus-all whole genome alignments of 6 EMALE genomes
- [`emale_cogs`](https://thackl.github.io/gggenomes/reference/emale_cogs.md)
  : Clusters of orthologs of 6 EMALE proteomes
- [`emale_gc`](https://thackl.github.io/gggenomes/reference/emale_gc.md)
  : Relative GC-content along 6 EMALE genomes
- [`emale_genes`](https://thackl.github.io/gggenomes/reference/emale_genes.md)
  : Gene annotations if 6 EMALE genomes (endogenous virophages)
- [`emale_ngaros`](https://thackl.github.io/gggenomes/reference/emale_ngaros.md)
  : Integrated Ngaro retrotransposons of 6 EMALE genomes
- [`emale_prot_ava`](https://thackl.github.io/gggenomes/reference/emale_prot_ava.md)
  : All-versus-all alignments 6 EMALE proteomes
- [`emale_seqs`](https://thackl.github.io/gggenomes/reference/emale_seqs.md)
  : Sequence index of 6 EMALE genomes (endogenous virophages)
- [`emale_tirs`](https://thackl.github.io/gggenomes/reference/emale_tirs.md)
  : Terminal inverted repeats of 6 EMALE genomes
