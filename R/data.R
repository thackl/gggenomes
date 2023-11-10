## Documentations for bundled example data  (/data/*.rda)

#' Sequence index of 6 EMALE genomes (endogenous virophages)
#'
#' A data set containing the sequence information on 6 endogenous virophages
#' found in the genomes of the marine protist *Cafeteria burkhardae*.
#'
#' @format A data frame with 6 rows and 4 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{seq_id}{sequence identifier}
#'   \item{seq_desc}{sequence description}
#'   \item{length}{length of the sequence}
#' }
#' @source
#' * Publication: \url{http://dx.doi.org/10.1101/2020.11.30.404863}
#' * Raw data: \url{https://github.com/thackl/cb-emales}
#' * Derived & bundled data: `ex("emales/emales.fna")`
"emale_seqs"

#' Gene annotations if 6 EMALE genomes (endogenous virophages)
#'
#' A data set containing gene feature annotations for 6 endogenous virophages
#' found in the genomes of the marine protist *Cafeteria burkhardae*.
#'
#' @format A data frame with 143 rows and 17 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{seq_id}{identifier of the sequence the feature appears on}
#'   \item{start}{start of the feature on the sequence}
#'   \item{end}{end of the feature on the sequence}
#'   \item{strand}{reading orientation relative to sequence (+ or -)}
#'   \item{type}{feature type (CDS, mRNA, gene, ...)}
#'   \item{feat_id}{unique identifier of the feature}
#'   \item{introns}{a list column with internal intron start/end positions}
#'   \item{parent_ids}{a list column with parent IDs - feat_id's of parent features}
#'   \item{source}{source of the annotation}
#'   \item{score}{score of the annotation}
#'   \item{phase}{For "CDS" features indicates where the next codon begins relative to the 5' start}
#'   \item{width}{width of the feature}
#'   \item{gc_content}{relative GC-content of the feature}
#'   \item{name}{name of the feature}
#'   \item{Note}{}
#'   \item{geom_id}{an identifier telling the which features should be plotted as on items (usually CDS and mRNA of same gene)}
#' }
#' @source
#' * Publication: \url{http://dx.doi.org/10.1101/2020.11.30.404863}
#' * Raw data: \url{https://github.com/thackl/cb-emales}
#' * Derived & bundled data: `ex("emales/emales.gff")`
"emale_genes"

#' Terminal inverted repeats of 6 EMALE genomes
#'
#' @format A data frame with 3 rows and 14 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{seq_id}{identifier of the sequence the feature appears on}
#'   \item{start}{start of the feature on the sequence}
#'   \item{end}{end of the feature on the sequence}
#'   \item{strand}{reading orientation relative to sequence (+ or -)}
#'   \item{type}{feature type (CDS, mRNA, gene, ...)}
#'   \item{feat_id}{unique identifier of the feature}
#'   \item{introns}{a list column with internal intron start/end positions}
#'   \item{parent_ids}{a list column with parent IDs - feat_id's of parent features}
#'   \item{source}{source of the annotation}
#'   \item{score}{score of the annotation}
#'   \item{phase}{For "CDS" features indicates where the next codon begins relative to the 5' start}
#'   \item{name}{name of the feature}
#'   \item{width}{end-start+1}
#'   \item{geom_id}{an identifier telling the which features should be plotted as on items (usually CDS and mRNA of same gene)}
#' }
#' @source
#' * Publication: \url{http://dx.doi.org/10.1101/2020.11.30.404863}
#' * Raw data: \url{https://github.com/thackl/cb-emales}
#' * Derived & bundled data: `ex("emales/emales-tirs.gff")`
"emale_tirs"

#' Integrated Ngaro retrotransposons of 6 EMALE genomes
#'
#' @format A data frame with 3 rows and 14 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{seq_id}{identifier of the sequence the feature appears on}
#'   \item{start}{start of the feature on the sequence}
#'   \item{end}{end of the feature on the sequence}
#'   \item{strand}{orientation of the feature relative to the sequence (+ or -)}
#'   \item{type}{feature type (CDS, mRNA, gene, ...)}
#'   \item{feat_id}{unique identifier of the feature}
#'   \item{introns}{a list column with internal intron start/end positions}
#'   \item{parent_ids}{a list column with parent IDs - feat_id's of parent features}
#'   \item{source}{source of the annotation}
#'   \item{score}{score of the annotation}
#'   \item{phase}{For "CDS" features indicates where the next codon begins relative to the 5' start}
#'   \item{name}{name of the feature}
#'   \item{geom_id}{an identifier telling the which features should be plotted as on items (usually CDS and mRNA of same gene)}
#' }
#' @source
#' * Publication: \url{http://dx.doi.org/10.1101/2020.11.30.404863}
#' * Raw data: \url{https://github.com/thackl/cb-emales}
#' * Derived & bundled data: `ex("emales/emales-ngaros.gff")`
"emale_ngaros"

#' Relative GC-content along 6 EMALE genomes
#'
#' One row per 50 bp window.
#'
#' @format A data frame with 2856 rows and 6 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{seq_id}{identifier of the sequence the feature appears on}
#'   \item{start}{start of the feature on the sequence}
#'   \item{end}{end of the feature on the sequence}
#'   \item{name}{name of the feature}
#'   \item{score}{relative GC-content of the window}
#' }
#' @source
#' * Derived & bundled data: `ex("emales/emales-gc.bed")`
"emale_gc"

#' All-versus-all whole genome alignments of 6 EMALE genomes
#'
#' One row per alignment block. Alignments were computed with minimap2.
#'
#' @format A data frame with 125 rows and 23 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{seq_id}{identifier of the sequence the feature appears on}
#'   \item{length}{length of the sequence}
#'   \item{start}{start of the feature on the sequence}
#'   \item{end}{end of the feature on the sequence}
#'   \item{strand}{orientation of the feature relative to the sequence (+ or -)}
#'   \item{seq_id2}{identifier of the sequence the feature appears on}
#'   \item{length2}{length of the sequence}
#'   \item{start2}{start of the feature on the sequence}
#'   \item{end2}{end of the feature on the sequence}
#'   \item{...}{see \url{https://github.com/lh3/miniasm/blob/master/PAF.md} for additional columns}
#' }
#' @source
#' * Derived & bundled data: `ex("emales/emales.paf")`
"emale_ava"

#' All-versus-all alignments 6 EMALE proteomes
#'
#' One row per alignment. Alignments were computed with mmseqs2 (blast-like).
#'
#' @format A data frame with 827 rows and 13 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{feat_id}{identifier of the first feature in the alignment}
#'   \item{feat_id2}{identifier of the second feature in the alignment}
#'   \item{pident, ...}{see \url{https://github.com/seqan/lambda/wiki/BLAST-Output-Formats} for BLAST-tabular format columns}
#' }
#' @source
#' * Derived & bundled data: `ex("emales/emales-prot-ava.o6")`
"emale_prot_ava"

#' Alignments of 6 EMALE proteomes against Uniref50
#'
#' One row per alignment. Alignments were computed with mmseqs2 (blast-like), and
#' filtered by evalue (<1e-20).
#'
#' @format A data frame with 509 rows and 16 columns
#' \describe{
#'   \item{file_id}{name of the file the data was read from}
#'   \item{feat_id}{identifier of the first feature in the alignment}
#'   \item{feat_id2}{identifier of the second feature in the alignment}
#'   \item{pident, ...}{see \url{https://github.com/seqan/lambda/wiki/BLAST-Output-Formats} for BLAST-tabular format columns}
#'   \item{seq_head}{full sequence header of the emale protein}
#'   \item{seq_head2}{full sequence header of the Uniref50 protein}
#'   \item{taxname}{one of the 4 most abundant taxonomic names among the Uniref50 hits or NA}
#' }
#' @source
#' * Derived & bundled data: `ex("emales/emales-prot-uniref50.tsv")`
"emale_prot_uniref50"

#' Clusters of orthologs of 6 EMALE proteomes
#'
#' One row per feature. Clusters are based on manual curation.
#'
#' @format A data frame with 48 rows and 3 columns
#' \describe{
#'   \item{cluster_id}{identifier of the cluster}
#'   \item{feat_id}{identifer of the gene}
#'   \item{cluster_size}{number of features in the cluster}
#' }
#' @source
#' * Derived & bundled data: `ex("emales/emales-cogs.tsv")`
"emale_cogs"
