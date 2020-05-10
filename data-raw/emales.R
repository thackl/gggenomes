library(tidyverse)
library(devtools)
devtools::load_all("~/Code/projects/gggenomes")
library(thacklr)

emale_seqs <- read_fai("emales/cr-emales-33.fna") %>%
  extract(seq_desc, into = c("emale_type", "is_typespecies"),
          "=(\\S+) \\S+=(\\S+)", remove=F, convert=T) %>%
  arrange(emale_type, length)
usethis::use_data(emale_seqs, overwrite=TRUE)

emale_genes <- read_gff("emales/cr-emales-33-prodigal.gff") %>%
  dplyr::rename(feature_id=ID)
usethis::use_data(emale_genes, overwrite=TRUE)

# minimap2 -X -N 50 -p 0.1 -c cr-emales-33.fna cr-emales-33.fna \
# >cr-emales-33.paf
emale_links <- read_paf("emales/cr-emales-33.paf")
usethis::use_data(emale_links, overwrite=TRUE)

# seqkit split -i cr-emales-33.fna                                  
# for fna in `ls cr-emales-33.fna.split/*.fna`; do
#   minimap2 -c -B5 -O6 -E3 --rev-only $fna $fna > $fna.paf;
# done;
# cat cr-emales-33.fna.split/*.paf > tir.paf
emale_tirs_paf <- read_paf("emales/cr-emales-33-tirs.paf") %>%
  filter(seq_id1 == seq_id2 & start1 < start2 & map_length > 99 & de < 0.1)
emale_tirs <- bind_rows(
  select(emale_tirs_paf, seq_id=seq_id1, start=start1, end=end1, de),
  select(emale_tirs_paf, seq_id=seq_id2, start=start2, end=end2, de))
usethis::use_data(emale_tirs, overwrite=TRUE)

# manual annotations by MFG
emale_transposons <- read_gff("emales/cr-emales-33.gff",
                              types = c("mobile_element"))
usethis::use_data(emale_transposons, overwrite=TRUE)

# seq-gc -Nbw 50 cr-emales-33.fna > cr-emales-33-gc.tsv
emale_gc <- thacklr::read_bed("emales/cr-emales-33-gc.tsv") %>%
  rename(seq_id=contig_id)
usethis::use_data(emale_gc, overwrite=TRUE)

# gff2cds --aa --type CDS --source Prodigal_v2.6.3 --fna cr-emales-33.fna \
# cr-emales-33-prodigal.gff > cr-emales-33-prodigal.faa
# blastp -query cr-emales-33-prodigal.faa -subject mavirus.faa -outfmt 7 \
# >cr-emales-33_mavirus-blastp.tsv
emale_blast <- read_blast("emales/cr-emales-33_mavirus-blastp.tsv")
emale_blast %<>%
  filter(evalue < 1e-3) %>%
  select(feature_id=qaccver, start=qstart, end=qend, saccver) %>%
  left_join(read_tsv("emales/mavirus.tsv", col_names = c("saccver", "blast_hit", "blast_desc")))
usethis::use_data(emale_blast, overwrite=TRUE)

# mmseqs easy-cluster cr-emales-33-prodigal.faa cr-emales-33-mmseqs /tmp -e 1e-5 -c 0.7;
# cluster-ids -t "cog%03d" < cr-emales-33-mmseqs_cluster.tsv > cr-emales-33-cogs.tsv
emale_cogs <- read_tsv("emales/cr-emales-33-cogs.tsv", col_names = c("feature_id", "cluster_id", "cluster_n"))
emale_cogs %<>% mutate(
  cluster_label = paste0(cluster_id, " (", cluster_n, ")"),
  cluster_label = fct_lump_min(cluster_label, 5, other_level = "rare"),
  cluster_label = fct_lump_min(cluster_label, 15, other_level = "medium"),
  cluster_label = fct_relevel(cluster_label, "rare", after=Inf))
usethis::use_data(emale_cogs, overwrite=TRUE)
emale_cogs


