#' Read AliTV .json file
#'
#' this file contains sequences, links and (optionally) genes
#'
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr unnest
#' @importFrom jsonlite fromJSON
#' @param file path to json
#' @export
#' @return list with seqs, genes, and links
#' @examples
#' ali <- read_alitv("https://alitvteam.github.io/AliTV/d3/data/chloroplasts.json")
#' gggenomes(ali$genes, ali$seqs, links = ali$links) +
#'   geom_seq() +
#'   geom_bin_label() +
#'   geom_gene(aes(fill = class)) +
#'   geom_link()
#' p <- gggenomes(ali$genes, ali$seqs, links = ali$links) +
#'   geom_seq() +
#'   geom_bin_label() +
#'   geom_gene(aes(color = class)) +
#'   geom_link(aes(fill = identity)) +
#'   scale_fill_distiller(palette = "RdYlGn", direction = 1)
#' p %>%
#'   flip_seqs(5) %>%
#'   pick_seqs(1, 3, 2, 4, 5, 6, 7, 8)
read_alitv <- function(file) {
  ali <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
  seqs <- tibble(seq = ali$data$karyo$chromosome) %>%
    mutate(seq_id = names(seq)) %>%
    unnest_wider(seq) %>%
    rename(bin_id = .data$genome_id)
  genes <- tibble(feature = ali$data$feature) %>%
    mutate(class = names(.data$feature)) %>%
    filter(class != "link") %>%
    unnest(.data$feature) %>%
    rename(seq_id = .data$karyo)
  links <- tibble(links = ali$data$links) %>%
    unnest(links) %>%
    unnest(links) %>%
    unnest_wider(links)
  link_pos <- tibble(link = ali$data$features$link) %>%
    mutate(id = names(.data$link)) %>%
    unnest_wider(.data$link)
  links <- links %>%
    left_join(link_pos, by = c("source" = "id")) %>%
    left_join(link_pos, by = c("target" = "id")) %>%
    transmute(
      seq_id = .data$karyo.x,
      start = .data$start.x,
      end = .data$end.x,
      seq_id2 = .data$karyo.y,
      start2 = .data$start.y,
      end2 = .data$end.y,
      identity = identity
    )
  return(list(seqs = seqs, genes = genes, links = links))
}

read_alitv_seqs <- function(...) read_alitv(...)$seqs
read_alitv_genes <- function(...) read_alitv(...)$genes
read_alitv_links <- function(...) read_alitv(...)$links
