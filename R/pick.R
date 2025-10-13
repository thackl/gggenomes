#' Pick bins and seqs by name or position
#'
#' Pick which bins and seqs to show and in what order. Uses
#' [dplyr::select()]-like syntax, which means unquoted genome names, positional
#' arguments and [selection
#' helpers](https://tidyselect.r-lib.org/reference/language.html), such as
#' [tidyselect::starts_with()] are supported. Renaming is not supported.
#'
#' Use the dots to select bins or sequences (depending on function suffix), and
#' the `.bins` argument to set the scope for positional arguments. For example,
#' `pick_seqs(1)` will pick the first sequence from the first bin, while
#' `pick_seqs(1, .bins=3)` will pick the first sequence from the third bin.
#' @examples
#' s0 <- tibble::tibble(
#'   bin_id = c("A", "B", "B", "B", "C", "C", "C"),
#'   seq_id = c("a1", "b1", "b2", "b3", "c1", "c2", "c3"),
#'   length = c(1e4, 6e3, 2e3, 1e3, 3e3, 3e3, 3e3)
#' )
#'
#' p <- gggenomes(seqs = s0) + geom_seq(aes(color = bin_id), size = 3) +
#'   geom_bin_label() + geom_seq_label() +
#'   expand_limits(color = c("A", "B", "C"))
#' p
#'
#' # remove
#' p %>% pick(-B)
#'
#' # select and reorder, by ID and position
#' p %>% pick(C, 1)
#'
#' # use helper function
#' p %>% pick(starts_with("B"))
#'
#' # pick just some seqs
#' p %>% pick_seqs(1, c3)
#'
#' # pick with .bin scope
#' p %>% pick_seqs(3:1, .bins = C)
#'
#' # change seqs in some bins, but keep rest as is
#' p %>% pick_seqs_within(3:1, .bins = B)
#'
#' # same w/o scope, unaffected bins remain as is
#' p %>% pick_seqs_within(b3, b2, b1)
#'
#' # Align sequences with and plot next to a phylogenetic tree
#' library(patchwork) # arrange multiple plots
#' library(ggtree) # plot phylogenetic trees
#'
#' # load and plot a phylogenetic tree
#' emale_mcp_tree <- read.tree(ex("emales/emales-MCP.nwk"))
#' t <- ggtree(emale_mcp_tree) + geom_tiplab(align = TRUE, size = 3) +
#'   xlim(0, 0.05) # make room for labels
#'
#' try({ # can fail on older systems with older ggtree versions
#'
#' p <- gggenomes(seqs = emale_seqs, genes = emale_genes) +
#'   geom_seq() + geom_seq() + geom_bin_label()
#'
#' # plot next to each other, but with
#' # different order in tree and genomes
#' t + p + plot_layout(widths = c(1, 5))
#'
#' # reorder genomes to match tree order
#' # with a warning caused by mismatch in y-scale expansions
#' t + p %>% pick_by_tree(t) + plot_layout(widths = c(1, 5))
#'
#' # extra genomes are dropped with a notification
#' emale_seqs_more <- emale_seqs
#' emale_seqs_more[7, ] <- emale_seqs_more[6, ]
#' emale_seqs_more$seq_id[7] <- "One more genome"
#' p <- gggenomes(seqs = emale_seqs_more, genes = emale_genes) +
#'   geom_seq() + geom_seq() + geom_bin_label()
#' t + p %>% pick_by_tree(t) + plot_layout(widths = c(1, 5))
#'
#' # no shared ids will cause an error
#' p <- gggenomes(seqs = tibble::tibble(seq_id = "foo", length = 1)) +
#'   geom_seq() + geom_seq() + geom_bin_label()
#' t + p %>% pick_by_tree(t) + plot_layout(widths = c(1, 5))
#'
#' # extra leafs in tree will cause an error
#' emale_seqs_fewer <- slice_head(emale_seqs, n = 4)
#' p <- gggenomes(seqs = emale_seqs_fewer, genes = emale_genes) +
#'   geom_seq() + geom_seq() + geom_bin_label()
#' t + p %>% pick_by_tree(t) + plot_layout(widths = c(1, 5))
#'
#' })
#'
#' @describeIn pick pick bins by bin_id, positional argument (start at top)
#'   or select-helper.
#' @param x gggenomes object
#' @param ... bins/seqs to pick, select-like expression.
#' @return gggenomes object with selected bins and seqs.
#' @export
pick <- function(x, ...) {
  if (...length() == 0) {
    return(x)
  }
  pick_impl(x, .bins = c(...))
}

#' @describeIn pick pick individual seqs seq_id, positional argument (start at
#'   top left) or select-helper.
#' @param .bins scope for positional arguments, select-like expression, enclose
#'   multiple arguments with `c()`!.
#' @return gggenomes object with selected seqs.
#' @export
pick_seqs <- function(x, ..., .bins = everything()) {
  if (...length() == 0) {
    return(x)
  }
  pick_impl(x, ..., .bins = {{ .bins }}, .seqs_within = FALSE)
}

#' @describeIn pick pick individual seqs but only modify bins containing those
#'   seqs, keep rest as is.
#' @param .bins scope for positional arguments, select-like expression, enclose
#'   multiple arguments with `c()`!
#' @return gggenomes object with selected seqs.
#' @export
pick_seqs_within <- function(x, ..., .bins = everything()) {
  if (...length() == 0) {
    return(x)
  }
  pick_impl(x, ..., .bins = {{ .bins }}, .seqs_within = TRUE)
}

#' @describeIn pick align bins with the leaves in a given phylogenetic tree.
#' @param tree a phylogenetic tree in [ggtree::ggtree] or [`ape::ape-package`]-"phylo" format.
#' @param infer_bin_id an expression to extract bin_ids from the tree data.
#' @return gggenomes object with seqs selected by tree order.
#' @export
pick_by_tree <- function(x, tree, infer_bin_id = .data$label) {
  if (!requireNamespace("ggtree", quietly = TRUE)) {
    abort("ggtree must be installed to use this function")
  }

  if (inherits(tree, "phylo")) tree <- ggtree::ggtree(tree)
  tree_ids <- tree$data %>%
    filter(.data$isTip) %>%
    arrange(-.data$y) %>%
    transmute(bin_id = {{ infer_bin_id }}) %>%
    pull("bin_id")

  # check ID matches
  bin_ids <- get_seqs(x)$bin_id
  tree_in_bins <- tree_ids %in% bin_ids
  bins_in_tree <- bin_ids %in% tree_ids
  if (!any(tree_in_bins)) {
    abort("No shared bin_ids between tree and genomes. Check your IDs.")
  }
  if (!all(tree_in_bins)) {
    abort(c(
      "Some bin_ids only exist in the tree. Please drop those first.",
      str_glue("{tree_ids[!tree_in_bins]}")
    ))
  }
  if (any(!bins_in_tree)) {
    inform(c(
      "Some bin_ids are missing in the tree, will drop those from genomes.",
      str_glue("{bin_ids[!bins_in_tree]}")
    ))
  }

  # check if y scales have matching expansions
  tree_exp <- tree$scales$scales[[1]]$expand %||% NA
  bins_exp <- x$scales$scales[[1]]$expand %||% NA
  if (!isTRUE(all(tree_exp == bins_exp))) {
    warn(c(
      str_glue(
        "Tree and genomes have different y-scale expansions.",
        " This can cause slight misalignments of leaves and sequences.",
        "\nConsider adding `+ scale_y_continuous(expand=c({comma(bins_exp)}))`",
        " to the tree as a fix"
      ),
      str_glue("tree: {comma(tree_exp)}"),
      str_glue("bins: {comma(bins_exp)}")
    ))
  }

  pick(x, all_of(tree_ids))
}

pick_impl <- function(x, ..., .bins = everything(), .seqs_within = FALSE) {
  # split by bin_id and select bins
  s <- get_seqs(x)
  l <- s %>% split_by(.data$bin_id)
  i <- tidyselect::eval_select(expr({{ .bins }}), l)
  if (length(i) == 0) rlang::abort("no bins selected")
  s <- bind_rows(l[i])

  # pick seqs from bins
  if (...length() > 0) {
    seq_ids <- s$seq_id %>% set_names(.)
    j <- tidyselect::eval_select(expr(c(...)), seq_ids)
    s <- s[j, ]
    if (isTRUE(.seqs_within)) { # splice modified bins into rest
      m <- s %>% split_by(.data$bin_id)
      l[names(m)] <- m
      s <- bind_rows(l)
    }
  }

  x <- set_seqs(x, s)
  layout(x)
}
