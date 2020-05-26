#' Use a specific track table inside a `geom_*` call.
#'
#' Use these function inside `geom_*` calls to set the track table of the
#' gggenomes layout you want to use, e.g. seqs, features, links etc. Same
#' semantics as for `pull_*()` apply. `use_genes()` works on feature tracks, but
#' augments the data to play nicer with `geom_gene()`. `use_bins()` returns a
#' binwise summary of the seqs data, and powers `geom_bin_*()` calls. `...`
#' allow you to pass on filter arguments to subset the data.
#' @inheritParams pull_features
#' @export
use_seqs <- function(...){
  dots <- quos(...)
  function(.x, ...){
    pull_seqs(.x, !!! dots)
  }
}
#' @rdname use_seqs
#' @export
use_features <- function(.track_id=1, ..., .ignore="genes"){
  dots <- quos(...)
  function(.x, ...){
    pull_features(.x, {{.track_id}}, !!! dots, .ignore=.ignore)
  }
}
#' @rdname use_seqs
#' @export
use_links <- function(.track_id=1, ...){
  dots <- quos(...)
  function(.x, ...){
    pull_links(.x, {{.track_id}}, !!! dots)
  }
}
#' @rdname use_seqs
#' @export
use_genes <- function(.track_id=1, ..., .ignore=NA){
  dots <- quos(...)
  function(.x, ...){
    t <- pull_features(.x, {{.track_id}}, .ignore=.ignore)
    if(is_likely_feature_track(t)){
      if(!has_name(t, "type")) t[["type"]] <- "CDS"
      if(!has_name(t, "gene_id")) t[["gene_id"]] <- paste0("__", seq_len(nrow((t))))
    }
    filter(t, !!! dots)
  }
}
is_likely_feature_track <- function(x){
  !any(has_name(x, c("bin_offset", "from_id")))
}
#' @rdname use_seqs
#' @param group what variables to use in grouping of bins from seqs in addition
#' to `y` and `bin_id`. Use this to get additional shared variables from the
#' seqs table into the bins table.
#' @export
use_bins <- function(..., group = vars()){
  dots <- quos(...)
  group <- c(vars(y, bin_id), group)
  function(.x, ...){
    seqs(.x) %>% filter(!!! dots) %>% group_by(!!!group) %>%
      summarize(x = min(x,xend), xend = max(x,xend)) %>%
      ungroup()
  }
}
