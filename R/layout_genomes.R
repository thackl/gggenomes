#' Compute a genomes layout from sequences, features and links
#'
#' See `gggenomes::gggenomes()` for more info.
#' @rdname as_genomes
#' @param seqs a table with sequence data (seq_id, bin_id, length)
#' @param features a table (or names list of tables) with feature data (seq_id,
#' bin_id, start, end)
#' @param links a table with link data (from, to, from_start, from_end,
#' to_start, to_end)
#' @param infer_bin without seqs infer bin_ids from features/links
#' @param infer_bin without seqs infer seq length from features/links
#' @param ... layout parameters passed on to `layout_seqs()`
#' @export
layout_genomes <- function(seqs=NULL, features=NULL, links=NULL, .feature_id = "genes", .link_id = "links", infer_bin = seq_id, infer_length = max(end), ...){

  x <- list(seqs = NULL, features = list(), links = list(), orig_links = list(),
            params = list())
  x %<>% set_class("gggenomes_layout", "prepend")

  if(!is.null(features) & is.data.frame(features))
    features <- set_names(list(features), .feature_id)
  if(!is.null(links) & is.data.frame(links))
    links <- set_names(list(links), .link_id)

  if(!is.null(seqs)){
    if(!has_name(seqs, "bin_id"))
      seqs <- mutate(seqs, bin_id = {{ infer_bin }})
  }else{
    if(is.null(features) & is.null(links))
      stop("Need at least one of: contigs, genes or links")

    # infer dummy seqs
    if(!is.null(features)){
      write("No seqs provided, inferring seqs from features", stderr())
      seqs <- infer_seqs_from_features(features[[1]], {{infer_bin}}, {{infer_length}})
    }else if(!is.null(links)){
      write("No seqs or features provided, inferring seqs from links", stderr())
      seqs <- infer_seqs_from_links(links[[1]], infer_bin={{infer_bin}}, {{infer_length}})
    }
  }

  x %<>% add_seqs(seqs, ...) # layout seqs
  if(!is.null(features)) x <- exec(add_features, x, !!!features)
  if(!is.null(links)) x <- exec(add_links, x, !!!links)
  x
}

#' `ggplot2::facet_null` checks data with `empty(df)` using `dim`. This causes
#' and error because dim(gggenome_layout) is undefined. Return dim of primary
#' table instead
#' @export
dim.gggenomes_layout <- function(x) dim(x$seqs)


infer_seqs_from_features <- function(features, infer_bin = seq_id, infer_length = max(end)){
  if(!has_name(features, "bin_id"))
    features <- mutate(features, bin_id = {{ infer_bin }})

  seqs <- features %>%
    group_by(bin_id, seq_id) %>%
    summarize(length = {{ infer_length }})
}

infer_seqs_from_links <- function(links, infer_bin = seq_id, infer_length = max(end)){
  seqs <- bind_rows(
    select_at(links, vars(starts_with("from_")), str_replace, "from_", ""),
    select_at(links, vars(starts_with("to_")), str_replace, "to_", "")
  ) %>%
    rename(seq_id = id) %>%
    mutate(bin_id = {{ infer_bin }}) %>%
    group_by(bin_id, seq_id) %>%
    summarize(length = {{ infer_length }})
}

#' Use a specific track table inside a `geom_*` call.
#'
#' Use this function inside `geom_*` calls to set the track table of the
#' gggenomes layout you want to use, e.g. seqs, genes or links. Also allows you
#' to pass on filter arguments to subset the data.
#'
#' @inheritParams expose
#' @param ... filter arguments passed through to [dplyr::filter].
#' @export
use <- function(track=seqs, ...) {
  dots <- quos(...)
    function(x, ...){
      track_names <- c("seqs", names(x$features), names(x$links))
      # more useful error
      track_name <- tryCatch(
        tidyselect::vars_pull(track_names, {{track}}),
        error = function(err){
          rlang::abort(paste("in use()", err$message))})

      if(track_name == "seqs"){
        filter(x[[track_name]], !!! dots)
      }else{
        track_types <- c("seqs", rep("features", length(x$features)),
                         rep("links", length(x$links)))
        track_type <- track_types[track_name == track_names]
        filter(x[[track_type]][[track_name]], !!! dots)
      }
    }
}


