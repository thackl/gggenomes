#' Example data sets
#'
#' list available example data sets
#' @export
#' @importFrom tibble as_tibble
example_sets <- function(quiet=FALSE){
  TODO('df with desc')
  example_dir <- system.file(paste0("extdata/"), package="gggenomes")
  if(!quiet) cat('# Reading example data from:\n# ', example_dir, '\n', sep="")
  example_sets <- list.files(example_dir, recursive=FALSE)
  #cat(paste0(example_sets, '\n', collapse=""))
  as_tibble(example_sets)
}

#' list files from example data sets
#'
#' @param set data set, see `example_sets` for a list
#' @param patt wildcard pattern to filter files
#' @export
example_files <- function(set = NULL, patt = "*", ...){
  if(is.null(set)) return(example_sets(...))
  example_dir <- system.file(paste0("extdata/", set), package="gggenomes")
  list.files(example_dir, patt, recursive=FALSE, full.names=TRUE)
}
