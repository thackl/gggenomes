null_else <- function(a, b) {
  if (is.null(a)) NULL else b
}

has_dots <- function(env = parent.frame()){
  length(ellipsis:::dots(env)) > 0
}
