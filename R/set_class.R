#' Modify object class attriutes
#'
#' Set class of an object. Optionally append or prepend to exiting class
#' attributes. `add_class` is short for `set_class(x, class, "prepend")`.
#' `strip_class` removes matching class strings from the class attribute vector.
#'
#' @param x Object to assign new class to.
#' @param class Class value to add/strip.
#' @param add Possible values: "overwrite", "prepend", "append"
#' @return Object x as class value.
#' @export
set_class <- function(x, class, add=c("overwrite", "prepend", "append")){
  add = match.arg(add)
  class <- switch(add,
                  overwrite = class,
                  prepend = unique(c(class, class(x))),
                  append = unique(c(class(x), class)))
  if(length(class) == 0) # for class == ""
    unclass(x)
  else
    `class<-`(x, class)
}
#' @export
#' @rdname set_class
add_class <- function(x, class){
  set_class(x, class, "prepend")
}
#' @export
#' @rdname set_class
strip_class <- function(x, class){
  classes <- class(x)[!class(x) %in% class]
  set_class(x, classes)
}
