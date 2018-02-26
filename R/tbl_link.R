#' @export
as_tbl_link <- function(data, ...){
    UseMethod("as_tbl_link")
}

#' @export
as_tbl_link.default <- function(data, ...) {
    # try to coerce into tbl
    as_tbl_link(as_tibble(data), ...)
}

#' @export
as_tbl_link.tbl_df <- function(
    data, mapping = quos(gid = 1, cid = 2, length = 3, everything())){

    if(ncol(data) < 3)
        stop("At least 3 columns required: genome ID, contig ID, and contig length")
    
    required_mapping <- c("gid", "cid", "length")
    default_mapping <- quos(gid = 1, cid = 2, length = 3, everything());
    
    mapping <- modifyList(default_mapping, mapping)
    
    data <- select(data, !!! mapping)
    if(!identical(names(data)[1:3], required_mapping))
        stop("Bad column name mapping: first three columns must map to: ", paste(required_mapping, collapse=", "))

    class(data) <- c("tbl_link", class(data))
    data
}
