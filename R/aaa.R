# thomasp85/ggraph https://github.com/thomasp85/ggraph/blob/master/R/aaa.R
aesIntersect <- function(aes1, aes2) {
    structure(
        c(as.list(aes1), aes2[!names(aes2) %in% names(aes1)]),
        class = 'uneval'
    )
}

null_else <- function(a, b) {
    if (is.null(a)) NULL else b
}
