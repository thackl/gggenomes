#' @export
stat_offset_length <- function (mapping = NULL, data = NULL, geom = "segment",
    position = "identity", ..., show.legend = NA, inherit.aes = TRUE){

    layer(
        data = data, mapping = mapping, stat = StatOffsetLength,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, ...))
}

StatOffsetLength <- ggproto(
    "StatOffsetLength", Stat,
    compute_panel = function(data, scales){
        if(any(names(data) == 'strand')){
            data$x <- ifelse(data$strand < 0, data$offset + data$length, data$offset)
            data$xend <- ifelse(data$strand < 0, data$offset, data$offset + data$length)
        }else{
            data$x <- data$offset
            data$xend <- data$offset + data$length
        }
        data
    },
    required_aes = c("offset","length"),
    default_aes = aes(strand = 1)
)

#' @export
stat_offset_range <- function (mapping = NULL, data = NULL, geom = "segment",
    position = "identity", ..., arrow = NULL, show.legend = NA, inherit.aes = TRUE){

    layer(
        data = data, mapping = mapping, stat = StatOffsetRange,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(arrow = arrow, na.rm = FALSE, ...))
}

StatOffsetRange <- ggproto(
    "StatOffsetRange", Stat,
    compute_panel = function(data, scales){
        if(any(names(data) == 'strand')){
            data$x <- ifelse(data$strand < 0, data$offset + data$end, data$offset + data$start)
            data$xend <- ifelse(data$strand < 0, data$offset + data$start, data$offset + data$end)
        }else{
            data$x <- data$start + data$offset
            data$xend <- data$end + data$offset
        }
        data
    },
    required_aes = c("offset","start","end"),
    default_aes = aes(strand = 1)
)
