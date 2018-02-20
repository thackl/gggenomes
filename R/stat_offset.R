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
    compute_group = function(data, scales){
        # set strand (too lazy to handle cases w/o)
        if(!any(names(data) == 'strand')) data$strand <- 1

        # x and xend depending on strand
        data$x    <- ifelse(data$strand < 0, data$offset + data$length, data$offset)
        data$xend <- ifelse(data$strand < 0, data$offset, data$offset + data$length)
        data
    },
    required_aes = c("offset","length"),
    default_aes = aes(strand = 1) # makes it "known aesthetics"
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
        # set strand (too lazy to handle cases w/o)
        if(!any(names(data) == 'strand')) data$strand <- 1
        if(!any(names(data) == 'end')) data$end <- data$start

        # x and xend depending on strand
        data$x    <- ifelse(data$strand < 0, data$offset + data$end, data$offset + data$start)
        data$xend <- ifelse(data$strand < 0, data$offset + data$start, data$offset + data$end)
        data
    },
    required_aes = c("offset","start"),
    default_aes = aes(strand = 1, end = start) # makes it "known aesthetics"
)

#' @export
stat_offset_gene <- function (mapping = NULL, data = expose_data(features),
  geom = gggenes:::GeomGeneArrow, position = "identity", ...,
  arrowhead_width = grid::unit(4, "mm"),
  arrowhead_height = grid::unit(4, "mm"),
  arrow_body_height = grid::unit(3, "mm"),
  show.legend = NA, inherit.aes = TRUE){

    layer(
        data = data, mapping = mapping, stat = StatOffsetGene,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(na.rm = FALSE,
            arrowhead_width = arrowhead_width,
            arrowhead_height = arrowhead_height,
            arrow_body_height = arrow_body_height, ...))
}

StatOffsetGene <- ggproto(
    "StatOffsetGene", Stat,
    compute_panel = function(data, scales){
        # set strand (too lazy to handle cases w/o)
        if(!any(names(data) == 'strand')) data$strand <- 1

        # xmin and xmax depending on strand
        data$xmin <- ifelse(data$strand < 0, data$offset + data$end, data$offset + data$start)
        data$xmax <- ifelse(data$strand < 0, data$offset + data$start, data$offset + data$end)
        data
    },
    required_aes = c("offset","start","end"),
    default_aes = aes(strand = 1) # makes it "known aesthetics"
)
