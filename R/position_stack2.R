#' Stack overlapping features
#'
#' @export
position_stack2 <- function(offset = 0.02, strandwise = FALSE, vjust = as.numeric(strandwise), padding=1) {
  ggproto(NULL, PositionStack2, offset = offset, strandwise = strandwise, vjust = vjust)
}

#' @rdname position_stack2
#' @exporta
position_stack3 <- function(offset = 0.02, strandwise = TRUE, vjust = as.numeric(strandwise)){
  ggproto(NULL, PositionStack3, offset = offset, strandwise = strandwise, vjust = vjust)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionStack2 <- ggproto("PositionStack2", Position,
  offset = 0.02,
  strandwise = FALSE,
  vjust = 0,
  required_aes = c("x","xend","y"),
  optional_aes = c("yend"),
  setup_params = function(self, data){
    list(offset = self$offset, strandwise = self$strandwise, vjust=self$vjust)
  },
  compute_panel = function(data, params, scales) {
    foo <- data %>%
      group_by(y,group) %>%
      summarize(
        start=min(x,xend)+1, end=max(x,xend),
        reverse=ifelse(params$strandwise, min(x)>max(xend), FALSE)) %>%
      group_by(y,reverse) %>%
      mutate(yoff = params$offset * stack_pos(start,end,params$vjust) * ifelse(reverse, -1,1)) %>%
      ungroup
    #
    data <- left_join(data, select(foo, y, group, yoff))
    if("yend" %in% names(data))
      data <- mutate(data, y = y + yoff, yend = yend + yoff, reverse=NULL, yoff=NULL)
    else
      data <- mutate(data, y = y + yoff, reverse=NULL, yoff=NULL)
    data
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionStack3 <- ggproto("PositionStack3", PositionStack2,
  strandwise = TRUE, vjust = 1)

stack_pos <- function(start,end,vjust=0){
  IRanges::disjointBins(IRanges::IRanges(start={{start}}, end={{end}})) + (vjust/2 -1)
}
