#' Stack overlapping features
#'
#' `position_stack1()` stacks all overlapping features in one direction,
#' `position_stack2()` stacks forward and reverse strand features in opposite
#' directions above and below each sequence.
#'
#' @param offset Shift overlapping labels up/down this much on the y-axis. The
#' y-axis distance between two sequences is 1, so this is usually a small
#' fraction, such as 0.02.
#' @param strandwise TRUE to stack forward features above and reverse features
#' below the sequence.
#' @param vjust How to align the stack relative to the sequence. 0 to center the
#' lowest stack level on the sequence, 1 to put forward/reverse sequence on half
#' offset above/below the sequence line.
#' @param padding A positive number to stack features that are not overlapping
#' but within this padding distance of each other, or negative number to not
#' stack features with smaller overlaps. Set to NA to not stack at all, but
#' still have strandwise or vjust take effect
#' @param forward_below By default forward features are stacked above, reverse
#' features depending on strandwise also above or below the genome. Set TRUE to
#' stack the other way around.
#' @export
position_stack1 <- function(offset = 0.02, strandwise = FALSE, vjust = as.numeric(strandwise), padding=1,
  forward_below = FALSE) {
  ggproto(NULL, PositionStack2, offset = offset, strandwise = strandwise, vjust = vjust, padding = padding,
    forward_below = forward_below)
}

#' @rdname position_stack1
#' @export
position_stack2 <- function(offset = 0.02, strandwise = TRUE, vjust = as.numeric(strandwise), padding=1,
  forward_below = FALSE){
  ggproto(NULL, PositionStack2, offset = offset, strandwise = strandwise, vjust = vjust, padding = padding,
    forward_below = forward_below)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionStack2 <- ggproto("PositionStack2", Position,
  offset = 0.02,
  strandwise = FALSE,
  vjust = 0,
  padding = 0,
  forward_below = FALSE,
  required_aes = c("x","xend","y"),
  optional_aes = c("yend"),
  setup_params = function(self, data){
    list(offset = self$offset, strandwise = self$strandwise, vjust=self$vjust, padding=self$padding,
         forward_below = self$forward_below)
  },
  compute_panel = function(data, params, scales) {
    if(!params$strandwise && is.na(params$padding)){
      return(data) # nothing to do
    }
    # not pretty, but works for now
    foo <- data %>%
      group_by(y,group) %>%
      summarize(
        start=min(x,xend)+1, end=max(x,xend),
        is_reverse=ifelse(params$strandwise, xor(min(x)>max(xend), params$forward_below), params$forward_below)) %>%
      group_by(y,is_reverse) %>%
      mutate(yoff = params$offset * stack_pos(start,end,params$vjust,params$padding) *
               ifelse(is_reverse, -1,1)) %>%
      ungroup
    #
    data <- left_join(data, select(foo, y, group, yoff))
    if("yend" %in% names(data))
      data <- mutate(data, y = y + yoff, yend = yend + yoff, is_reverse=NULL, yoff=NULL)
    else
      data <- mutate(data, y = y + yoff, is_reverse=NULL, yoff=NULL)
    data
  }
)

stack_pos <- function(start,end,vjust=0,padding=0){
  if(!is.na(padding)){
    start <- start-padding/2
    end <- end+padding/2
    i <- end< start  # set negtive width features to zero width
    start[i] <- (start[i] + end[i])/2
    end[i] <- start[i]
    y <- IRanges::disjointBins(IRanges::IRanges(start=start, end=end))
  }else{
    y <- rep(1, length(start))
  }
  y   + (vjust/2 -1)
}
