#' Stack features
#'
#' `position_strand()` offsets forward feats upward and reverse feats downward.
#' `position_pile()` stacks overlapping feats upward. `position_strandpile()`
#' stacks overlapping feats up-/downward based on their strand.
#' `position_sixframe()` offsets the feats based on their strand and reading
#' frame.
#'
#' @param offset Shift overlapping feats up/down this much on the y-axis. The
#'   y-axis distance between two sequences is 1, so this is usually a small
#'   fraction, such as 0.1.
#' @param gap If two feats are closer together than this, they will be stacked.
#'   Can be negative to allow small overlaps. NA disables stacking.
#' @param flip stack downward, and for stranded versions reverse upward.
#' @param grouped if TRUE feats in the same group are stacked as a single
#'   feature. Useful to move CDS and mRNA as one unit. If NULL (default) set to
#'   TRUE if data appears to contain gene-ish features.
#' @param base How to align the stack relative to the sequence. 0 to center the
#'   lowest stack level on the sequence, 1 to put forward/reverse sequence one
#'   half offset above/below the sequence line.
#' @export
#' @examples
#' library(patchwork)
#' p <- gggenomes(emale_genes) %>%
#'   pick(3:4) + geom_seq()
#'
#' f0 <- tibble::tibble(
#'   seq_id = pull_seqs(p)$seq_id[1],
#'   start = 1:20*1000,
#'   end = start + 2500,
#'   strand = rep(c("+", "-"), length(start)/2)
#' )
#'
#' sixframe <- function(x, strand) as.character((x%%3 + 1) * strand_int(strand))
#'
#' p1 <- p + geom_gene()
#' p2 <- p + geom_gene(aes(fill=strand), position="strand")
#' p3 <- p + geom_gene(aes(fill=strand), position=position_strand(flip=TRUE, base=0.2))
#' p4 <- p + geom_gene(aes(fill=sixframe(x,strand)), position="sixframe")
#' p5 <- p %>% add_feats(f0) + geom_gene() + geom_feat(aes(color=strand))
#' p6 <- p %>% add_feats(f0) + geom_gene() + geom_feat(aes(color=strand), position="strandpile")
#' p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol=3, guides="collect") & ylim(2.5,0.5)
#'
position_strand <- function(offset = 0.1, flip = FALSE, grouped = NULL,
                            base = offset/2){
  ggproto(NULL, PositionStrand, offset = offset, flip = flip,
          grouped = grouped, base = base)
}
#' @rdname position_strand
#' @export
position_pile <- function(offset = 0.1, gap=1, flip = FALSE,
    grouped = NULL, base = 0){
  ggproto(NULL, PositionPile, offset = offset, gap = gap,
          flip = flip, grouped = grouped, base = base)
}
#' @rdname position_strand
#' @export
position_strandpile <- function(offset = 0.1, gap=1, flip = FALSE,
    grouped = NULL, base = offset*1.5){
  ggproto(NULL, PositionStrandpile, offset = offset, gap = gap,
          flip = flip, grouped = grouped, base = base)
}
#' @rdname position_strand
#' @export
position_sixframe <- function(offset = 0.1, flip = FALSE, grouped = NULL,
    base = offset/2){
  ggproto(NULL, PositionSixframe, offset = offset, flip = flip,
          grouped = grouped, base = base, framewise=TRUE)
}
#' @rdname position_strand
#' @format NULL
#' @usage NULL
#' @export
PositionStrandpile <- ggproto("PositionStrandpile", Position,
  offset = 0.1,
  strandwise = TRUE,
  base = 0.15,
  gap = 0,
  flip = FALSE,
  grouped = NULL,
  framewise = FALSE,
  required_aes = c("x","xend","y"),
  optional_aes = c("yend"),
  setup_params = function(self, data){
    # DO NOT OVERWRITE self$grouped - it will propagate to subsequent
    # instanciations in other plots!
    if(is.null(self$grouped)) # assume grouped for geneish geoms
      grouped <- has_vars(data, c("type"))
    else
      grouped <- self$grouped

    list(offset = self$offset, strandwise = self$strandwise, base=self$base,
         gap=self$gap, flip = self$flip,
         grouped = grouped, framewise = self$framewise)
  },
  compute_panel = function(data, params, scales) {
    if(!params$strandwise && !params$framewise && is.na(params$gap)){
      return(data) # nothing to do
    }
    if(params$framewise){
      data <- data %>% mutate(
        is_reverse =  xor(x>xend, params$flip),
        yoff = (params$base + params$offset * x %% 3) * ifelse(is_reverse, -1,1)
      )
    }else if(is.na(params$gap)){ # strand only
      data <- data %>% mutate(
        is_reverse =  xor(x>xend, params$flip),
        yoff = params$base * ifelse(is_reverse, -1,1)
      )
    }else if(params$grouped){ # i.e. multi-exon as one unit
      # not pretty, but works for now
      data_grouped <- data %>%
        group_by(y,group) %>%
        summarize(
          start=min(x,xend)+1, end=max(x,xend), is_reverse=ifelse(params$strandwise,
              xor(min(x)>max(xend), params$flip), params$flip))

      data_grouped <- data_grouped %>% group_by(y,is_reverse) %>%
        mutate(yoff = (params$base + params$offset *
          stack_pos(start,end,params$gap)) *
          ifelse(is_reverse, -1,1)) %>%
        ungroup

      data <- left_join(data, select(data_grouped, y, group, yoff), by=c("y", "group"))

    }else{
      data <- data %>%
        mutate(
          start=pmin(x,xend)+1, end=pmax(x,xend),
          is_reverse=if(params$strandwise) xor(x>xend, params$flip) else params$flip) %>%
        group_by(y,is_reverse) %>%
        mutate(yoff = (params$base + params$offset *
           stack_pos(start,end,params$gap)) *
           ifelse(is_reverse, -1,1)) %>%
        ungroup
    }

    if("yend" %in% names(data))
      data <- mutate(data, y = y + yoff, yend = yend + yoff, is_reverse=NULL, yoff=NULL)
    else
      data <- mutate(data, y = y + yoff, is_reverse=NULL, yoff=NULL)
    data
  }
)
#' @rdname position_strand
#' @format NULL
#' @usage NULL
#' @export
PositionPile <- ggproto("PositionPile", PositionStrandpile, strandwise = FALSE, base = 0)
#' @rdname position_strand
#' @format NULL
#' @usage NULL
#' @export
PositionStrand <- ggproto("PositionStrand", PositionStrandpile, gap = NA, base = 0.05)
#' @rdname position_strand
#' @format NULL
#' @usage NULL
#' @export
PositionSixframe <- ggproto("PositionSixframe", PositionStrand, framewise = TRUE, base = 0.05)


stack_pos <- function(start,end,gap=0){
  if(!is.na(gap)){
    end <- end+gap
    i <- end< start  # set negtive width feats to zero width
    start[i] <- (start[i] + end[i])/2
    end[i] <- start[i]
    y <- IRanges::disjointBins(IRanges::IRanges(start=start, end=end)) -1
  }else{
    y <- rep(0, length(start))
  }
  y
}
