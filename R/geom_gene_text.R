#' Add text to genes, features, etc.
#'
#' @description
#' The functions below are useful for labeling features/genes in plots.
#' Users have to call on `aes(label = ...)` or `(label = ...) `to define label's text
#' Based on the function, the label will be placed at a specific location:
#' - `geom_..._text()` will plot **text in the middle of the feature**.
#' - `geom_..._tag()`  will plot **text on top of the feature, with a 45 degree angle**.
#' - `geom_..._note()` will plot **text under the feature at the left side**.
#'
#' *The `...` can be either replaced with `feat` or `gene` depending on which*
#' *track the user wants to label.*
#'
#'
#' With arguments such as `hjust`, `vjust`, `angle`, and `nudge_y`, the user
#' can also manually change the position of the text.
#'
#' @details
#' These labeling functions use `ggplot2::geom_text()` under the hood.
#' Any changes to the aesthetics of the text can be performed in a ggplot2 manner.
#' @param hjust Moves the text horizontally
#' @param vjust Moves the text vertically
#' @param nudge_y Moves the text vertically an entire contig/sequence.
#' (e.g. `nudge_y = 1` places the text to the contig above)
#' @param angle Defines the angle in which the text will be placed. *Note
#'
#' @inheritParams ggplot2::geom_text
#' @export
#' @examples
#' # example data
#' genes <- tibble::tibble(
#'   seq_id = c("A", "A", "A", "B", "B", "C"),
#'   start = c(20, 40, 80, 30, 10, 60),
#'   end = c(30, 70, 85, 40, 15, 90),
#'   feat_id = c("A1", "A2", "A3", "B1", "B2", "C1"),
#'   type = c("CDS", "CDS", "CDS", "CDS", "CDS", "CDS"),
#'   name = c("geneA", "geneB", "geneC", "geneA", "geneC", "geneB")
#' )
#'
#' seqs <- tibble::tibble(
#'   seq_id = c("A", "B", "C"),
#'   start = c(0, 0, 0),
#'   end = c(100, 100, 100),
#'   length = c(100, 100, 100)
#' )
#'
#' # basic plot creation
#' plot <- gggenomes(seqs = seqs, genes = genes) +
#'   geom_bin_label() +
#'   geom_gene()
#'
#' # geom_..._text
#' plot + geom_gene_text(aes(label = name))
#'
#' # geom_..._tag
#' plot + geom_gene_tag(aes(label = name))
#'
#' # geom_..._note
#' plot + geom_gene_note(aes(label = name))
#'
#' # with horizontal adjustment (`hjust`), vertical adjustment (`vjust`)
#' plot + geom_gene_text(aes(label = name), vjust = -2, hjust = 1)
#'
#' # using `nudge_y` and and `angle` adjustment
#' plot + geom_gene_text(aes(label = name), nudge_y = 1, angle = 10)
#'
#' # labeling with manual input
#' plot + geom_gene_text(label = c("This", "is", "an", "example", "test", "test"))
geom_feat_text <- function(mapping = NULL, data = feats(), stat = "identity", position = "identity",
                           ..., parse = FALSE, check_overlap = FALSE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  default_aes <- aes(y = .data$y, x = .data$x, xend = .data$xend)
  mapping <- aes_intersect(mapping, default_aes)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFeatText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_feat_text
#' @param xjust Move text in x direction
#' @param strandwise plotting of feature tags
#' @export
geom_feat_tag <- function(mapping = NULL, data = feats(), stat = "identity", position = "identity",
                          hjust = 0, vjust = 0, angle = 45, nudge_y = .03, xjust = 0.5, strandwise = TRUE, ...,
                          parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom_feat_text(
    mapping = mapping, data = data, stat = stat, position = position,
    hjust = hjust, vjust = vjust, angle = angle, nudge_y = nudge_y, xjust = xjust, strandwise = strandwise,
    ..., parse = parse, check_overlap = check_overlap, na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @rdname geom_feat_text
#' @export
geom_feat_note <- function(mapping = NULL, data = feats(), stat = "identity", position = "identity",
                           hjust = 0, vjust = 1, nudge_y = -.03, xjust = 0, strandwise = FALSE, ...,
                           parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom_feat_text(
    mapping = mapping, data = data, stat = stat, position = position,
    hjust = hjust, vjust = vjust, nudge_y = nudge_y, xjust = xjust, strandwise = strandwise, ...,
    parse = parse, check_overlap = check_overlap, na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @rdname geom_feat_text
#' @export
geom_gene_text <- function(mapping = NULL, data = genes(), stat = "identity", position = "identity",
                           ..., parse = FALSE, check_overlap = FALSE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  default_aes <- aes(y = .data$y, x = .data$x, xend = .data$xend, type = .data$type, group = .data$geom_id)
  mapping <- aes_intersect(mapping, default_aes)

  geom_feat_text(
    mapping = mapping, data = data, stat = stat, position = position, ...,
    parse = parse, check_overlap = check_overlap, na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @rdname geom_feat_text
#' @export
geom_gene_tag <- function(
    mapping = NULL, data = genes(), stat = "identity", position = "identity",
    hjust = 0, vjust = 0, angle = 45, nudge_y = .03, xjust = 0.5, strandwise = TRUE, ...,
    parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom_gene_text(
    mapping = mapping, data = data, stat = stat, position = position,
    hjust = hjust, vjust = vjust, angle = angle, nudge_y = nudge_y, xjust = xjust, strandwise = strandwise,
    ..., parse = parse, check_overlap = check_overlap, na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @rdname geom_feat_text
#' @export
geom_gene_note <- function(
    mapping = NULL, data = genes(), stat = "identity", position = "identity",
    hjust = 0, vjust = 1, nudge_y = -.03, xjust = 0, strandwise = FALSE, ...,
    parse = FALSE, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom_gene_text(
    mapping = mapping, data = data, stat = stat, position = position,
    hjust = hjust, vjust = vjust, nudge_y = nudge_y, xjust = xjust, strandwise = strandwise, ...,
    parse = parse, check_overlap = check_overlap, na.rm = na.rm, show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}



#' Geom for feature text
#' @export
GeomFeatText <- ggproto("GeomFeatText", Geom,
  required_aes = c("x", "xend", "y", "label"),
  optional_aes = c("type"),
  default_aes = aes(
    colour = "black", size = 2.5, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = 1, family = "", fontface = 1, lineheight = 1.2
  ),
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE,
                        nudge_x = 0, nudge_y = 0, xjust = 0.5, strandwise = FALSE) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    # nudge
    if (strandwise) {
      data$nudge_x <- ifelse(data$x > data$xend, nudge_x * -1, nudge_x)
    } else {
      data$nudge_x <- nudge_x
      data <- swap_if(data, x > xend, x, xend)
    }

    # need to keep x/xend as is for position pile. Use xmin for text anchor -
    # needs to be known x-aes for coord$transform
    data$xmin <- data$x * (1 - xjust) + data$xend * xjust
    data$xmin <- data$xmin + data$nudge_x
    data$y <- data$y + nudge_y

    # transform
    data <- coord$transform(data, panel_params)

    # just
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$xmin)
    }

    textGrob(
      lab,
      data$xmin, data$y,
      default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },
  draw_key = draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(
    left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1
  )[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
