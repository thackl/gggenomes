#' Scalebars for gggenomes plots
#'
#' Add and customize genomic scalebars in gggenomes plots. `axis_scalebar()`
#' configures a scalebar along the plot axis, `guide_scalebar()` provides the
#' underlying ggplot2 guide, `theme_scalebar()` controls its appearance, and
#' `geom_scalebar()` draws a freely positioned scalebar inside the gggenomes
#' panel.
#'
#' @describeIn axis_scalebar customize the axis scalebar, conveniently wrapping `guide_scalebar()` and `theme_scalebar()`
#' @inheritParams guide_scalebar
#' @inheritParams theme_scalebar
#' @return a list with a ggplot2 guide and a ggplot theme object
#' @export
#' @examples
#' # gggenomes' options for representing the x-axis scaling are:
#' library(patchwork) # to combine plots in same figure
#'
#' # 1. The default `axis_scalebar` with short ...
#' p1 <- gggenomes(genes = emale_genes) |> pick(1:2) + # two short genomes
#'   geom_seq() + geom_gene()
#'
#' # ... and with long sequences; some simple styling ...
#' s0 <- tibble::tibble(
#'   seq_id = c("a", "b", "b"),
#'   length = c(2000000, 500000, 1000000))
#' q0 <- gggenomes(seqs = s0) + geom_seq()
#' q1 <- q0 + axis_scalebar(length = 0.1, fontface = "bold")
#'
#' # ... or more advanced styling, ...
#' p2 <- p1 + axis_scalebar(length = 3333, label_position = "below",
#'     linewidth = 0.6, tick_height = c(1,6))
#' # ... down to theme and guide customization
#' q2 <- q1 +
#'  theme_scalebar(text_size = 8, color = "red") +
#'  guides(
#'   x = guide_scalebar(just = "left", label = \(x){paste(x, "nucleotides")}))
#'
#' p1 + q1 + p2 + q2 + plot_layout(ncol=2)
#'
#' # 2. The optional `geom_scalebar` with easy relative ...
#' p3 <- p1 + geom_scalebar(x = "center", y = 0.5)
#' # ... and absolute placement anywhere in the plot area
#' q3 <- q1 + geom_scalebar(length = 0.1, x = 1.5e6, y = 1.3, color = "blue",
#'   fontface = "italic")
#' # 3. Or also a regular axis with genomic units...
#' p4 <- p1 + guides(x = "axis")
#' # ... which can be further customized
#' q4 <- q0 + scale_x_genomic(guide = "axis", unit = "", sep = "")
#'
#' p3 + q3 + p4 + q4 + plot_layout(ncol=2)
#'
#' # Note: `xlim()` or scale_x_continuous()` will overwrite the genomic scale
#' p1 + xlim(c(0, 1e4)) + 
#' # Use `scale_x_genomic(limits=)` to prevent that
#' p1 + scale_x_genomic(limits=c(0, 1e4))
axis_scalebar <- function(
    length = 0.25,
    label = NULL,
    just = c("right", "center", "left"),
    position = "bottom",
    label_position = c("above", "below"),
    linewidth = NULL,
    color = NULL,
    colour = NULL,
    linetype = NULL,
    lineend = NULL,
    text_size = NULL,
    text_color = NULL,
    text_colour = NULL,
    family = NULL,
    fontface = NULL,
    tick_height = NULL,
    label_offset = NULL
) {
  just <- match.arg(just)
  label_position <- match.arg(label_position)

  colour <- color %||% colour
  text_colour = text_color %||% text_colour


  list(
    ggplot2::guides(
      x = guide_scalebar(
        length = length,
        label = label,
        just = just,
        position = position,
        label_position = label_position
      )
    ),
    theme_scalebar(
      linewidth = linewidth,
      colour = colour,
      linetype = linetype,
      lineend = lineend,
      text_size = text_size,
      text_colour = text_colour,
      family = family,
      fontface = fontface,
      tick_height = tick_height,
      label_offset = label_offset
    )
  )
}

#' @describeIn axis_scalebar create a scalebar guide
#' @inheritParams ggplot2::guide_axis
#' @param length Scalebar length. Values between 0 and 1 are interpreted as
#'   a target fraction of the displayed x range and rounded down to a nice
#'   1/2/5 x 10^n value. Values >= 1 are interpreted as absolute x units.
#' @param just Horizontal placement within the scale: left, center, or right.
#' @param label Optional label. By default, the scalebar length in bp/kb/Mb/Gb;
#'   Can be text or a function, which is called with the scalebar length.
#' @param label_position Draw the label above or below the bar.
#' @return a ggplot2 guide object
#' @export
guide_scalebar <- function(
    length = 0.25,
    just = c("right", "center", "left"),
    label = NULL,
    label_position = c("above", "below"),
    theme = NULL,
    order = 0,
    position = ggplot2::waiver()
) {
  just <- match.arg(just)
  label_position <- match.arg(label_position)

  .check_scalebar_length(length)

  ggplot2::new_guide(
    length = length,
    label = label,
    just = just,
    label_position = label_position,
    title = NULL,
    theme = theme,
    order = order,
    position = position,
    available_aes = "x",
    name = "scalebar",
    super = GuideScalebar
  )
}

#' @describeIn axis_scalebar update the axis scalebar theme
#' @details
#' `theme_scalebar()` styles the dedicated gggenomes scalebar theme elements
#' used by `guide_scalebar()`. `geom_scalebar()` exposes matching styling
#' arguments directly because ggplot2 3.5.x does not pass the plot theme to
#' Geom$draw_panel().
#'
#' @param linewidth,color,colour,linetype,lineend Scalebar line/tick styling.
#' @param text_size,text_color,text_colour,family,fontface Scalebar label styling.
#' @param tick_height Total physical length of each centered end tick. Numeric
#'   values are interpreted as points.
#' @param label_offset Physical spacing between bar and label. Numeric values
#'   are interpreted as points.
#' @inheritParams ggplot2::theme
#' @return a ggplot2 theme object
#' @export
theme_scalebar <- function(
    linewidth = NULL,
    color = NULL,
    colour = NULL,
    linetype = NULL,
    lineend = NULL,
    text_size = NULL,
    text_color = NULL,
    text_colour = NULL,
    family = NULL,
    fontface = NULL,
    tick_height = NULL,
    label_offset = NULL
) {
  elements <- list()

  colour <- color %||% colour
  text_colour = text_color %||% text_colour


  if (any(!vapply(
    list(linewidth, colour, linetype, lineend),
    is.null, logical(1)
  ))) {
    elements$gggenomes.scalebar.line <- ggplot2::element_line(
      linewidth = linewidth,
      colour = colour,
      linetype = linetype,
      lineend = lineend
    )
  }

  if (any(!vapply(
    list(text_size, text_colour, family, fontface),
    is.null, logical(1)
  ))) {
    elements$gggenomes.scalebar.text <- ggplot2::element_text(
      size = text_size,
      colour = text_colour,
      family = family,
      face = fontface
    )
  }

  if (!is.null(tick_height)) {
    elements$gggenomes.scalebar.tick.height <-
      .as_scalebar_unit(tick_height)
  }

  if (!is.null(label_offset)) {
    elements$gggenomes.scalebar.label.spacing <-
      .as_scalebar_unit(label_offset)
  }

  do.call(ggplot2::theme, elements)
}

#' @describeIn axis_scalebar draw a flexible scalebar inside the gggenomes panel
#' @inheritParams geom_feat
#' @inheritParams guide_scalebar
#' @inheritParams theme_scalebar
#' @param x,y Scalebar coordinates. Values between 0 and 1 are interpreted as
#'   panel-relative, values >= 1 and < 0 as absolute coordinates. Also
#'   supports the keywords "right", "left", "bottom", "top" and "center".
#'   `x` specifies the scalebar center, `y` the scalebar baseline.
#' @param guide_x Overwrite for the x scale guide. Defaults to `"none"` so the
#'   in-panel scalebar replaces the axis guide. Use `NULL` to leave the existing
#'   x guide untouched.
#'
#' @export
geom_scalebar <- function(
    mapping = NULL,
    data = seqs(),
    length = 0.25,
    x = "right",
    y = "bottom",
    label = NULL,
    label_position = c("above", "below"),
    guide_x = "none",
    linewidth = NULL,
    color = NULL,
    colour = NULL,
    linetype = NULL,
    lineend = NULL,
    text_size = NULL,
    text_color = NULL,
    text_colour = NULL,
    family = NULL,
    fontface = NULL,
    tick_height = NULL,
    label_offset = NULL,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    show.legend = FALSE,
    inherit.aes = TRUE,
    ...
) {

  colour <- color %||% colour
  text_colour = text_color %||% text_colour
    
  x <- xy_numeric(x)
  y <- xy_numeric(y)
  
  label_position <- match.arg(label_position)

  .check_scalebar_length(length)

  default_aes <- ggplot2::aes(x, y, xend = xend)
  mapping <- aes_intersect(mapping, default_aes)

  layer <- ggplot2::layer(
    geom = GeomScalebar,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        length = length,
        label = label,
        x = x,
        y = y,
        label_position = label_position,
        linewidth = linewidth,
        colour = colour,
        linetype = linetype,
        lineend = lineend,
        text_size = text_size,
        text_colour = text_colour,
        family = family,
        fontface = fontface,
        tick_height = tick_height,
        label_offset = label_offset,
        na.rm = na.rm
      ),
      list(...)
    )
  )

  if (is.null(guide_x)) {
    layer
  } else {
    list(
      layer,
      ggplot2::guides(x = guide_x)
    )
  }
}

# =============================================================================
# ggproto implementations
# =============================================================================


#' @noRd
GuideScalebar <- ggplot2::ggproto(
  "GuideScalebar",
  ggplot2::GuideAxis,

  params = utils::modifyList(
    ggplot2::GuideAxis$params,
    list(
      name = "scalebar",
      length = 0.25,
      label = NULL,
      just = "right",
      label_position = "above",
      cap = "both",
      minor.ticks = FALSE
    ),
    keep.null = TRUE
  ),

  available_aes = "x",

  hashables = rlang::exprs(
    title,
    key$.value,
    key$.bar_label,
    label_position,
    name
  ),

  # Dedicated theme elements; unlike GuideAxis, we deliberately do not append
  # ".x.bottom" / ".x.top" suffixes.
  elements = list(
    line = "gggenomes.scalebar.line",
    text = "gggenomes.scalebar.text",
    ticks = "gggenomes.scalebar.line",
    major_length = "gggenomes.scalebar.tick.height",
    label_offset = "gggenomes.scalebar.label.offset"
  ),

  setup_elements = function(params, elements, theme) {
    ggplot2::Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    # Reuse GuideAxis' text alignment handling where appropriate.
    elements <- ggplot2::GuideAxis$override_elements(
      params, elements, theme
    )
    elements
  },

  extract_key = function(
      scale,
      aesthetic,
      length = 0.25,
      label = NULL,
      just = "right",
      ...
  ) {
    limits <- scale$get_limits()

    if (!is.numeric(limits) ||
        length(limits) != 2 ||
        any(!is.finite(limits))) {
      cli::cli_abort(
        "{.fn guide_scalebar} requires a continuous numeric x scale."
      )
    }

    limits <- range(limits)
    span <- diff(limits)

    if (!is.finite(span) || span <= 0) {
      cli::cli_abort(
        "{.fn guide_scalebar} requires an x scale with a positive range."
      )
    }

    bar_length <- resolve_scalebar_length(length, span)

    if (bar_length > span) {
      cli::cli_abort(
        "Scalebar length ({bar_length}) is larger than the x range ({span})."
      )
    }

    start <- switch(
      just,
      left = limits[1],
      center = mean(limits) - bar_length / 2,
      right = limits[2] - bar_length
    )

    values <- c(start, start + bar_length)

    key <- data.frame(
      value = scale$map(values),
      .value = values,
      .label = rep("", 2),
      .bar_label = rep(resolve_scalebar_label(label, bar_length), 2),
      .type = rep("major", 2),
      check.names = FALSE
    )

    names(key)[1] <- aesthetic
    key
  },

  # Draw horizontal bar plus an optional above-bar label.
  build_decor = function(decor, grobs, elements, params) {
    if (nrow(decor) == 0) {
      return(grid::nullGrob())
    }

    draw_scalebar_grob(
      x0 = min(decor$x),
      x1 = max(decor$x),
      y = mean(decor$y),
      label = params$key$.bar_label[[1]],
      line = elements$line,
      text = elements$text,
      tick_height = elements$major_length,
      label_offset = elements$label_offset,
      label_position = params$label_position,
      draw_ticks = FALSE, # build via build_ticks() in guide
      draw_label = params$label_position == "above"
    )
  },

  # Centered caps: same half-tick drawn once in each direction.
  build_ticks = function(
      key,
      elements,
      params,
      position = params$opposite
  ) {
    if (".type" %in% names(key)) {
      key <- key[key$.type == "major", , drop = FALSE]
    }

    if(length(elements$major_length) == 1){
      inward_length <- elements$major_length / 2
      outward_length <- elements$major_length / 2
    } else {
      inward_length <- elements$major_length[1]
      outward_length <- elements$major_length[2]
    }

    outward <- ggplot2::Guide$build_ticks(
      key,
      elements$ticks,
      params,
      position = position,
      length = outward_length
    )

    inward <- ggplot2::Guide$build_ticks(
      key,
      elements$ticks,
      params,
      position = position,
      length = -inward_length
    )

    grid::grobTree(
      outward,
      inward,
      name = "scalebar-caps"
    )
  },

  # Above labels are already drawn with the bar; below labels use the
  # normal GuideAxis label layout.
  build_labels = function(key, elements, params) {
    if (params$label_position == "above") {
      return(list(grid::nullGrob()))
    }

    if (".type" %in% names(key)) {
      key <- key[key$.type == "major", , drop = FALSE]
    }

    if (nrow(key) < 2) {
      return(list(grid::nullGrob()))
    }

    label_key <- key[1, , drop = FALSE]
    label_key[[params$aes]] <- mean(key[[params$aes]])
    label_key$.value <- mean(key$.value)
    label_key$.label <- key$.bar_label[[1]]

    ggplot2::GuideAxis$build_labels(
      label_key,
      elements,
      params
    )
  }
)


#' @noRd
GeomScalebar <- ggplot2::ggproto(
  "GeomScalebar",
  ggplot2::Geom,

  required_aes = c("x", "xend", "y"),
  default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank,

  draw_panel = function(
      data,
      panel_params,
      coord,
      length = 0.25,
      x = NULL,
      y = NULL,
      label = NULL,
      label_position = "above",
      linewidth = NULL,
      colour = NULL,
      linetype = NULL,
      lineend = NULL,
      text_size = NULL,
      text_colour = NULL,
      family = NULL,
      fontface = NULL,
      tick_height = NULL,
      label_offset = NULL,
      na.rm = FALSE
  ) {
    if (!coord$is_linear()) {
      cli::cli_abort(
        "{.fn geom_scalebar} currently requires linear coordinates."
      )
    }

    ranges <- coord$backtransform_range(panel_params)

    xr <- range(ranges$x)
    yr <- range(ranges$y)
    xspan <- diff(xr)
    
    if (!is.finite(xspan) || xspan <= 0) {
      return(grid::nullGrob())
    }

    bar_length <- resolve_scalebar_length(length, xspan)

    if (bar_length > xspan) {
      cli::cli_abort(
        "Scalebar length ({bar_length}) is larger than the displayed x range ({xspan})."
      )
    }

    bar_label <- resolve_scalebar_label(label, bar_length)

    # -----------------------------------------------------------------------
    # Horizontal placement
    # -----------------------------------------------------------------------

    # work in absolute coordinates first to get the inward justification right
    # convert relative x to absolute
    if (0 <= x && x <= 1) {
      x <- xr[1] + x * xspan
    }

    x0 <- x - bar_length / 2
    x1 <- x + bar_length / 2

    # Shift, rather than truncate, if automatic placement extends off-panel.
    if (x0 < xr[1]) {
      shift <- xr[1] - x0
      x0 <- x0 + shift
      x1 <- x1 + shift
    }

    if (x1 > xr[2]) {
      shift <- x1 - xr[2]
      x0 <- x0 - shift
      x1 <- x1 - shift
    }

    # Convert genomic x positions to panel-relative coordinates.
    x_trans <- coord$transform(
      data.frame(
        x = c(x0, x1),
        y = rep(mean(yr), 2)
      ),
      panel_params
    )$x

    # -----------------------------------------------------------------------
    # Vertical placement
    # -----------------------------------------------------------------------

    # Convert genomic y positions to panel-relative coordinates.
    if (y > 1 || y < 0) {
      y_trans <- coord$transform(
        data.frame(
          x = mean(xr),
          y = y
        ),
        panel_params
      )$y
    } else {
      y_trans <- y
    }

    style <- scalebar_style(
      linewidth = linewidth,
      colour = colour,
      linetype = linetype,
      lineend = lineend,
      text_size = text_size,
      text_colour = text_colour,
      family = family,
      fontface = fontface,
      tick_height = tick_height,
      label_offset = label_offset
    )

    draw_scalebar_grob(
      x0 = x_trans[1],
      x1 = x_trans[2],
      y = y_trans,
      label = bar_label,
      line = style$line,
      text = style$text,
      tick_height = style$tick_height,
      label_offset = style$label_offset,
      label_position = label_position,
      draw_ticks = TRUE,
      draw_label = TRUE
    )
  }
)


# =============================================================================
# Shared/internal helpers
# =============================================================================


.scalebar_defaults <- list(
  linewidth = 1,
  colour = "black",
  linetype = 1,
  lineend = "butt",
  text_size = 10,
  text_colour = "black",
  family = "",
  fontface = "plain",
  tick_height = 0,
  label_offset = 6
)


.scalebar_or <- function(x, y) {
  if (is.null(x)) y else x
}


.as_scalebar_unit <- function(x, unit = "pt") {
  if (grid::is.unit(x)) x else grid::unit(x, unit)
}


.check_scalebar_length <- function(x) {
  if (length(x) != 1 || !is.numeric(x) || !is.finite(x) || x <= 0) {
    cli::cli_abort(
      "{.arg length} must be a positive finite number."
    )
  }
  invisible(x)
}


.check_npc_position <- function(x, arg) {
  if (is.null(x)) {
    return(invisible(NULL))
  }

  if (length(x) != 1 || !is.numeric(x) || !is.finite(x) ||
      x < 0 || x > 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number between 0 and 1."
    )
  }

  invisible(x)
}

#' @keywords internal
xy_numeric <- function(k){
  if(is.numeric(k)) return(k)
  k <- match.arg(k, c("right", "center", "left", "bottom", "top"))
  switch(k,
    top = 1,
    bottom = 0,
    center = 0.5,
    left = 0,
    right = 1
  )
}

#' Pick a nice scalebar length
#'
#' Returns the largest 1, 2, or 5 x 10^n value <= x.
#'
#' @param x Positive numeric value.
#' @return A numeric scalar.
#' @keywords internal
nice_scalebar_length <- function(x) {
  if (length(x) != 1 || !is.finite(x) || x <= 0) {
    cli::cli_abort(
      "{.arg x} must be a positive finite number."
    )
  }

  magnitude <- 10^floor(log10(x))
  candidates <- c(1, 2, 5, 10) * magnitude

  max(candidates[candidates <= x])
}


#' Resolve relative or absolute scalebar length
#'
#' `0 < length < 1` is interpreted as a target fraction of `span`, rounded
#' down to a nice value. `length >= 1` is interpreted as absolute x units.
#'
#' @keywords internal
resolve_scalebar_length <- function(length = 0.25, span) {
  .check_scalebar_length(length)

  if (length < 1) {
    nice_scalebar_length(span * length)
  } else {
    length
  }
}

#' Resolve scalebar label
#'
#' @keywords internal
resolve_scalebar_label <- function(label, length) {
  if (is.null(label)) {
    format_bp(length)
  } else if (is.function(label)) {
    label(length)
  } else {
    as.character(label)
  }
}


#' Resolve scalebar visual styling
#'
#' Shared by the geom and theme defaults.
#'
#' @keywords internal
scalebar_style <- function(
    linewidth = NULL,
    colour = NULL,
    linetype = NULL,
    lineend = NULL,
    text_size = NULL,
    text_colour = NULL,
    family = NULL,
    fontface = NULL,
    tick_height = NULL,
    label_offset = NULL
) {
  d <- .scalebar_defaults

  list(
    line = ggplot2::element_line(
      linewidth = .scalebar_or(linewidth, d$linewidth),
      colour = .scalebar_or(colour, d$colour),
      linetype = .scalebar_or(linetype, d$linetype),
      lineend = .scalebar_or(lineend, d$lineend)
    ),

    text = ggplot2::element_text(
      size = .scalebar_or(text_size, d$text_size),
      colour = .scalebar_or(text_colour, d$text_colour),
      family = .scalebar_or(family, d$family),
      face = .scalebar_or(fontface, d$fontface)
    ),

    tick_height = .as_scalebar_unit(
      .scalebar_or(tick_height, d$tick_height)
    ),

    label_offset = .as_scalebar_unit(
      .scalebar_or(label_offset, d$label_offset)
    )
  )
}


#' Draw a complete scalebar grob
#'
#' x0/x1/y are panel-relative coordinates in npc units.
#'
#' @keywords internal
draw_scalebar_grob <- function(
    x0,
    x1,
    y,
    label = NULL,
    line,
    text,
    tick_height,
    label_offset,
    label_position = c("above", "below"),
    draw_ticks = TRUE,
    draw_label = TRUE
) {
  label_position <- match.arg(label_position)

  grobs <- list()
  y0 <- grid::unit(y, "npc")

  if (!inherits(line, "element_blank")) {
    grobs <- c(
      grobs,
      list(
        ggplot2::element_grob(
          line,
          x = grid::unit(c(x0, x1), "npc"),
          y = grid::unit(c(y, y), "npc")
        )
      )
    )
  }

  if (draw_ticks && !inherits(line, "element_blank")) {

    grobs <- c(
      grobs,
      list(
        ggplot2::element_grob(
          line,
          x = grid::unit(c(x0, x0, x1, x1), "npc"),
          y = grid::unit.c(
            y0 - tick_height,
            y0 + tick_height,
            y0 - tick_height,
            y0 + tick_height
          ),
          id.lengths = c(2L, 2L)
        )
      )
    )
  }

  if (draw_label &&
      !is.null(label) &&
      !inherits(text, "element_blank")) {

    if (label_position == "above") {
      label_y <- y0 + label_offset
      vjust <- 0
    } else {
      label_y <- y0 - label_offset
      vjust <- 1
    }

    grobs <- c(
      grobs,
      list(
        ggplot2::element_grob(
          text,
          label = label,
          x = grid::unit(mean(c(x0, x1)), "npc"),
          y = label_y,
          hjust = 0.5,
          vjust = vjust,
          margin_y = FALSE
        )
      )
    )
  }

  if (!length(grobs)) {
    return(grid::nullGrob())
  }

  do.call(grid::grobTree, grobs)
}