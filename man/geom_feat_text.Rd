% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_gene_text.R
\name{geom_feat_text}
\alias{geom_feat_text}
\alias{geom_feat_tag}
\alias{geom_feat_note}
\alias{geom_gene_text}
\alias{geom_gene_tag}
\alias{geom_gene_note}
\title{Add text to genes, features, etc.}
\usage{
geom_feat_text(
  mapping = NULL,
  data = feats(),
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_feat_tag(
  mapping = NULL,
  data = feats(),
  stat = "identity",
  position = "identity",
  hjust = 0,
  vjust = 0,
  angle = 45,
  nudge_y = 0.03,
  xjust = 0.5,
  strandwise = TRUE,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_feat_note(
  mapping = NULL,
  data = feats(),
  stat = "identity",
  position = "identity",
  hjust = 0,
  vjust = 1,
  nudge_y = -0.03,
  xjust = 0,
  strandwise = FALSE,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_gene_text(
  mapping = NULL,
  data = genes(),
  stat = "identity",
  position = "identity",
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_gene_tag(
  mapping = NULL,
  data = genes(),
  stat = "identity",
  position = "identity",
  hjust = 0,
  vjust = 0,
  angle = 45,
  nudge_y = 0.03,
  xjust = 0.5,
  strandwise = TRUE,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_gene_note(
  mapping = NULL,
  data = genes(),
  stat = "identity",
  position = "identity",
  hjust = 0,
  vjust = 1,
  nudge_y = -0.03,
  xjust = 0,
  strandwise = FALSE,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function. Cannot be jointy specified with
\code{nudge_x} or \code{nudge_y}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{parse}{If \code{TRUE}, the labels will be parsed into expressions and
displayed as described in \code{?plotmath}.}

\item{check_overlap}{If \code{TRUE}, text that overlaps previous text in the
same layer will not be plotted. \code{check_overlap} happens at draw time and in
the order of the data. Therefore data should be arranged by the label
column before calling \code{geom_label()} or \code{geom_text()}.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{nudge_y}{Horizontal and vertical adjustment to nudge labels by.
Useful for offsetting text from points, particularly on discrete scales.
Cannot be jointly specified with \code{position}.}
}
\description{
Add text to genes, features, etc.
}
