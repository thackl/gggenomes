% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_wiggle.R
\name{geom_wiggle}
\alias{geom_wiggle}
\title{Draw wiggle ribbons or lines}
\usage{
geom_wiggle(
  mapping = NULL,
  data = feats(),
  stat = "wiggle",
  geom = "ribbon",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  offset = 0,
  height = 0.8,
  bounds = Hmisc::smedian.hilow,
  ...
)
}
\arguments{
\item{offset}{distance between seq center and wiggle mid.}

\item{height}{distance in plot between wiggle low and high.}

\item{bounds}{mid, low and high boundary value for plotting wiggle data. Can
be a function or a vector returning those three values. Defaults to
\link[Hmisc:smean.sd]{Hmisc::smedian.hilow}.}
}
\description{
Visualize data that varies along sequences as ribbons, lines, lineranges,
etc.
}
\section{Aesthetics}{
 \code{geom_wiggle()} understands aesthetics depending on the
chosen underlying ggplot geom, by default \code{\link[ggplot2:geom_ribbon]{ggplot2::geom_ribbon()}}. Other
options that play well are for example \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}},
\code{\link[ggplot2:geom_linerange]{ggplot2::geom_linerange()}}, \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}. The only required
aesthetic is:
\itemize{
\item \strong{z}
}
}

\examples{
# Plot varying GC-content along sequences as ribbon
gggenomes(seqs=emale_seqs, feats=emale_gc) +
  geom_wiggle(aes(z=score)) +
  geom_seq()

# customize color and position
gggenomes(genes=emale_genes, seqs=emale_seqs, feats=emale_gc) +
  geom_wiggle(aes(z=score), fill="lavenderblush3", offset=-.3, height=.5) +
  geom_seq() + geom_gene()

# GC-content as line and with variable color
gggenomes(seqs=emale_seqs, feats=emale_gc) +
  geom_wiggle(aes(z=score, color=score), geom="line", bounds=c(.5,0,1)) +
  geom_seq() +
  scale_colour_viridis_b(option="A")

# or as lineranges
gggenomes(seqs=emale_seqs, feats=emale_gc) +
  geom_wiggle(aes(z=score, color=score), geom="linerange") +
  geom_seq() +
  scale_colour_viridis_b(option="A")
}