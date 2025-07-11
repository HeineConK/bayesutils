% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot.dens}
\alias{plot.dens}
\title{Plot Density Curve with Optional Histogram}
\usage{
\method{plot}{dens}(
  x,
  normalize = NULL,
  show.hist = F,
  breaks.hist = 10,
  border.hist = FALSE,
  col.hist = "grey80",
  col = 1,
  lwd = 2,
  y0 = 0,
  xlim,
  ylim,
  xlab = "x",
  ylab = "density",
  ...
)
}
\arguments{
\item{x}{Numeric vector of values.}

\item{normalize}{Logical; whether to normalize the density.}

\item{show.hist}{Logical; whether to overlay a histogram.}

\item{breaks.hist}{Number of histogram bins.}

\item{border.hist}{Border color for histogram.}

\item{col.hist}{Fill color for histogram.}

\item{col}{Color for density line.}

\item{lwd}{Line width for density.}

\item{y0}{Vertical offset.}

\item{xlim, }{ylim Axis limits.}

\item{xlab, }{ylab Axis labels.}

\item{...}{Additional plot arguments.}
}
\value{
A base R plot with density and optional histogram.
}
\description{
Plots a kernel density estimate with optional histogram overlay and eye-style shapes.
}
