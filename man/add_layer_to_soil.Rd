% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_Soils_Functions.R
\name{add_layer_to_soil}
\alias{add_layer_to_soil}
\title{Split soil layer in two layers}
\usage{
add_layer_to_soil(x, il, w, method = c("interpolate", "exhaust"))
}
\arguments{
\item{x}{A numeric data.frame or matrix. Columns are soil layers.}

\item{il}{An integer value. The column/soil layer number after which a new
layer is added.}

\item{w}{A numeric vector of length one or two. The weights used to calculate
the values of the new layer.}

\item{method}{A character string. See \code{Details}.}
}
\value{
An object like x with one column more at position \code{il + 1}.
}
\description{
Split soil layer in two layers
}
\section{Details}{
 If the weight vector is of length one and \code{x} contains
  a row with name 'depth_cm', then it is assumed that the value of \code{w}
  corresponds to the weight of the first layer and the weight of the second
  layer is calculated as \code{(depth of first layer of x) - (first value of
  w)}. If this is case and if the added layer is either more shallow or
  deeper than any input layers, then the depth of the added layer is
  calculated proportionally if \code{sum(w) <= 1} otherwise additively.

 The method \code{interpolate} calculates the weighted mean
  of the columns/layers \code{il} and \code{il + 1}. If \code{il == 0}, i.e.,
  add layer at a more shallow depth than any existing layer, then values from
  the previously first layer are copied to the newly created layer. The
  method \code{exhaust} distributes the value of \code{il + 1} according to
  the weights.
}

