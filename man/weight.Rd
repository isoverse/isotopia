\name{weight}
\alias{weight}
\title{Weight an isotope value object}
\usage{
weight(iso, weight)
}
\arguments{
  \item{iso}{- object to get weight or add weight}

  \item{weight}{- vector of weight values, has to be a
  single value or the same length as the data stored in the
  isotope value object.}
}
\description{
\code{weight(iso, weight)} adds a weight (can be thought of
as mass or concentration) to an isotopic value which will
be used to weigh the isotope value when adding together
multiple isotope values. \code{\link{as.weight}(iso)}
returns the weight of an isotope value object.
}
\note{
This can also be achieved when first initializing (or
updating) an object via calls to \code{\link{ratio}},
\code{\link{abundance}}, \code{\link{delta}}, etc.
}
\examples{
r <- ratio(0.2)
r <- weight(r, 10)
print(weight(r)) # returns 10
}

