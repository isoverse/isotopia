\name{as.epsilon}
\alias{as.epsilon}
\title{Convert to epsilon value}
\usage{
as.epsilon(iso, permil = use_permil())
}
\arguments{
  \item{iso}{isotopic data object (e.g.
  \code{\link{alpha}})}

  \item{permil}{whether to generate epsilon object in
  permil values (x1000) or not}
}
\value{
isotope \code{\link{epsilon}} object if iso can be
converted to an \code{\link{epsilon}}, an error otherwise
}
\description{
\code{as.delta} converts another isotopic data type to an
epsilon values
}
\seealso{
Other data type conversions: \code{\link{as.abundance}};
\code{\link{as.alpha}}; \code{\link{as.delta}};
\code{\link{as.ratio}}; \code{\link{as.value}};
\code{\link{as.weighted_value}}; \code{\link{as.weight}}
}

