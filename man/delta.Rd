\name{delta}
\alias{delta}
\alias{use_permil}
\title{Delta value}
\usage{
delta(..., major = default_major_isotope(), compound = "", ref = "",
  ref_ratio = numeric(), permil = use_permil(), weight = numeric(),
  single_as_df = FALSE)

use_permil(permil)
}
\arguments{
  \item{...}{- numeric vectors (can be named) to turn into
  delta values}

  \item{major}{- name of the major isotope in the isotope
  system [optional]}

  \item{compound}{- name of the compound the isotopic
  values belong to [optional]}

  \item{ref}{- name of the reference material}

  \item{ref_ratio}{- value of the reference material}

  \item{permil}{- whether the values passed in are in
  permil or raw values (i.e. no 1000x multiplication)}

  \item{weight}{- weight the isotope value (with a mass,
  concentration, etc.) for easy mass balance calculations.
  The default value is 1, i.e. an unweighted isotope value.
  If specified, \code{weight} must be a single value or a
  numeric vector of the same size as the data values. The
  weight of an isotope value obejct can be retrieved and
  (re)set with the \code{\link{weight}} function.}
}
\description{
Generate an isotope delta value object. See \link{isotopia}
for general information on initializing and converting
isotope data objects.
}
\details{
For mass balance calculations with delta values, simply add
the appropriate weights (if different from the default) and
use \code{delta(...) + delta(...)}. Use
\code{\link{exact_mass_balance}(TRUE/FALSE)} to adjust how
these are calculated.
}
\note{
\code{use_permil} is a function to globally enable/disable
the use of permil values in conversions from/to delta and
epsilon values. It can always be overwritten in individual
conversions using the \code{permil} parameter.
}
\examples{
delta(50, permil = TRUE) # enter as permil value
delta(0.05, permil = FALSE) # enter as non-permil value
}
\seealso{
Other isotope data types: \code{\link{abundance}};
\code{\link{alpha}}; \code{\link{epsilon}},
\code{\link{use_permil}}; \code{\link{intensity}};
\code{\link{ratio}}
}

