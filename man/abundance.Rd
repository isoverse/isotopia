\name{abundance}
\alias{abundance}
\title{Fractional abundance}
\usage{
abundance(..., major = "", compound = "", weight = numeric(),
  single_as_df = FALSE)
}
\arguments{
  \item{...}{- numeric vectors (can be named) to turn into
  isotope abundance objects}

  \item{major}{- name of the major isotope in the isotope
  system [optional], only of importance if converting from
  abundance to ratio or delta value, and want automatic
  name propagation}

  \item{compound}{- name of the compound the isotopic
  values belong to [optional]}
}
\description{
Generate an isotope abundance object. See \link{isotopia}
for general information on initializing and converting
isotope data objects.
}
\seealso{
Other isotope data types: \code{\link{alpha}};
\code{\link{delta}}, \code{\link{use_permil}};
\code{\link{epsilon}}, \code{\link{use_permil}};
\code{\link{intensity}}; \code{\link{ratio}}
}

