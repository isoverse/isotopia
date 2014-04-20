\name{register_standard}
\alias{get_standards}
\alias{register_standard}
\alias{standards}
\title{Register an isotope standard}
\usage{
register_standard(ratio)

get_standards(minor = NULL, major = NULL, name = NULL)
}
\arguments{
  \item{ratio}{- a ratio object with minor, and major
  isotope as well as compound set, can be converted from
  another isotope object if desired (e.g. a measured delta
  value or an abundance)}

  \item{minor}{- character vector of minor isotope names to
  search for}

  \item{major}{- character vector of major isotope names to
  search for}

  \item{name}{- character vector of standards names to
  search for}
}
\value{
list of ratio objects
}
\description{
Use this function to register an isotope standard. This can
be useful for keeping track of standards you use internally
and will also allow conversions from \code{\link{delta}} to
e.g. \code{\link{ratio}} to automatically try to find the
approriate standard ratio from the registered values.

This function retrieves any number of registered isotope
standards that can be identified with the provided search
terms.
}

