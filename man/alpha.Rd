\name{alpha}
\alias{alpha}
\title{Alpha value}
\usage{
alpha(..., major = "", ctop = "", cbot = "", single_as_df = FALSE)
}
\arguments{
  \item{...}{- numeric vectors (can be named) to turn into
  alpha values}

  \item{major}{- name of the major isotope in the isotope
  system [optional]}

  \item{ctop}{- name of the compound representing the top
  isotope ratio [optional]}

  \item{cbot}{- name of the compound representing the
  bottom isotope ratio [optional]}
}
\description{
Generate a fractionation factor (alpha value = ratio of two
isotope ratios) object. See \link{isotopia} for general
information on initializing and converting isotope data
objects.
}
\seealso{
Other isotope data types: \code{\link{abundance}};
\code{\link{delta}}, \code{\link{use_permil}};
\code{\link{epsilon}}, \code{\link{use_permil}};
\code{\link{intensity}}; \code{\link{ratio}}
}

