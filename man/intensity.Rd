\name{intensity}
\alias{intensity}
\title{Ion intensity}
\usage{
intensity(..., major = default_major_isotope(), compound = "", unit = "",
  single_as_df = FALSE)
}
\arguments{
  \item{...}{- numeric vectors (can be named) to turn into
  ion intensity objects}

  \item{major}{- name of the major isotope in the isotope
  system [optional],}

  \item{compound}{- name of the compound the isotopic
  values belong to [optional]}

  \item{unit}{- units of the measurement (e.g. #, V, mV)}
}
\description{
Generate an ion intensity object (e.g. ion counts or signal
intensity). See \link{isotopia} for general information on
initializing and converting isotope data objects.
}
\seealso{
Other isotope data types: \code{\link{abundance}};
\code{\link{alpha}}; \code{\link{delta}},
\code{\link{use_permil}}; \code{\link{epsilon}},
\code{\link{use_permil}}; \code{\link{ratio}}
}

