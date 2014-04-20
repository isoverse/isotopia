\name{fractionate}
\alias{fractionate}
\title{Fractionate an isotopic value}
\usage{
fractionate(frac, iso)
}
\arguments{
  \item{frac}{the isotope object used to fractionate the
  isotope value}

  \item{iso}{the isotope object to fractionate}
}
\value{
an object of the same type as \code{iso}
}
\description{
This function calculates the outcome of isotopic
fractionation on an isotopic value and is defined for a
number of different combinations (how an
\code{\link{alpha}} fractionation factor fractionates a
single \code{\link{ratio}} or a \code{\link{delta}} value,
how it fractionates another \code{\link{alpha}} value; how
a \code{\link{epsilon}} value does the same, etc.)
}
\note{
Several of these calculations are also implemented with an
\code{\link{arithmetic}} shorthand. All calculatinos are
only permissible if the fractionation factors and isotope
values have matching attributes.
}

