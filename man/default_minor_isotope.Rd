\name{default_minor_isotope}
\alias{default_minor_isotope}
\title{Set the default minor isotope}
\usage{
default_minor_isotope(minor)
}
\arguments{
  \item{minor}{the default minor isotope name}
}
\description{
Set a default to be used with all new isotope value
objects, that are initialized without a specified minor
isotope. To disable, set \code{default_minor_isotope("")}.
To retrieve currently set value, call
\code{default_minor_isotope()} without any parameters.
}

