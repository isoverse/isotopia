#' R interface for working with isotopic data (abundances, ratios, delta values, etc.). 
#' 
#' This package provides several isotopic data types that can be initialized 
#' by calling the respective \code{\link{ratio}}, \code{\link{abundance}},
#' \code{\link{delta}}, \code{\link{fractionation_factor}} and \code{\link{intensity}} functions. 
#' Each data type
#' has additional attributes (such as name of the major isotope for all data
#' types, reference ratio for \code{\link{delta}} values, notation for \code{\link{delta}}
#' and \code{\link{fractionation_factor}}, unit for \code{\link{intensity}})
#' and these are described in detail in the help for each function. The attributes of any
#' existing isotope data object can be modified easily by calling \code{\link{set_attrib}}
#' 
#' Each data type can be initialized as a single vector of isotopic data or an entire system
#' of isotope values for the same element (e.g. all oxygen or all sulfur isotopes). To
#' intialize an isotope system, simply pass multiple named data vectors with the same number
#' of data points to the initialization functions (please see examples
#' for details). Isotope systems are returned as a \code{data.frame} with all the different 
#' components of the system
#' as separate columns. This object can be treated and manipulated just like a 
#' regular R data.frame. The column headers are named after the individual named
#' data vectors (e.g. \code{ratio(`34S` = 0.1, `33S` = 0.2)} will produce a data.frame
#' with columns \code{34S} and \code{33S}) - careful if using 
#' names like '12C' that start with a number, they are not syntactically valid
#' variable names in R and must be back quoted as \code{`34S`}. Isotope data objects in
#' an isotope system that are not named generate columns named \code{iso, iso.1, iso.2, ...}.
#' 
#' Isotope data objects (both single vectors and isotope systems) can then be converted 
#' to different data types using the respective \code{\link{to_ratio}}, \code{\link{to_abundance}},
#' \code{\link{to_delta}} functions. Notations can also be changed using \code{\link{switch_notation}}
#' 
#' Global options for isotopia can be set using \code{\link{set_iso_opts}} and 
#' standard reference ratios can be registered using \code{\link{register_standard}}
#' 
#' @name isotopia-package
#' @aliases isotopia
#' @docType package
#' @title isotopia package
#' @author Sebastian Kopf
#' @seealso \code{\link{ratio}}, \code{\link{is.ratio}}, \code{\link{to_ratio}}, etc.
#' @examples
#' # these examples are for initializing isotope ratio objects but apply equally to other data types
#' ratio(0.1) # single value
#' ratio(c(0.1, 0.2, 0.3)) # multiple values
#' ratio(`13C` = c(0.1, 0.2, 0.3)) # named ratio
#' ratio(`33S` = c(0.1, 0.2, 0.3), `34S` = c(0.2, 0.4, 0.6), major = "32S") # isotope system
NULL

#' @include options.R
#' @include operations.R
#' @include arithmetic.R
#' @include initialization.R
NULL

