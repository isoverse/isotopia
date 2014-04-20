#' R interface for working with isotopic data (abundances, ratios, delta values, etc.). 
#' 
#' This package provides several isotopic data types that can be initialized 
#' by calling the respective \code{\link{ratio}}, \code{\link{abundance}},
#' \code{\link{delta}} and \code{\link{intensity}} functions. Each data type
#' has additional attributes (such as name of the major isotope for all data
#' types, reference ratio for \code{\link{delta}} values, unit for \code{\link{intensity}})
#' and these are described in detail in the help for each function. The attributes of any
#' existing isotope data object can be modified easily by calling the initialization
#' function (\code{\link{ratio}}, \code{\link{abundance}}, etc.) again and passing the object as 
#' well as any of the attributes (such as \code{major}) to modify.
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
#' \code{\link{to_delta}} functions. 
#' 
#' 
#' @name isotopia-package
#' @aliases isotopia
#' @docType package
#' @title isotopia package
#' @author Sebastian Kopf
#' @seealso \code{\link{ratio}}, \code{\link{is.ratio}}, \code{\link{as.ratio}}, etc.
#' @examples
#' # these examples are for initializing isotope ratio objects but apply equally to other data types
#' ratio(0.1) # single value
#' ratio(c(0.1, 0.2, 0.3)) # multiple values
#' ratio(`13C` = c(0.1, 0.2, 0.3)) # named ratio
#' ratio(`33S` = c(0.1, 0.2, 0.3), `34S` = c(0.2, 0.4, 0.6), major = "32S") # isotope system
NULL

#' @include arithmetic.R
NULL

#' @note
#' \code{exact_mass_balance} is a function to enable/disable exact mass balance calculations.
#' 
#' If enabled, mass balance calculations with delta values (i.e. \code{mass_balance(delta, delta, delta...} 
#' or \code{\link{delta}() + \link{delta}()}) will
#' be performed exact by converting to natural abundances first and making the addition in 
#' abundance space (will be converted back to delta value afterwards). 
#' This is only possible if the \code{ref_ratio} in the delta values is
#' set and will lead to an error if attempted without the reference ratios set.
#' 
#' If disabled, mass balance calculations with delta values (\code{\link{delta}() + \link{delta}()}) will
#' be performed in delta space (which is not exact but the discrepancy is negligible unless
#' the minor isotopes in an isotope system make up a significant portion)
#' 
#' @rdname mass_balance
#' @export
exact_mass_balance <- function(exact) {
    if (!missing(exact)) {
        options(exact_mass_balance = exact)
        return(invisible(exact))
    } else
        return(options("exact_mass_balance")[[1]])
}
exact_mass_balance(FALSE)

#' @note
#' \code{use_permil} is a function to globally enable/disable the use of permil values
#' in conversions from/to delta and epsilon values. It can always be overwritten in
#' individual conversions using the \code{permil} parameter.
#' @usage use_permil(permil)
#' 
#' @name use_permil
#' @rdname delta
NULL

#' @note
#' \code{use_permil} is a function to globally enable/disable the use of permil values
#' in conversions from/to delta and epsilon values. It can always be overwritten in
#' individual conversions using the \code{permil} parameter.
#' 
#' @rdname epsilon
#' @export
use_permil <- function(permil) {
    if (!missing(permil)) {
        options(use_permil = permil)
        return(invisible(permil))
    } else
        return(options("use_permil")[[1]])
}
use_permil(TRUE)
