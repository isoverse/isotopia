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

#' @include operations.R
#' @include arithmetic.R
#' @include initialization.R
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

#' Set the default minor isotope
#' 
#' Set a default to be used with all new isotope value objects,
#' that are initialized without a specified minor isotope. To
#' disable, set \code{default_minor_isotope("")}. To retrieve
#' currently set value, call \code{default_minor_isotope()} without
#' any parameters.
#' 
#' @param minor the default minor isotope name
#' @export
default_minor_isotope <- function(minor) {
    if (!missing(minor)) {
        if (!is.character(minor) || length(minor) == 0)
            stop("not a valid default name for minor isotopes")
        options(default_minor_isotope = minor)
        return(invisible(minor))
    } else {
        minor <- options("default_minor_isotope")[[1]]
        return (ifelse(is.null(minor), "", minor))
    }
}


#' Set the default major isotope
#' 
#' Set a default to be used with all new isotope value objects,
#' that are initialized without a specified major isotope. To
#' disable, set \code{default_major_isotope("")}. To retrieve
#' currently set value, call \code{default_major_isotope()} without
#' any parameters.
#' 
#' @param major the default major isotope name
#' @export
default_major_isotope <- function(major) {
    if (!missing(major)) {
        if (!is.character(major) || length(major) == 0)
            stop("not a valid default name for major isotopes")
        options(default_major_isotope = major)
        return(invisible(major))
    } else {
        major <- options("default_major_isotope")[[1]]
        return (ifelse(is.null(major), "", major))
    }
}

#' Register an isotope standard
#' 
#' Use this function to register an isotope standard. This can be useful
#' for keeping track of standards you use internally and will also allow
#' conversions from \code{\link{delta}} to e.g. \code{\link{ratio}} to 
#' automatically try to find the approriate standard ratio from the 
#' registered values.
#' 
#' @param ratio - a ratio object with minor, and major isotope as well as compound set,
#' can be converted from another isotope object if desired (e.g. a measured delta
#' value or an abundance)
#' @aliases standards
#' @export
#' @rdname standards
register_standard <- function(ratio) {
    if (!is.ratio(ratio))
        stop("can only register standards that are ratio isotope objects")
    
    if (length(ratio) != 1L)
        stop("must be a single ratio value, found ", length(ratio))
    
    if (nchar(ratio@isoname) == 0 || nchar(ratio@major) == 0 || nchar(ratio@compound) == 0)
        stop("can only register ratios that have minor, major isotope and compound name set")
    
    if (is.null(refs <- options("isotope_standards")[[1]]))  
        refs <- data.frame(minor = character(), major = character(), name = character(), ratio = numeric(), stringsAsFactors = F)
    
    index <- which(refs$minor == ratio@isoname & refs$major == ratio@major & refs$name == ratio@compound)
    if (length(index) > 1)
        stop("more than one reference already exists with these characteristics, they must be unique!")
    else if (length(index) == 1) {
        index <- index
        if (get_value(ratio) != refs$ratio[index])
            warning("overwriting an existing standard with ratio: ", refs$ratio[index])
    } else
        index <- nrow(refs) + 1
    
    refs[index,] <- list(minor = ratio@isoname, major = ratio@major, name = ratio@compound, ratio = get_value(ratio))
    options(isotope_standards = refs)
    invisible(get_standards())
}


#' Retrive registered isotope standards
#' 
#' This function retrieves any number of registered isotope standards
#' that can be identified with the provided search terms.
#' @param minor - character vector of minor isotope names to search for
#' @param major - character vector of major isotope names to search for
#' @param name - character vector of standards names to search for
#' @return list of ratio objects
#' @export 
#' @rdname standards
get_standards <- function(minor = NULL, major = NULL, name = NULL) {
    if (is.null(refs <- options("isotope_standards")[[1]]))
        return (list())

    index <- rep(TRUE, nrow(refs))
    if (!is.null(minor))
        index <- index & refs$minor %in% minor
    if (!is.null(major))
        index <- index & refs$major %in% major
    if (!is.null(name))
        index <- index & refs$name %in% name
    stds <- list()
    for (i in which(index)) {
        stds <- c(stds, list(update_iso(ratio(refs[i, "ratio"]), 
            list(isoname = refs[i, "minor"], major = refs[i, "major"], compound = refs[i, "name"]))))
    }
    stds
}

