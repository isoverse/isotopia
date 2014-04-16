#' R interface for working with isotopic data (abundances, ratios, delta values, etc.). 
#' 
#' This package provides several isotopic data types that can be initialized 
#' by calling the respective \code{\link{ratio}}, \code{\link{abundance}},
#' \code{\link{delta}} and \code{\link{intensity}} functions. Each data type
#' has additional attributes (such as name of the major isotope for all data
#' types, reference ratio for \code{\link{delta}} values, unit for \code{\link{intensity}}
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
#' @title isotoper package
#' @author Sebastian Kopf
#' @seealso \code{\link{ratio}}, \code{\link{is_ratio}}, \code{\link{to_ratio}}, etc.
#' @examples
#' # these examples are for initializing isotope ratio objects but apply equally to other data types
#' ratio(0.1) # single value
#' ratio(c(0.1, 0.2, 0.3) # multiple values
#' ratio(`13C` = c(0.1, 0.2, 0.3)) # named ratio
#' ratio(`33S` = c(0.1, 0.2, 0.3), `34S` = c(0.2, 0.4, 0.6), major = "32S") # isotope system
NULL

#' @include conversion.R
NULL

#' Isotope ratio
#'
#' Generate isotope ratio objects. See \link{isotopia} for general information on initializing
#' and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into isotope ratio objects
#' @param major - name of the major isotope in the single ratio or isotope system [optional]
#' @family isotope data types
#' @export
#' @examples
#' ratio(0.1) # single value
#' ratio(c(0.1, 0.2, 0.3) # multiple values
#' ratio(`13C` = c(0.1, 0.2, 0.3)) # named ratio
#' ratio(`33S` = c(0.1, 0.2, 0.3), `34S` = c(0.2, 0.4, 0.6), major = "32S") # isotope system
ratio <- function(..., major = "", single_as_df = FALSE) {
    iso("Ratio", "Ratios", ..., attribs = list(major = major), single_as_df = single_as_df)
}

#' Fractional abundance
#'
#' Generate an isotope abundance object. See \link{isotopia} for general information on initializing
#' and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into isotope abundance objects
#' @param major - name of the major isotope in the isotope system [optional], 
#' only of importance if converting from abundance to ratio or delta value, 
#' and want automatic name propagation
#' @family isotope data types
#' @export
abundance <- function(..., major = "", single_as_df = FALSE) {
    iso("Abundance", "Abundances", ..., attribs = list(major = major), single_as_df = single_as_df)
}


delta <- function(x) {
    
}

#' Ion intensity
#' 
#' Generate an ion intensity object (e.g. ion counts or signal intensity). 
#' See \link{isotopia} for general information on initializing and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into ion intensity objects
#' @param major - name of the major isotope in the isotope system [optional], 
#' @param unit - units of the measurement (e.g. #, V, mV)
#' @family isotope data types
#' @export
intensity <- function(..., major = "", unit = "", single_as_df = FALSE) {
    new("Intensity", "Intensities", ..., 
        attribs = list(major = major, unit = unit), single_as_df = single_as_df)
}

#' create an isotope value object (this function is not exported and should
#' be access via the appropriate wrapper functions, e.g. \code{ratio}, \code{abundance}, etc.)
#' @param class.isoval name of the class for a single value
#' @param class.isosys name of the class for an isotope system
#' @param attribs named list of attributes to pass to the isotope data object constructors
#' @param ... values (can be single data frame or list)
#' @param single_as_df whether to return a single value as a data frame
#' FIXME: add more document from isosys about how things are named
#' (might be possible to include in overall documentation from here
#' just by saying @note ?)
#' @note the setup for this function also means that you can modify
#' e.g. an existing ratio with the paramters passed in (say to set the name later on)
#' @export
iso <- function(class.isoval, class.isosys, ..., attribs = list(), single_as_df = FALSE) {
    values <- list(...)
    
    # function to make a new isotope value object
    new_isoval <- function(data, isoname) {
        if (!is (data, class.isoval))
            data <- new(class.isoval, data) # initialize new if not already the right object
        update(data, isoname = isoname, attribs = attribs) # update attributes
    }
    
    # type checks
    if (!extends(class.isoval, "Isoval"))
        stop("not an Isoval class:", class.isoval)
    
    if (!extends(class.isosys, "Isosys"))
        stop("not an Isosys class:", class.isosys)
        
    # no values passed at all --> initialize with empty numeric
    if (length(values) == 0)
        values <- list(numeric()) 
    
    if (length(values) == 1L && (is(values[[1]], 'data.frame') || is(values[[1]], 'list'))) {
        # argument is a single data frame or list
        values <- values[[1]]
    } else if (length(values) == 1L && !single_as_df) {
        # single isotope value object as vector
        return(new_isoval(values[[1]], isoname = names(values)[1]))
    }
    
    # system of values / data frame 
    if (!all((val <- sapply(values, length)) == length(values[[1]])))
        stop("Not the same number of measurements provided for each isotope: ", paste(val, collapse = ", "))
    
    # initialize each value 
    print(values)
    print(names(values))
    values <- mapply(new_isoval, values, names(values), SIMPLIFY = FALSE)
    print(values)
    
    # generate data frame and with the correct column names
    df <- data.frame(values)
    names(df) <- make.unique(
        sapply(values, function(i) {
            if (nchar(i@isoname) == 0) 'iso' else i@isoname
        }, simplify = TRUE))
    
    # isotope system object
    new(class.isosys, df)
}

