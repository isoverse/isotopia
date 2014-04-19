#' @include attribs.R
#' @include conversion.R
NULL

#' Isotope ratio
#'
#' Generate isotope ratio objects. See \link{isotopia} for general information on initializing
#' and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into isotope ratio objects
#' @param major - name of the major isotope in the single ratio or isotope system [optional]
#' @param compound - name of the compound the isotopic values belong to [optional]
#' @param weight - weight the isotope value (with a mass, concentration, etc.) for easy mass balance calculations.
#' The default value is 1, i.e. an unweighted isotope value.
#' If specified, \code{weight} must be a single value or a numeric vector of the same size as the data values. 
#' The weight of an isotope value obejct can be retrieved and (re)set with the \code{\link{weight}} function.
#' @family isotope data types
#' @export
#' @examples
#' ratio(0.1) # single value
#' ratio(c(0.1, 0.2, 0.3)) # multiple values
#' ratio(`13C` = c(0.1, 0.2, 0.3)) # named ratio
#' ratio(`33S` = c(0.1, 0.2, 0.3), `34S` = c(0.2, 0.4, 0.6), major = "32S") # isotope system
ratio <- function(..., major = "", compound = "", weight = numeric(), single_as_df = FALSE) {
    iso("Ratios", ..., attribs = list(major = major, compound = compound, weight = weight), single_as_df = single_as_df)
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
#' @param compound - name of the compound the isotopic values belong to [optional]
#' @family isotope data types
#' @export
abundance <- function(..., major = "", compound = "", weight = numeric(), single_as_df = FALSE) {
    iso("Abundances", ..., attribs = list(major = major, compound = compound, weight = weight), single_as_df = single_as_df)
}

#' Alpha value
#'
#' Generate an isotope alpha value (ratio of two isotope ratios) object. See \link{isotopia} for general information on initializing
#' and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into alpha values
#' @param major - name of the major isotope in the isotope system [optional]
#' @param ctop - name of the compound representing the top isotope ratio [optional]
#' @param cbot - name of the compound representing the bottom isotope ratio [optional]
#' @family isotope data types
#' @export
alpha <- function(..., major = "", ctop = "", cbot = "", single_as_df = FALSE) {
    iso("Alphas", ..., attribs = list(major = major, compound = ctop, compound2 = cbot), single_as_df = single_as_df)
}

#' Delta value
#'
#' Generate an isotope delta value object. See \link{isotopia} for general information on initializing
#' and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into delta values
#' @param major - name of the major isotope in the isotope system [optional]
#' @param compound - name of the compound the isotopic values belong to [optional]
#' @param ref - name of the reference material
#' @param ref_ratio - value of the reference material (can be numeric or any valid isotope value object)
#' @param permil - whether the values passed in are in permil or raw values (i.e. no 1000x multiplication)
#' @param weight - weight the isotope value (with a mass, concentration, etc.) for easy mass balance calculations.
#' The default value is 1, i.e. an unweighted isotope value.
#' If specified, \code{weight} must be a single value or a numeric vector of the same size as the data values. 
#' The weight of an isotope value obejct can be retrieved and (re)set with the \code{\link{weight}} function.
#' @family isotope data types
#' @examples
#' delta(50, permil = T) # enter as permil value
#' delta(0.05, permil = F) # enter as non-permil value
#' @export
delta <- function(..., major = "", compound = "", ref = "", ref_ratio = numeric(), permil = TRUE, weight = numeric(), single_as_df = FALSE) {
    if (is.isoval(ref_ratio))
        ref_ratio <- as.value(as.ratio(ref_ratio))
    iso("Deltas", ..., attribs = list(major = major, compound = compound, ref = ref, ref_ratio = ref_ratio, permil = permil, weight = numeric()), single_as_df = single_as_df)
}

#' Ion intensity
#' 
#' Generate an ion intensity object (e.g. ion counts or signal intensity). 
#' See \link{isotopia} for general information on initializing and converting isotope data objects.
#' 
#' @param ... - numeric vectors (can be named) to turn into ion intensity objects
#' @param major - name of the major isotope in the isotope system [optional], 
#' @param compound - name of the compound the isotopic values belong to [optional]
#' @param unit - units of the measurement (e.g. #, V, mV)
#' @family isotope data types
#' @export
intensity <- function(..., major = "", compound = "", unit = "", single_as_df = FALSE) {
    iso("Intensities", ..., 
        attribs = list(major = major, compound = compound, unit = unit), single_as_df = single_as_df)
}

#' create an isotope value object (this function is not exported and should
#' be access via the appropriate wrapper functions, e.g. \code{ratio}, \code{abundance}, etc.)
#' @param class_isosys name of the class for an isotope system (which holds the info on which isoval class belongs to the system as well)
#' @param attribs named list of attributes to pass to the isotope data object constructors
#' @param ... values (can be single data frame or list)
#' @param single_as_df whether to return a single value as a data frame
#' @note the setup for this function also means that you can modify
#' e.g. an existing ratio with the paramters passed in (say to set the name later on)
#' @export
iso <- function(class_isosys, ..., attribs = list(), single_as_df = FALSE) {
    values <- list(...)
    
    # type checks
    if (!extends(class_isosys, "Isosys"))
        stop("not an Isosys class: ", class_isosys)
    class_isoval <- new(class_isosys)@isoval_class
    
    # function to make a new isotope value object
    new_isoval <- function(data, isoname) {
        if (!is (data, class_isoval))
            data <- new(class_isoval, data) # initialize new if not already the right object
        obj <- update_iso(data, attribs = c(list(isoname = isoname), attribs)) # update attributes
        validObject(obj) # test validity
        return(obj)
    }
    
    # no values passed at all --> initialize with empty numeric
    if (length(values) == 0)
        values <- list(numeric()) 
    
    # list with single value --> go single
    if (is(values[[1]], 'list') && length(values[[1]]) == 1L)
        values <- values[[1]]
    
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
    if (is.null(isonames <- names(values))) isonames <- ""
    values <- mapply(new_isoval, values, isonames, SIMPLIFY = FALSE)
    
    # isotope system object
    new(class_isosys, data.frame(values, stringsAsFactors = F))
}

