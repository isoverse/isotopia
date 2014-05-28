#' @include classes.R
NULL

#' Checks for isotope value objects
#' 
#' Checks for different kinds of isotope value objects. All checks recognize
#' both the vector (single isotope value) and the data.frame (isotope system) 
#' version of an isotope value object. \code{is.isosys(obj)} can be used to
#' make the distinction between the two.
#' 
#' @details
#' \code{is.iso} checks whether the object is an isotope value object of any kind. 
#' Returns TRUE if it is (e.g. ratio, abundance, delta, etc. - single or system of
#' values), FALSE otherwise.
#' 
#' @param obj - object to test
#' @export
is.iso <- function(obj) inherits(obj, "Isoval") || inherits(obj, "Isosys")

#' @details
#' \code{is.isoval} checks whether the object is a single isotope value.
#' Returns TRUE if it's a single isotope value object (of any kind, ratio, abundance, delta, etc.)
#' and FALSE otherwise.
#' @rdname is.iso
#' @export
is.isoval <- function(obj) inherits(obj, "Isoval")

#' @details
#' \code{is.isosys} checks whether the object is a an isotope system.
#' Returns TRUE if it's an isotope system (of any kind, ratios, abundances, deltas, etc.)
#' and FALSE otherwise.
#' @rdname is.iso
#' @export
is.isosys <- function(obj) inherits(obj, "Isosys")

#' @details
#' \code{is.ratio} checks whether the object is an isotope ratio object.
#' Returns TRUE if it's a single isotope ratio object or an isotope system of ratios,
#' FALSE otherwise.
#' @rdname is.iso
#' @export
is.ratio <- function(obj) inherits(obj, "Ratio") || inherits(obj, "Ratios")

#' @details
#' \code{is.abundance} checks whether the object is an isotope abundance object.
#' Returns TRUE if it's a single isotope abundance object or an isotope system of abundances,
#' FALSE otherwise.
#' @rdname is.iso
#' @export
is.abundance <- function(obj) inherits(obj, "Abundance") || inherits(obj, "Abundances")

#' @details
#' \code{is.delta} checks whether the object is a delta value object.
#' Returns TRUE if it's a single delta value object or an isotope system of delta values,
#' FALSE otherwise.
#' @rdname is.iso
#' @export
is.delta <- function(obj) class(obj) %in% c("Delta", "Deltas") 

#' @details
#' \code{is.intensity} checks whether the object is an ion intensity object.
#' Returns TRUE if it's a single ion intensity object or an isotope system of ion intensities,
#' FALSE otherwise.
#' @rdname is.iso
#' @export
is.intensity <- function(obj) inherits(obj, "Intensity") || inherits(obj, "Intensities")

#' @details
#' \code{is.ff} checks whether the object is an fractionation factor value object.
#' Returns TRUE if it's a single fractionation factor value object or an isotope system of fractionation factor values,
#' FALSE otherwise.
#' @rdname is.iso
#' @export
is.ff <- function(obj) inherits(obj, "FractionationFactor") || inherits(obj, "FractionationFactors")

#' @details
#' \code{is.weighted} checks if an isotope object is weighted. An object
#' counts as weighted if any of the weights associated with the data values
#' is != 1, that means only objects whose weights are ALL 1 is considered
#' unweighted.
#' 
#' @usage
#' \code{is.weighted(iso)}
#' @examples
#' is.weighted(ratio(0.2)) # returns FALSE
#' is.weighted(ratio(0.2, weight = 1)) # returns FALSE
#' is.weighted(ratio(c(0.1, 0.2), weight = c(1,2))) # returns TRUE
#' @rdname is.iso
#' @export
is.weighted <- function(iso) !is.null(attr(iso, "weight")) && any(iso@weight != 1)

# =====================================
# Built-in object validity checks 
# Triggered automatically when new instance is created
# and can be re-run with validObject(obj) at any time

setValidity(
    "Isoval",
    function(object) {             
        if (any(is.na(object))) 
            return(paste('NA is not a valid isotope data type, found: ', paste(get_value(object), collapse = ", ")))
        
        if (length(object@weight) > 0 && length(object@weight) != length(object@.Data))
             return(sprintf("Not the same number of data values and weights. Found %s data values and %s weights. ", length(object@.Data), length(object@weight)))
         
        return(TRUE)
    })

setValidity(
    "Ratio",
    function(object) {             
        if (any(object < 0)) return('isotope ratios cannot be negative')
        if (nchar(object@isoname) > 0 && nchar(object@major) > 0 && object@isoname == object@major)
            return("isotope ratios cannot be defined for the same isotope as minor and major isotope")
        return(TRUE)
    })

setValidity(
    "Abundance",
    function(object) {
        # FIXME: this is fine with a switch_notation but doesn't work with abundance(20, notation="percent")!
        if (any(object < 0)) return('fractional abundances cannot be negative')
        if (is(object@notation, "Notation_raw") & any(object > 1)) 
            return('fractional abundances cannot be larger than 1') 
        if (is(object@notation, "Notation_percent") & any(object > 100)) 
            return('fractional abundances cannot be larger than 100%') 
        return(TRUE)
    })

# FIXME, valid values for fractionation factors depend on the notation!

setValidity(
    "FractionationFactor",
    function(object) {
        # FIXME: depends on notation, make sure that it works both with initialization and with switch_notation!
        #if (any(object < 0)) return('alpha values cannot be negative')
        return(TRUE)
    })

setValidity(
    "Intensity",
    function(object) {             
        if (any(object < 0)) return('ion intensities cannot be negative')
        return(TRUE)
    })

setValidity(
    "Isosys",
    function(object) {
        # IMPORTANT note: in the validation functions, it is critical to select the data with object@.Data rather
        # than via [] because otherwise there will be an endless loop (node stack overflow) when the [] function
        # tries to select a subset of an Isosys data frame and validate it
        if (!any(val <- sapply(object@.Data, is.isoval)))
            return("There are no isotope values in this isotope system.")
        isovals <- object@.Data[which(val)]
        
        if (!all((val <- sapply(isovals, class)) == object@isoval_class))
            return(paste0("Not all isotopes in the system have the expected data type (", object@isoval_class, "), found: ", paste(val, collapse = ", ")))
        
        if (any(duplicated(val <- unlist(sapply(isovals, function(i) if(nchar(i@isoname) > 0) i@isoname)))))
            return(paste("All isotopes in a system must be unique, found duplicates:", paste(val, collapse = ", ")))
        
        majors <- unlist(sapply(isovals, function(i) if (nchar(i@major) > 0) i@major))
        if (!is.null(majors) && !all(majors == majors[1]))
            return(paste("If specified, the major ion of all isotope value object in an isotope system must be the same.",
                         "Found:", paste(majors, collapse=", ")))
        
        compounds <- unlist(sapply(isovals, function(i) if (nchar(i@compound) > 0) i@compound))
        if (!is.null(compounds) && !all(compounds == compounds[1]))
            return(paste("If specified, the compound name of all isotope value objects in an isotope system must be the same.",
                         "Found:", paste(compounds, collapse=", ")))
        
        weights <- lapply(isovals, get_weight)
        if (length(weights) > 0 && any(sapply(weights, function(i) any(i != weights[[1]]))))
            return(paste("If specified, the weights of all isotope value objects in an isotope system must be the same."))
        
        return (TRUE)
    })

setValidity(
    "Abundances",
    function(object) {
        isovals <- object@.Data[which(sapply(object@.Data, is.isoval))]
        if (any( (sums <- rowSums(data.frame(isovals))) > 1.0)) 
            return(paste("the sum of fractional abundances for each data point in an isotope system cannot exceed 1",
                         ", found:", paste(sums[sums > 1.0], collapse = ", ")))
        return (TRUE)
    })


#FIXME: should probably check that notation is the same here!!!

setValidity(
    "Intensities",
    function(object) {
        isovals <- object@.Data[which(sapply(object@.Data, is.isoval))]
        isonames <- unlist(sapply(isovals, function(i) if(nchar(i@isoname) > 0) i@isoname))
        majors <- unlist(sapply(isovals, function(i) if (nchar(i@major) > 0) i@major))
        if (!is.null(majors) && ! majors[1] %in% isonames )
            return(paste0("The major ion (", majors[1], ") must be part of the ion intensities isotopic system but is missing"))
         
        units <- unlist(sapply(isovals, function(i) if (nchar(i@unit) > 0) i@unit))
        if (!is.null(units) && !all(units == units[1]))
            return(paste("If specified, the units in an isotopic system of ion intensities must all be the same.",
                         "Found:", paste(units, collapse=", ")))
        
        return (TRUE)
    })