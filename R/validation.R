#' @include classes.R
NULL

#FIXME convert to is_iso, is_ratio, etc.

#' Check for isotope value objects
#' 
#' Checks for different kinds of isotope value objects. All checks recognize
#' both the vector (single isotope value) and the data.frame (isotope system) 
#' version of an isotope value object. \code{is.isosys(obj)} can be used to
#' make the distinction between the two.
#' 
#' @note Isotope value objects that are subset or extended loose their identification
#' as isotope value objects and will no longer be recognized as such by these functions.
#' For example, a \code{r <- \link{ratio}(x)} object that is subset with \code{r <- r[i]}
#' or extended with \code{r <- c(r, y)} looses its identification as a Ratio. The same
#' is true for an isotope system \code{sys <- \link{ratio}(x, y)} that is subset with
#' \code{sys <- sys[i, j]} or \code{\link{subset}} or extended with \code{\link{rbind}}.
#' Hopefully, this will be remedied in future versions. For now, modification requires
#' recasting, i.e. rerunning, for example, the \code{\link{ratio}} function on the modified
#' isotope values.
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
is.delta <- function(obj) inherits(obj, "Delta") || inherits(obj, "Deltas")

#' @details
#' \code{is.intensity} checks whether the object is an ion intensity object.
#' Returns TRUE if it's a single ion intensity object or an isotope system of ion intensities,
#' FALSE otherwise.
#' @rdname is.iso
#' @export
is.intensity <- function(obj) inherits(obj, "Intensity") || inherits(obj, "Intensities")

# =====================================
# Built-in object validity checks 
# Triggered automatically when new instance is created
# and can be re-run with validObject(obj) at any time

setValidity(
    "Ratio",
    function(object) {             
        if (any(is.na(object))) return('NA is not a valid isotope ratio')
        if (any(object < 0)) return('isotope ratios cannot be negative')
        if (nchar(object@isoname) > 0 && nchar(object@major) > 0 && object@isoname == object@major)
            return("isotope ratios cannot be defined for the same isotope as minor and major isotope")
        return(TRUE)
    })

setValidity(
    "Abundance",
    function(object) {
        if (any(is.na(object))) return('NA is not a valid fractional abundance')
        if (any(object < 0)) return('fractional abundances cannot be negative')
        if (any(object > 1)) return('fractional abundances cannot be larger than 1')
        return(TRUE)
    })

setValidity(
    "Intensity",
    function(object) {             
        if (any(is.na(object))) return('NA is not a valid ion intensity')
        if (any(object < 0)) return('ion intensities cannot be negative')
        return(TRUE)
    })

setValidity(
    "Isosys",
    function(object) {
        if (!all(sapply(object, function(i) is(i, "Isoval"))))
            return("Not all data in the system are provided as isotope data types (delta, ratio, abundance, etc.)")
        
        if (!all((val <- sapply(object, class)) == class(object[[1]])))
            return(paste("Not all isotopes in the system have the same data type, found:", paste(val, collapse = ", ")))
        
        if (any(duplicated(val <- unlist(sapply(object, function(i) if(nchar(i@isoname) > 0) i@isoname)))))
            return(paste("All isotopes in a system must be unique, found duplicates:", paste(val, collapse = ", ")))
        
        return (TRUE)
    })

setValidity(
    "Intensities",
    function(object) {
#         if (nchar(object@major) == 0)
#             return("The major ion must be specified in an isotopic system defined by ion intensities")
#         
#         if (! (object@major %in% sapply(object, function(i) i@isoname)))
#             return("The major ion must be part of the ion intensities isotopic system")
#         
#         units <- unlist(sapply(object, function(i) if (nchar(i@unit) > 0) i@unit))
#         if (!is.null(units) && !all(units == units[1]))
#             return(paste("If units are specified in an isotopic system of ion intensities, they must all be the same.",
#                          "Found:", paste(units, collapse=", ")))
        
        return (TRUE)
    })