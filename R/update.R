#' @include classes.R
NULL

#' update_iso the attributes of an isotope value object
#' @genericMethods
setGeneric("update_iso", function(obj, attribs) standardGeneric("update_iso"))
setMethod("update_iso", "Isoval", function(obj, attribs) {
    if (!is.null(isoname <- attribs$isoname) && nchar(isoname) > 0){
        if (nchar(obj@isoname) > 0 && obj@isoname != isoname) 
            warning("changing the name of a '", class(obj), " value' object from '", obj@isoname, "' to '", isoname, "'")
        obj@isoname <- isoname
    }
    if (!is.null(major <- attribs$major) && nchar(major) > 0) {
        if (nchar(obj@major) > 0 && obj@major != major)
            warning("changing the major isotope of a '", class(obj), " value' object from '", obj@major, "' to '", major, "'")
        obj@major <- major
    }
    if (!is.null(compound <- attribs$compound) && nchar(compound) > 0) {
        if (nchar(obj@compound) > 0 && obj@compound != compound)
            warning("changing the compound name of a '", class(obj), " value' object from '", obj@compound, "' to '", compound, "'")
        obj@compound <- compound
    }
    if (!is.null(weight <- attribs$weight) && !is.na(weight)) {
        if (!is.na(obj@weight) && obj@weight != weight)
            warning("changing the weight of a '", class(obj), " value' object, differences: '", paste(setdiff(obj@weight, weight), collapse=", "), "'")
        obj@weight <- weight
    }
    obj
})

setMethod("update_iso", "Delta", function(obj, attribs) {
    obj <- callNextMethod(obj, attribs)
    # IMPELEMENT ME - take different ref options from attribs and match them to this objects isoname
})

setMethod("update_iso", "Intensity", function(obj, attribs) {
    obj <- callNextMethod(obj, attribs)
    if (!is.null(unit <- attribs$unit) && nchar(unit) > 0) {
        if (nchar(obj@unit) > 0 && obj@unit != unit)
            warning("changing the unit of a '", class(obj), " value' object from '", obj@unit, "' to '", unit, "'")
        obj@unit <- unit
    }
    obj
})

#' Weight an isotope value object
#' 
#' Adds a weight (can be thought of as mass or concentration) to an isotopic value
#' object or entire isotope system which will be used to weigh the isotope value
#' when adding together multiple isotope values. 
#' 
#' @param iso - object to add weight to
#' @param weight - vector of weight values, has to be a single value or the same length
#' as the data stored in the isotope value object.
#' @return same as input (\code{iso})
#' @export
#' @note This can also be achieved when first initializing (or updating) an object 
#' via calls to \code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.
#' @genericMethods
setGeneric("weight", function(iso, weight) standardGeneric("weight"))
setMethod("weight", signature("Isoval", "numeric"), function(iso, weight) {
    update_iso(iso, list(weight = weight))
})
setMethod("weight", signature("Isosys", "numeric"), function(iso, weight) {
    validObject(iso) 
    for (i in which(sapply(iso, is.isoval))) {
        iso[[i]] <- weight(iso[[i]], weight)
    }
    return(iso)
})
