#' @include classes.R
NULL

# update attributes =================================

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
    if (!is.null(weight <- attribs$weight) && length(weight) > 0) {
        if (length(weight) == 1L) weight <- rep(weight, length(obj@.Data))
        if (is.weighted(obj) && 
                length(diffs <- union(setdiff(obj@weight, weight), setdiff(weight, obj@weight))) > 0)
            warning("changing the weight of a '", class(obj), " value' object, differences: '", paste(diffs, collapse=", "), "'")
        obj@weight <- weight
    }
    obj
})

setMethod("update_iso", "Alpha", function(obj, attribs) {
    obj <- callNextMethod(obj, attribs)
    if (!is.null(compound2 <- attribs$compound2) && nchar(compound2) > 0) {
        if (nchar(obj@compound2) > 0 && obj@compound2 != compound2)
            warning("changing the bottom compound of a '", class(obj), " value' object from '", obj@compound2, "' to '", compound2, "'")
        obj@compound2 <- compound2
    }
    obj
})

setMethod("update_iso", "Delta", function(obj, attribs) {
    obj <- callNextMethod(obj, attribs)
    if (!is.null(ref <- attribs$ref) && nchar(ref) > 0) {
        if (nchar(obj@ref) > 0 && obj@ref != ref)
            warning("changing the reference name of a '", class(obj), " value' object from '", obj@ref, "' to '", ref, "'")
        obj@ref <- ref
    }
    if (!is.null(permil <- attribs$permil) && length(permil) > 0) {
        if (length(obj@permil) > 0 && obj@permil != permil)
            stop("changing the type of an already initialized delta value from permil to non-permil must be done using the appropriate as.delta() and as.deltax functions")
        obj@permil <- permil
    }
    obj
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
#' \code{weight(iso, weight)} adds a weight (can be thought of as mass or concentration) to an isotopic value
#' which will be used to weigh the isotope value when adding together multiple isotope values. 
#' \code{weight(iso)} returns the weight of an isotope value object or \code{NULL} if it is undefined.
#' 
#' @param iso - object to get weight or add weight
#' @param weight - vector of weight values, has to be a single value or the same length
#' as the data stored in the isotope value object.
#' @usage
#' \code{weight(iso)}
#' \code{weight(iso, weight)}
#' @export
#' @note This can also be achieved when first initializing (or updating) an object 
#' via calls to \code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.
#' @genericMethods
#' @examples
#' r <- ratio(0.2)
#' r <- weight(r, 10)
#' print(weight(r)) # returns 10
setGeneric("weight", function(iso, weight) standardGeneric("weight"))
setMethod("weight", signature("Isoval", "numeric"), function(iso, weight) {
    iso <- update_iso(iso, list(weight = weight))
    validObject(iso) 
    iso
})

# retrieve individual attributes =================================

#' Retrieve isotope object's primitive values
#' 
#' This function returns an isotope object's (single value or isotope system)  
#' primitive data value(s).  
#' 
#' @return In the case of a single isotope object (Isoval), returns the numeric
#' vector of raw values stored in the object. In the case of an isotope system (Isosys),
#' returns the data frame underlying the object with all its isotope value
#' objects also replaced with their numeric raw values. To just get the data
#' frame but keep the isotope values intact, use \code{\link{as.data.frame}} instead.
#' @seealso \code{\link{as.data.frame}}, \code{\link[base]{as.data.frame}} (base method)
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.value", function(iso) standardGeneric("as.value"))
setMethod("as.value", "Isoval", function(iso) iso@.Data)
setMethod("as.value", "Isosys", function(iso) {
    data.frame(lapply(iso,
      function(col) {
          if (is.isoval(col)) as.value(col)
          else col
      }), stringsAsFactors = F)
})

#' Retrieve isotope object's weight values
#' 
#' This function returns an isotope object's weight values.
#' 
#' @return In the case of a single isotope object (Isoval), returns the numeric
#' vector of weights stored in the object. In the case of an isotope system (Isosys),
#' returns the data frame underlying the object with all its isotope value
#' objects replaced with their weight values. 
#' @seealso \code{\link{as.data.frame}}, \code{\link[base]{as.data.frame}} (base method)
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.weight", function(iso) standardGeneric("as.weight"))
setMethod("as.weight", "Isoval", function(iso) iso@weight)
setMethod("as.weight", "Isosys", function(iso) {
    data.frame(lapply(iso,
      function(col) {
          if (is.isoval(col)) as.weight(col)
          else col
      }), stringsAsFactors = F)
})

#' Retrieve isotope object's weighted values
#' 
#' This function returns an isotope object's weighted values.
#' 
#' @return In the case of a single isotope object (Isoval), returns a numeric
#' vector of the object's values weighted by the object's weights. In the case of an isotope system (Isosys),
#' returns the data frame underlying the object with all its isotope value
#' objects replaced with their weighted values. 
#' @seealso \code{\link{as.data.frame}}, \code{\link[base]{as.data.frame}} (base method)
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.weighted_value", function(iso) standardGeneric("as.weighted_value"))
setMethod("as.weighted_value", "Isoval", function(iso) as.weight(iso) * as.value(iso))
setMethod("as.weighted_value", "Isosys", function(iso) {
    data.frame(lapply(iso,
      function(col) {
          if (is.isoval(col)) as.weighted_value(col)
          else col
      }), stringsAsFactors = F)
})

#' Convert isotope system to a data frame.
#' 
#' This function returns the underlying data frame of an isotope system. The
#' individual columns that hold isotope values keep their status as isotope
#' value objects. 
#' @seealso \code{\link[base]{as.data.frame}}
#' @name as.data.frame
#' @export
as.data.frame.Isosys <- function(x, ..., stringsAsFactors = default.stringsAsFactors()){
    df <- data.frame(x@.Data, stringsAsFactors = stringsAsFactors)
    names(df) <- names(x) 
    df
}

