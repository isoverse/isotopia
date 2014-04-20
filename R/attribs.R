#' @include classes.R
NULL

# update attributes =================================

# helper function for updating text attributes
update_text_attrib <- function(obj, attribs, slot_name, msg) {
    if (!is.null(value <- attribs[[slot_name]]) && nchar(value) > 0){
        if (nchar(slot(obj, slot_name)) > 0 && slot(obj, slot_name) != value) 
            warning(msg, " ('", class(obj), " value' object) from '", slot(obj, slot_name), "' to '", value, "'")
        slot(obj, slot_name) <- value
    }
    obj
}

#' update_iso the attributes of an isotope value object
#' @genericMethods
setGeneric("update_iso", function(obj, attribs) standardGeneric("update_iso"))
setMethod("update_iso", "Isoval", function(obj, attribs) {
    obj <- update_text_attrib(obj, attribs, "isoname", "changing the isotope name")
    obj <- update_text_attrib(obj, attribs, "major", "changing the major isotope")
    obj <- update_text_attrib(obj, attribs, "compound", "changing the compound name")
    # weight
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
    obj <- update_text_attrib (obj, attribs, "compound2", "changing the bottom compound name of a fractionation factor")    
    obj
})

setMethod("update_iso", "Epsilon", function(obj, attribs) {
    obj <- callNextMethod(obj, attribs)
    obj <- update_text_attrib (obj, attribs, "compound2", "changing the bottom compound name of a fractionation factor")    
    if (!is.null(permil <- attribs$permil) && length(permil) > 0) {
        if (length(obj@permil) > 0 && obj@permil != permil)
            stop("changing the type of an already initialized ", tolower(class(obj)), 
                 " value from permil to non-permil must be done using the appropriate as.", 
                 tolower(class(obj)), "(..., permil = TRUE/FALSE) function")
        obj@permil <- permil
    }
    obj
})

setMethod("update_iso", "Delta", function(obj, attribs) {
    # ref ratio
    if (!is.null(ref_ratio <- attribs$ref_ratio) && length(ref_ratio) > 0) {
        if (is.isoval(ref_ratio)) {
            if (is.null(attribs$compound2) || nchar(attribs$compound2) == 0) attribs$compound2 <- ref_ratio@compound # take compound value
            ref_ratio <- as.value(as.ratio(ref_ratio)) # convert to numeric
        }
        if (length(ref_ratio) != 1)
            stop("reference ratio for a delta value object must be exactly one numeric value, supplied", length(ref_ratio))
        if (length(obj@ref_ratio) > 0 && obj@ref_ratio != ref_ratio)
            warning(sprintf("changing the reference ratio of a delta value object from %s to %s", obj@ref_ratio, ref_ratio))
        obj@ref_ratio <- ref_ratio
    }
    obj <- update_text_attrib (obj, attribs, "compound2", "changing the reference name")    
    obj <- callNextMethod(obj, attribs)
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
#' \code{\link{as.weight}(iso)} returns the weight of an isotope value object.
#' 
#' @param iso - object to get weight or add weight
#' @param weight - vector of weight values, has to be a single value or the same length
#' as the data stored in the isotope value object.
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

