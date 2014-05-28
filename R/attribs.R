#' @include classes.R
NULL

# update attributes =================================

#' Set attributes of isotope objects
#' 
#' Set an attribute of an existing isotope value object.
#' 
#' @param iso the isotope value object to update
#' @param minor the name of the minor isotope
#' @param major the name of the major isotope
#' @param compound name of the compound the isotopic values belong to [optional]
#' @param ref name of the reference material (\code{\link{delta}} values only)
#' @param ref_ratio - value of the reference material (\code{\link{delta}} values only)
#' @param ctop name of the compound representing the top isotope ratio in 
#' a \code{\link{fractionation_factor}}
#' @param cbot name of the compound representing the bottom isotope ratio in
#' a \code{\link{fractionation_factor}}
#' @param unit unit for \code{\link{intensity}} value objects
#' @family data type attributes
#' @export
set_attrib <- function(iso, minor = NULL, major = NULL, 
                       compound = NULL, compound2 = NULL, 
                       ref = NULL, ref_ratio = NULL,
                       ctop = NULL, cbot = NULL,
                       unit = NULL) {
    if (!is.isoval(iso))
        stop("cannot set attributes of non-isotope value objects: ", class(iso))
    
    # new attribs
    attribs <- list(minor = minor, major = major, compound = compound, 
                    compound2 = compound2, ref_ratio = ref_ratio, unit = unit)
    attribs[sapply(attribs, is.null)] <- NULL
    
    # special cases (convert names to what their update equivalents are)
    if (!missing(minor)) attribs$isoname <- minor
    if (!missing(ctop)) attribs$compound <- ctop
    if (!missing(cbot)) attribs$compound2 <- cbot
    if (!missing(ref)) attribs$compound2 <- ref
    
    update_iso(iso, attribs)
}

#' update_iso the attributes of an isotope value object
#' internal function that is called by set_attrib wrapper
setGeneric("update_iso", function(obj, attribs) standardGeneric("update_iso"))

setMethod("update_iso", "Isoval", function(obj, attribs) {
    obj <- update_text_attrib(obj, attribs, "isoname", "changing the isotope name")
    obj <- update_text_attrib(obj, attribs, "major", "changing the major isotope")
    obj <- update_text_attrib(obj, attribs, "compound", "changing the compound name")
    
    # new notation
    if (!is.null(notation <- attribs$notation) && notation != sub("Notation_(.*)", "\\1", class(obj@notation) ) ) {
        # tests whether notation switch is permissible (but not actually doing the conversion since this is just 
        # updating the notation class)
        convertible <- switch_notation(obj, notation) # throws an error if there is trouble
        obj@notation <- convertible@notation
    }
    
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

setMethod("update_iso", "FractionationFactor", function(obj, attribs) {
    obj <- callNextMethod(obj, attribs)
    obj <- update_text_attrib (obj, attribs, "compound2", "changing the bottom compound name of a fractionation factor")    
    obj
})

setMethod("update_iso", "Delta", function(obj, attribs) {
    # ref ratio
    if (!is.null(ref_ratio <- attribs$ref_ratio) && length(ref_ratio) > 0) {
        if (is.isoval(ref_ratio)) {
            if (is.null(attribs$compound2) || nchar(attribs$compound2) == 0) attribs$compound2 <- ref_ratio@compound # take compound value
            ref_ratio <- get_value(to_ratio(ref_ratio)) # convert to numeric
        }
        if (length(ref_ratio) != 1)
            stop("reference ratio for a delta value object must be exactly one numeric value, supplied ", length(ref_ratio))
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

# helper function for updating text attributes
update_text_attrib <- function(obj, attribs, slot_name, msg) {
    if (!is.null(value <- attribs[[slot_name]]) && nchar(value) > 0){
        if (nchar(slot(obj, slot_name)) > 0 && slot(obj, slot_name) != value) 
            warning(msg, " ('", class(obj), " value' object) from '", slot(obj, slot_name), "' to '", value, "'")
        slot(obj, slot_name) <- value
    }
    obj
}


#' Weight an isotope value object
#' 
#' \code{weight(iso, weight)} adds a weight (can be thought of as mass or concentration) to an isotopic value
#' which will be used to weigh the isotope value when adding together multiple isotope values. 
#' \code{\link{get_weight}(iso)} returns the weight of an isotope value object.
#' 
#' @param iso - object to get weight or add weight
#' @param weight - vector of weight values, has to be a single value or the same length
#' as the data stored in the isotope value object.
#' @note This can also be achieved when first initializing (or updating) an object 
#' via calls to \code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.
#' @examples
#' r <- ratio(0.2)
#' r <- weight(r, 10)
#' print(get_weight(r)) # returns 10
#' @family data type attributes
#' @method weight
#' @export
setGeneric("weight", function(iso, weight) standardGeneric("weight"))

#' @method weight
#' @export
setMethod("weight", "ANY", function(iso, weight) stop("weight not defined for objects of class ", class(iso)))

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
#' @param notation specifiy which notation to return the value in (default is the notation
#' that hte object is in)
#' @return In the case of a single isotope object (Isoval), returns the numeric
#' vector of raw values stored in the object (same as \code{\link{as.numeric}}). 
#' In the case of an isotope system (Isosys),
#' returns the data frame underlying the object with all its isotope value
#' objects also replaced with their numeric raw values. To just get the data
#' frame but keep the isotope values intact, use \code{\link{as.data.frame}} instead.
#' @seealso \code{\link{as.numeric}}, \code{\link{as.data.frame}}, \code{\link[base]{as.data.frame}} (base method)
#' @family data type attributes
#' @method get_value
#' @export
setGeneric("get_value", function(iso, notation = "raw") standardGeneric("get_value"))

#' @method get_value
#' @export
setMethod("get_value", "ANY", function(iso, notation = "raw") stop("get_value not defined for objects of class ", class(iso)))
setMethod("get_value", "numeric", function(iso, notation = "raw") iso) # allow this for simplicity so this is similiar to as_numeric
setMethod("get_value", "Isoval", function(iso, notation = iso@notation) as.numeric(switch_notation(iso, notation)))
setMethod("get_value", "Isosys", function(iso, notation = NULL) {
    data.frame(lapply(iso,
      function(col) {
          if (is.isoval(col) & !is.null(notation)) get_value(col, notation)
          else if (is.isoval(col) & is.null(notation)) get_value(col)
          else col
      }), stringsAsFactors = F)
})

#' Retrieve isotope object's weights
#' 
#' This function returns an isotope object's weight values.
#' 
#' @return In the case of a single isotope object (Isoval), returns the numeric
#' vector of weights stored in the object. In the case of an isotope system (Isosys),
#' returns the data frame underlying the object with all its isotope value
#' objects replaced with their weight values. 
#' @seealso \code{\link{as.data.frame}}, \code{\link[base]{as.data.frame}} (base method)
#' @family data type attributes
#' @method get_weight
#' @export
setGeneric("get_weight", function(iso) standardGeneric("get_weight"))

#' @method get_weight
#' @export
setMethod("get_weight", "ANY", function(iso) stop("get_weight not defined for objects of class ", class(iso)))
setMethod("get_weight", "Isoval", function(iso) iso@weight)
setMethod("get_weight", "Isosys", function(iso) {
    data.frame(lapply(iso,
      function(col) {
          if (is.isoval(col)) get_weight(col)
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
#' @family data type attributes
#' @method get_weighted_value
#' @export
setGeneric("get_weighted_value", function(iso) standardGeneric("get_weighted_value"))

#' @method get_weighted_value
#' @export
setMethod("get_weighted_value", "ANY", function(iso) stop("get_weighted_value not defined for objects of class ", class(iso)))
setMethod("get_weighted_value", "Isoval", function(iso) get_weight(iso) * get_value(iso))
setMethod("get_weighted_value", "Isosys", function(iso) {
    data.frame(lapply(iso,
      function(col) {
          if (is.isoval(col)) get_weighted_value(col)
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

