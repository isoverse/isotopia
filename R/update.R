#' @include classes.R
NULL

#' Update the attributes of an isotope value object
#' @genericMethods
setGeneric("update", function(obj, isoname, attribs) standardGeneric("update"))
setMethod("update", "Isoval", function(obj, isoname, attribs) {
    if (!is.null(isoname) && nchar(isoname) > 0){
        if (nchar(obj@isoname) > 0 && obj@isoname != isoname) 
            warning("changing the name of a '", class(obj), " value' object from '", obj@isoname, "' to '", isoname, "'")
        obj@isoname <- isoname
    }
    if (!is.null(major <- attribs$major) && nchar(major) > 0) {
        if (nchar(obj@major) > 0 && obj@major != major)
            warning("changing the major isotope of a '", class(obj), " value' object from '", obj@major, "' to '", major, "'")
        obj@major <- major
    }
    obj
})

setMethod("update", "Delta", function(obj, isoname, attribs) {
    obj <- callNextMethod(obj, isoname, attribs)
    # IMPELEMENT ME - take different ref options from attribs and match them to this objects isoname
})

setMethod("update", "Intensity", function(obj, isoname, attribs) {
    obj <- callNextMethod(obj, isoname, attribs)
    if (!is.null(unit <- attribs$unit) && nchar(unit) > 0) {
        if (nchar(obj@unit) > 0 && obj@unit != unit)
            warning("changing the unit of a '", class(obj), " value' object from '", obj@unit, "' to '", unit, "'")
        obj@unit <- unit
    }
    obj
})