#' @include classes.R
NULL

setMethod("show", "Isoval", function(object) {
    cat("An isotope value object of type '", class(object), " value': ", label(object), "\n", sep="")
    print(object@.Data)
})

setMethod("show", "Isosys", function(object) {
    cat("An isotope system object of type '", class(object), "' with ", label(object), "\n", sep="")
    print(as.data.frame(object))
})

# Helper methods ====================== 

#' Get the name of an isotopic data object
#' @export
#' @genericMethods
setGeneric("name", function(object) standardGeneric("name"))
setMethod("name", "Isoval", function(object) object@isoname)
setMethod("name", "Ratio", function(object) {
    ilen <- nchar(object@isoname)
    mlen <- nchar(object@major)
    if ( ilen > 0 && mlen > 0)
        paste0("R ", object@isoname, "/", object@major)
    else if (ilen > 0)
        paste0("R ", object@isoname, "/?")
    else if (mlen > 0)
        paste0("R ?/", object@major)
    else "R"
})
setMethod("name", "Abundance", function(object) paste0("F ", object@isoname))

#' Get the units of an isotope data object
#' @export
#' @genericMethods
setGeneric("unit", function(object) standardGeneric("unit"))
setMethod("unit", "Isoval", function(object) "")
setMethod("unit", "Intensity", function(object) object@unit)

#' Get the full label of an isotope data object
#' @export
#' @genericMethods
#' @examples
#' \dontrun{
#' label(ratio(...))
#' label(abundance(...))
#' label(isosys(ratio(...), ratio(...))
#' }
setGeneric("label", function(object) standardGeneric("label"))

setMethod("label", "Isoval", function(object) {
    if (nchar(unit(object)) > 0)
        paste0(name(object), " [", unit(object), "]")
    else
        name(object)
})

setMethod("label", "Isosys", function(object) {
    isos <- which(sapply(object, function(i) is(i, "Isoval")))
    paste0(sapply(object[isos], label), collapse = ", ")
})

