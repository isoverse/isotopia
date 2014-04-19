#' @include classes.R
NULL

setMethod("show", "Isoval", function(object) {
    validObject(object)
    text <- if(is.weighted(object)) "A weighted isotope" else "An isotope"
    cat(text, " value object of type '", class(object), " value': ", label(object), "\n", sep="")
    if (is.weighted(object)) {
        print(data.frame(
                value = object@.Data, 
                weight = object@weight))
    } else {
        print(as.value(object))
    }
})

setMethod("show", "Isosys", function(object) {
    validObject(object)
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
setMethod("name", "Alpha", function(object) {
    text <- paste(c(
        if (nchar(object@isoname) > 0) object@isoname,
        "α"), collapse = " ")
    
    tlen <- nchar(object@compound)
    blen <- nchar(object@compound2)
    if ( tlen > 0 && blen > 0)
        paste0(text, "_", object@compound, "/", object@compound2)
    else if (tlen > 0)
        paste0(text, "_", object@compound, "/?")
    else if (blen > 0)
        paste0(text, "_", "?/", object@compound2)
    else 
        text
})
setMethod("name", "Delta", function(object) {
    paste(c("δ", if (nchar(object@isoname) > 0) object@isoname), collapse = "")
})

#' Get the units of an isotope data object
#' @export
#' @genericMethods
setGeneric("unit", function(object) standardGeneric("unit"))
setMethod("unit", "Isoval", function(object) "")
setMethod("unit", "Delta", function(object) {
    if(object@permil) "‰" else ""
})
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
    label <- c(
        if (nchar(object@compound) > 0) object@compound,
        name(object),
        if (nchar(unit(object)) > 0) paste0("[", unit(object), "]"))
    paste(label, collapse=" ")
})

setMethod("label", "Delta", function(object) {
    paste(c(callNextMethod(object), 
            if (nchar(object@ref) > 0) "vs.",
            if (nchar(object@ref) > 0) object@ref), collapse = " ")
})

setMethod("label", "Alpha", function(object) name(object))

setMethod("label", "Isosys", function(object) {
    isos <- sapply(object, function(i) is(i, "Isoval"))
    if (any(isos))
        paste0(sapply(object[which(isos), drop = F], label), collapse = ", ")
    else
        ""
})

