#' @include attribs.R
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

# put together ratio names (ratio, alpha and epsilon) names
# [text1 text2][spacer[top/?]/[bottom/?]]
ratio_name <- function(text1, text2, spacer = "", top = "", bottom = "") {
    text <- paste(c(
        if (nchar(text1) > 0) text1,
        if (nchar(text2) > 0) text2), collapse = " ")
    
    tlen <- nchar(top)
    blen <- nchar(bottom)
    if ( tlen > 0 && blen > 0)
        paste0(text, spacer, top, "/", bottom)
    else if (tlen > 0)
        paste0(text, spacer, top, "/?")
    else if (blen > 0)
        paste0(text, spacer, "?/", bottom)
    else 
        text
}

#' Get the name of an isotopic data object
#' @export
#' @genericMethods
setGeneric("name", function(object) standardGeneric("name"))
setMethod("name", "Isoval", function(object) object@isoname)
setMethod("name", "Ratio", function(object) ratio_name("R", "", spacer = " ", object@isoname, object@major))
setMethod("name", "Abundance", function(object) ratio_name("F", object@isoname))
setMethod("name", "Alpha", function(object) {
    ratio_name(object@isoname, "α", "_", object@compound, object@compound2)
})
setMethod("name", "Epsilon", function(object) {
    ratio_name(object@isoname, "ε", "_", object@compound, object@compound2)
})
setMethod("name", "Delta", function(object) paste("δ", object@isoname, sep = ""))

#' Get the units of an isotope data object
#' @export
#' @genericMethods
setGeneric("unit", function(object) standardGeneric("unit"))
setMethod("unit", "Isoval", function(object) "")
setMethod("unit", "Epsilon", function(object) if(object@permil) "‰" else "")
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

# helper
iso_label <- function(object, show_compound = TRUE) {
    paste(c(
        if (show_compound && nchar(object@compound) > 0) object@compound,
        name(object),
        if (nchar(unit(object)) > 0) paste0("[", unit(object), "]")), 
        collapse=" ")
}

setMethod("label", "Isoval", function(object) iso_label(object))

setMethod("label", "Alpha", function(object) iso_label(object, show_compound = FALSE))

setMethod("label", "Epsilon", function(object) iso_label(object, show_compound = FALSE))

setMethod("label", "Delta", function(object) {
    paste(c(iso_label(object), 
            if (nchar(object@compound2) > 0) "vs.",
            if (nchar(object@compound2) > 0) object@compound2), collapse = " ")
})

setMethod("label", "Isosys", function(object) {
    isos <- sapply(object, function(i) is(i, "Isoval"))
    if (any(isos))
        paste0(sapply(object[which(isos), drop = F], label), collapse = ", ")
    else
        ""
})

