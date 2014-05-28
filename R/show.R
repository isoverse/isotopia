#' @include attribs.R
NULL

setMethod("show", "Isoval", function(object) {
    validObject(object)
    text <- if(is.weighted(object) && !is.ff(object) && !is.epsilon(object)) 
        "A weighted isotope" else "An isotope"
    cat(text, " value object of type '", class(object), " value': ", get_label(object), "\n", sep="")
    # only print weighting if it's an intensity, ratio, abundance or delta value (not for fractionation factors!)
    if (is.weighted(object) && !is.ff(object) && !is.epsilon(object)) {
        print(data.frame(
                value = object@.Data, 
                weight = object@weight))
    } else {
        print(get_value(object))
    }
})

setMethod("show", "Isosys", function(object) {
    validObject(object)
    cat("An isotope system object of type '", class(object), "' with ", get_label(object), "\n", sep="")
    print(as.data.frame(object))
})

# Helper methods ====================== 

# put together ratio and fraction factor names
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

#' Information about an isotopic data object
#' 
#' Get information about the name, label and units of
#' an isotopic data object.
#'
#' @details
#' \code{get_name()} returns the name of an isotopic data object
#' @export
#' @family data type attributes
#' @rdname object_info
#' @method get_name
setGeneric("get_name", function(object) standardGeneric("get_name"))

#' @export
#' @method get_name
setMethod("get_name", "ANY", function(object) stop("the get_name() function is not defined for objects of type ", class(object)))

setMethod("get_name", "Isoval", function(object) object@isoname)
setMethod("get_name", "Ratio", function(object) ratio_name("R", "", spacer = " ", object@isoname, object@major))
setMethod("get_name", "Abundance", function(object) ratio_name("F", object@isoname))

# FIXME: show proper naming of fractionation factors (consider alpha, eps, permil, ppm, etc.)

setMethod("get_name", "FractionationFactor", function(object) {
    notation <- switch(class(object@notation), 
                       "Notation_alpha" = get_iso_letter("alpha"),
                       get_iso_letter("eps")) # everything else (raw, permil, ppm) is essentially an epsilon value
    ratio_name(object@isoname, notation, "_", object@compound, object@compound2)
})
setMethod("get_name", "Epsilon", function(object) {
    ratio_name(object@isoname, get_iso_letter("epsilon"), "_", object@compound, object@compound2)
})
setMethod("get_name", "Delta", function(object) paste(get_iso_letter("delta"), object@isoname, sep = ""))

#' @details
#' \code{get_units()} provides the units of an isotope data object depending
#' on the object type and notation
#' 
#' @export
#' @rdname object_info
#' @method get_units
setGeneric("get_units", function(object) standardGeneric("get_units"))
setMethod("get_units", "Isoval", function(object) object@notation@unit)
setMethod("get_units", "Intensity", function(object) object@unit)

#' @details
#' \code{get_label()} provides the full label of an isotope data object
#' 
#' @rdname object_info
#' @export
#' @method get_label
#' @examples
#' \dontrun{
#' get_label(ratio(...))
#' get_label(abundance(...))
#' get_label(isosys(ratio(...), ratio(...))
#' }
setGeneric("get_label", function(object) standardGeneric("get_label"))

#' @export
#' @method get_label
setMethod("get_label", "ANY", function(object) stop("get_label() not defined for objects of type ", class(object)))

# helper
iso_label <- function(object, show_compound = TRUE) {
    paste(c(
        if (show_compound && nchar(object@compound) > 0) object@compound,
        get_name(object),
        if (nchar(get_units(object)) > 0) paste0("[", get_units(object), "]")), 
        collapse=" ")
}

setMethod("get_label", "Isoval", function(object) iso_label(object))

setMethod("get_label", "FractionationFactor", function(object) iso_label(object, show_compound = FALSE))

setMethod("get_label", "Epsilon", function(object) iso_label(object, show_compound = FALSE))

setMethod("get_label", "Delta", function(object) {
    paste(c(iso_label(object), 
            if (nchar(object@compound2) > 0) "vs.",
            if (nchar(object@compound2) > 0) object@compound2), collapse = " ")
})

setMethod("get_label", "Isosys", function(object) {
    isos <- sapply(object, function(i) is(i, "Isoval"))
    if (any(isos))
        paste0(sapply(object[which(isos), drop = F], get_label), collapse = ", ")
    else
        ""
})

