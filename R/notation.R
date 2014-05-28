setClass("Notation", representation(unit = "character"))
# alpha fractionation factor
setClass("Notation_alpha", contains = "Notation", prototype = prototype(unit = ""))
# alpha fractionation factor in log normal notation (NOT IMPLEMENTED YET!)
#setClass("Notation_ln", contains = "Notation", prototype = prototype(unit = ""))
# epsilon factionation factor (raw value, no multiplication)
setClass("Notation_eps", contains = "Notation", prototype = prototype(unit = ""))
# plain delta or plan fractional abundance
setClass("Notation_raw", contains = "Notation", prototype = prototype(unit = ""))
# permil delta or permil fractionation factor
setClass("Notation_permil", contains = "Notation", prototype = prototype(unit = get_iso_letter("permil")))
# ppm delta or ppm fractionation factor
setClass("Notation_ppm", contains = "Notation", prototype = prototype(unit = "ppm"))
# percent fractional abundance
setClass("Notation_percent", contains = "Notation", prototype = prototype(unit = "%"))

#' Switch notation 
#' 
#' Valid notation:
#' Abundance: raw, percent
#' Delta: raw, permil, ppm
#' Fractionation factor: alpha, eps, permil, ppm (note: alpha is considered the 'raw' unit of a fractionation factor)
#' 
#' @usage switch_notation(iso, to)
#' @param iso isotopic data object (\code{\link{ff}}, \code{\link{abundance}}, \code{\link{delta}})
#' @return isotope object with converted notation, an error if it is not a valid conversion
#' @family data type attributes
#' @method switch_notation
#' @export
setGeneric("switch_notation", function(iso, to = NULL, from = NULL) standardGeneric("switch_notation"))

#' @method switch_notation
#' @export
setMethod("switch_notation", "ANY", function(iso, to = NULL, from = NULL) stop(sprintf("don't know how to convert notation '%s' to notation '%s'", class(to), class(from))))

# what happens mathematically in the conversions
setMethod("switch_notation", signature("numeric", to = "Notation", from = "Notation"), function(iso, to, from) iso)
setMethod("switch_notation", signature("numeric", to = "Notation_percent", from = "Notation_raw"), function(iso, to, from) 100*iso)
setMethod("switch_notation", signature("numeric", to = "Notation_raw", from = "Notation_percent"), function(iso, to, from) iso/100)
setMethod("switch_notation", signature("numeric", to = "Notation_eps", from = "Notation_alpha"), function(iso, to, from) iso - 1)
setMethod("switch_notation", signature("numeric", to = "Notation_alpha", from = "Notation_eps"), function(iso, to, from) iso + 1)
setMethod("switch_notation", signature("numeric", to = "Notation_permil", from = "Notation_eps"), function(iso, to, from) 1000*iso)
setMethod("switch_notation", signature("numeric", to = "Notation_eps", from = "Notation_permil"), function(iso, to, from) iso/1000)
setMethod("switch_notation", signature("numeric", to = "Notation_permil", from = "Notation_alpha"), function(iso, to, from) 1000*(iso - 1))
setMethod("switch_notation", signature("numeric", to = "Notation_alpha", from = "Notation_permil"), function(iso, to, from) iso/1000 + 1)
setMethod("switch_notation", signature("numeric", to = "Notation_ppm", from = "ANY"), function(iso, to, from) 1000*switch_notation(iso, to = new("Notation_permil"), from))
setMethod("switch_notation", signature("numeric", to = "ANY", from = "Notation_ppm"), function(iso, to, from) switch_notation(iso/1000, to, from = new("Notation_permil")))

# implement conversion of isovals and what's permitted on the level of the Isoval objects
isoval_switch_notation <- function(iso, to) {
    unit <- new("Notation_raw") #FIXME: iso@unit
    iso@.Data <- switch_notation(iso@.Data, to = to, from = unit)
    #iso@unit <- to # FIXME
    iso
}
setMethod("switch_notation", signature("Isoval", "ANY", "missing"), function(iso, to, from) {
    if (!is(to, "Notation"))
        stop("not a recognized notation for isotope value objects: ", class(to))
    stop(sprintf("not permitted to convert an isotope value of type '%s' to unit '%s'", class(iso), sub("Notation_", "", class(to))))
})
setMethod("switch_notation", signature("Isoval", to = "Notation_raw", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("Abundance", to = "Notation_percent", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("Delta", to = "Notation_permil", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("Delta", to = "Notation_ppm", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("FractionationFactor", to = "Notation_raw", from = "missing"), function(iso, to = NULL, from = NULL) switch_notation(iso, "alpha")) # raw and alpha is the same for frac factors
setMethod("switch_notation", signature("FractionationFactor", to = "Notation_alpha", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("FractionationFactor", to = "Notation_eps", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("FractionationFactor", to = "Notation_permil", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))
setMethod("switch_notation", signature("FractionationFactor", to = "Notation_ppm", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_notation(iso, to))

# this is the one the user actually calls
setMethod("switch_notation", signature("Isoval", to = "character", from = "missing"), function(iso, to = NULL, from = NULL) {
    to_class <- paste0("Notation_", to)
    if (!extends(to_class, "Notation"))
        stop("not a recognized notation for isotope value objects: ", to)
    switch_notation(iso, to = new(to_class))
})
    
 