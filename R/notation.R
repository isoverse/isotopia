test <- function() {
    
    setClass("Unit", representation(unit = "character"))
    # alpha fractionation factor
    setClass("Unit_alpha", contains = "Unit", prototype = prototype(unit = ""))
    # epsilon factionation factor (not multipliation)
    setClass("Unit_eps", contains = "Unit", prototype = prototype(unit = ""))
    # plain delta or plan fractional abundance
    setClass("Unit_raw", contains = "Unit", prototype = prototype(unit = ""))
    # permil delta or permil fractionation factor
    setClass("Unit_permil", contains = "Unit", prototype = prototype(unit = get_iso_letter("permil")))
    # ppm delta or ppm fractionation factor
    setClass("Unit_ppm", contains = "Unit", prototype = prototype(unit = "ppm"))
    # percent fractional abundance
    setClass("Unit_percent", contains = "Unit", prototype = prototype(unit = "%"))
    
    #' Switch units 
    #' 
    #' Valid units:
    #' Abundance: raw, percent
    #' Delta: raw, permil, ppm
    #' Fractionation factor: alpha, eps, permil, ppm (note: alpha is considered the 'raw' unit of a fractionation factor)
    #' 
    #' @usage switch_units(iso, to)
    #' @param iso isotopic data object (\code{\link{ff}}, \code{\link{abundance}}, \code{\link{delta}})
    #' @return isotope object with converted units, an error if it is not a valid conversion
    #' @family data type attributes
    #' @method switch_units
    #' @export
    setGeneric("switch_units", function(iso, to = NULL, from = NULL) standardGeneric("switch_units"))
    
    #' @method switch_units
    #' @export
    setMethod("switch_units", "ANY", function(iso, to = NULL, from = NULL) stop(sprintf("don't know how to convert units '%s' to units '%s'", class(to), class(from))))
    
    # what happens mathematically in the conversions
    setMethod("switch_units", signature("numeric", to = "Unit", from = "Unit"), function(iso, to, from) iso)
    setMethod("switch_units", signature("numeric", to = "Unit_percent", from = "Unit_raw"), function(iso, to, from) 100*iso)
    setMethod("switch_units", signature("numeric", to = "Unit_raw", from = "Unit_percent"), function(iso, to, from) iso/100)
    setMethod("switch_units", signature("numeric", to = "Unit_eps", from = "Unit_alpha"), function(iso, to, from) iso - 1)
    setMethod("switch_units", signature("numeric", to = "Unit_alpha", from = "Unit_eps"), function(iso, to, from) iso + 1)
    setMethod("switch_units", signature("numeric", to = "Unit_permil", from = "Unit_eps"), function(iso, to, from) 1000*iso)
    setMethod("switch_units", signature("numeric", to = "Unit_eps", from = "Unit_permil"), function(iso, to, from) iso/1000)
    setMethod("switch_units", signature("numeric", to = "Unit_permil", from = "Unit_alpha"), function(iso, to, from) 1000*(iso - 1))
    setMethod("switch_units", signature("numeric", to = "Unit_alpha", from = "Unit_permil"), function(iso, to, from) iso/1000 + 1)
    setMethod("switch_units", signature("numeric", to = "Unit_ppm", from = "ANY"), function(iso, to, from) 1000*switch_units(iso, to = new("Unit_permil"), from))
    setMethod("switch_units", signature("numeric", to = "ANY", from = "Unit_ppm"), function(iso, to, from) switch_units(iso/1000, to, from = new("Unit_permil")))

    # implement conversion of isovals and what's permitted on the level of the Isoval objects
    isoval_switch_units <- function(iso, to) {
        unit <- new("Unit_raw") #FIXME: iso@unit
        iso@.Data <- switch_units(iso@.Data, to = to, from = unit)
        #iso@unit <- to # FIXME
        iso
    }
    setMethod("switch_units", signature("Isoval", "ANY", "missing"), function(iso, to, from) {
        if (!is(to, "Unit"))
            stop("not a recognized type of unit for isotope value objects: ", class(to))
        stop(sprintf("not permitted to convert an isotope value of type '%s' to unit '%s'", class(iso), sub("Unit_", "", class(to))))
    })
    setMethod("switch_units", signature("Isoval", to = "Unit_raw", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    setMethod("switch_units", signature("Abundance", to = "Unit_percent", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    setMethod("switch_units", signature("Delta", to = "Unit_permil", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    setMethod("switch_units", signature("Delta", to = "Unit_ppm", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    #setMethod("switch_units", signature("FractionationFactor", to = "Unit_raw", from = "missing"), function(iso, to = NULL, from = NULL) switch_units(iso, "alpha")) # raw and alpha is the same for frac factors
    #setMethod("switch_units", signature("FractionationFactor", to = "Unit_alpha", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    #setMethod("switch_units", signature("FractionationFactor", to = "Unit_eps", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    #setMethod("switch_units", signature("FractionationFactor", to = "Unit_permil", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    #setMethod("switch_units", signature("FractionationFactor", to = "Unit_ppm", from = "missing"), function(iso, to = NULL, from = NULL) isoval_switch_units(iso, to))
    
    # this is the one the user actually calls
    setMethod("switch_units", signature("Isoval", to = "character", from = "missing"), function(iso, to = NULL, from = NULL) {
        to_class <- paste0("Unit_", to)
        if (!extends(to_class, "Unit"))
            stop("not a recognized type of unit for isotope value objects: ", to)
        switch_units(iso, to = new(to_class))
    })
    
    expect_error(switch_units(5), "don't know how to convert")
    expect_error(switch_units(ratio(0.5)), "not a recognized type of unit")
    expect_error(switch_units(ratio(0.5), 5), "not a recognized type of unit")
    expect_error(switch_units(ratio(0.5), "bla"), "not a recognized type of unit")
    expect_error(switch_units(ratio(0.5), "percent"), "not permitted to convert .* 'Ratio' to unit 'percent'")
    expect_equal(switch_units(ratio(0.5), "raw"), ratio(0.5))
    expect_equal(get_value(ab <- switch_units(abundance(0.1), "percent")), 10)
    expect_equal(get_value(switch_units(ab, "raw")), 0.1) # continue here, after unit is implemented properly, testand implement the reverse transformation 
}