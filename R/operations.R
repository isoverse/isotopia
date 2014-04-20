#' @include conversion.R
NULL

#' Calculate isotope mass balance
#' 
#' This function calculates the isotope mass balance from combining multiple weighted
#' isotope \code{\link{abundance}} or \code{\link{delta}} value objects. This calculation
#' is also implemented with an \code{\link{arithmetic}} shorthand.
#'
#' @param ... - any number of weighted isotope value objects (have to be all either abundance or delta)
#' @param exact - whether to calculate mass balance of delta values exactly (default FALSE), 
#' use exact_mass_balance to set this paramter globally 
#' @return weighted abundance or delta value object that represents the combination of the parameters
#' @export
#' @genericMethods
setGeneric("mass_balance", function(iso, iso2, ..., exact = exact_mass_balance()) standardGeneric("mass_balance"))
setMethod("mass_balance", signature("Abundance", "Abundance"), function(iso, iso2, ..., exact = exact_mass_balance()) {
    # consider implementing a performance optimized version for many additions
    all <- c(list(iso2), list(...))
    for (i in all)
        iso = iso + i
    iso
})
setMethod("mass_balance", signature("Delta", "Delta"), function(iso, iso2, ..., exact = exact_mass_balance()) {
    if (exact)
        stop("not implemented yet!") # should be implemented here rather than in the operators
    
    all <- c(list(iso2), list(...))
    for (i in all)
        iso = iso + i
    iso
})

setMethod("mass_balance", signature("Deltas", "Deltas"), function(iso, iso2, ..., exact = exact_mass_balance()) {
    stop("not implemented yet")
})

#' Fractionate an isotopic value
#' 
#' This function calculates the outcome of isotopic fractionation on an isotopic value and
#' is defined for a number of different combinations (how an \code{\link{alpha}} fractionation
#' factor fractionates a single \code{\link{ratio}} or a \code{\link{delta}} value, how it fractionates another
#' \code{\link{alpha}} value; how a \code{\link{epsilon}} value does the same, etc.)
#' @param frac the isotope object used to fractionate the isotope value
#' @param iso the isotope object to fractionate
#' @return an object of the same type as \code{iso}
#' @note Several of these calculations are also
#' implemented with an \code{\link{arithmetic}} shorthand. All calculatinos are
#' only permissible if the fractionation factors and isotope values have matching
#' attributes.
#' @export
setGeneric("fractionate", function(frac, iso) standardGeneric("fractionate"))
setMethod("fractionate", signature("Alpha", "Ratio"), function(frac, iso) {
    iso_attribs_check(frac, iso, include = c("isoname", "major"), text = "cannot generate a ratio from a fractionation factor and a ratio")
    if (frac@compound2 != iso@compound)
        stop(sprintf("cannot generate a ratio if the fractionation factor's denominator (%s) does not match the ratio compound (%s)", frac@compound2, iso@compound))
    iso@.Data <- frac@.Data * iso@.Data # weight carried in second value
    recast_isoval(iso, "Ratio", list(compound = frac@compound))
})

# weight of first alpha is carried
setMethod("fractionate", signature("Alpha", "Alpha"), function(frac, iso) {
    iso_attribs_check(frac, iso, include = c("isoname", "major"), text = "cannot generate a fractionation factor from two fractionation factors")
    if (frac@compound2 != iso@compound)
        stop(sprintf("cannot combine two fractionation factors if their denominator (%s) and numerator (%s) don't cancel", frac@compound2, iso@compound))
    frac@.Data <- frac@.Data * iso@.Data
    recast_isoval(frac, "Alpha", list(compound2 = iso@compound2))
})

# weight of first alpha is carried
setMethod("fractionate", signature("Alpha", "Delta"), function(frac, iso) {
    a <- as.alpha(iso) # convert delta to alpha
    new <- fractionate(frac, a) # fractionate
    as.delta(new, ref_ratio = iso@ref_ratio, permil = iso@permil) # convert back to delta with the proper ref_ratio and permil parameters
})

# other options (use epsilon to fractionate something)
setMethod("fractionate", signature("Epsilon", "ANY"), function(frac, iso) fractionate(as.alpha(frac), iso))

#' Shift reference frame
#' 
#' This function shifts the reference frame of an isotopic data object
#' that has a reference associated (currently only \code{\link{delta}} values.
#' 
#' @param iso the isotope object whose reference frame to shift (\link{delta})
#' @param ref the isotope object which is relative to the new reference frame (\link{delta})
#' @return a \link{delta} value with the shifted reference
#' 
#' @note The function requires the reference of the first delta value to
#' the compound measured in the second delta value. This calculations is
#' also implemented with an \code{\link{arithmetic}} shorthand. All calculatinos are
#' only permissible if the fractionation factors and isotope values have matching
#' attributes.
#' @export
setGeneric("shift_reference", function(iso, ref) standardGeneric("shift_reference"))
setMethod("shift_reference", signature("Delta", "Delta"), function(iso, ref) {
    a <- as.alpha(iso) # convert value to shift to an alpha value
    fractionate(a, ref) # "fractionte" new reference with this (will automatically make sure everything is correct)
})


