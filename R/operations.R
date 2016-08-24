#' @include conversion.R
NULL


#' Calculate isotope mass balance
#' 
#' This function calculates the isotope mass balance from combining multiple weighted
#' isotope \code{\link{abundance}} or \code{\link{delta}} value objects. This calculation
#' is also implemented with an \code{\link{arithmetic}} shorthand.
#'
#' @param ... - any number of weighted isotope value objects (have to be all either abundance or delta)
#' @param exact - whether to calculate mass balance of delta values exactly (default FALSE), not
#' fully implemented yet
#' @return weighted abundance or delta value object that represents the combination of the parameters
#' @family operations
#' @method mass_balance
#' @export
setGeneric("mass_balance", function(iso, iso2, ..., exact = get_iso_opts("exact_mass_balance")) standardGeneric("mass_balance"))

#' @method mass_balance
#' @export
setMethod("mass_balance", "ANY", function(iso, iso2, ...,
                                          exact = get_iso_opts("exact_mass_balance")) 
    stop("mass_balance not defined for objects of class ", 
         class(iso), ", ", class(iso2), call. = FALSE))

setMethod("mass_balance", signature("Abundance", "Abundance"), function(iso, iso2, ..., exact = get_iso_opts("exact_mass_balance")) {
    # consider implementing a performance optimized version for many additions
    all <- c(list(iso2), list(...))
    for (i in all)
        iso = iso + i
    iso
})
setMethod("mass_balance", signature("Delta", "Delta"), function(iso, iso2, ..., exact = get_iso_opts("exact_mass_balance")) {
    if (exact)
        stop("not implemented yet!", call. = FALSE) # should be implemented here rather than in the operators
    
    all <- c(list(iso2), list(...))
    for (i in all)
        iso = iso + i
    iso
})

setMethod("mass_balance", signature("Deltas", "Deltas"), function(iso, iso2, ..., exact = get_iso_opts("exact_mass_balance")) {
    stop("not implemented yet", call. = FALSE)
})

#' Fractionate an isotopic value
#' 
#' This function calculates the outcome of isotopic fractionation by a \code{\link{fractionation_factor}}
#' and can be applied to \code{\link{ratio}} data, \code{\link{delta}} values or other
#' \code{\link{fractionation_factor}} objects.
#' @param frac the fractionation factor \code{\link{ff}} used to fractionate the isotope value
#' @param iso the isotope object to fractionate
#' @return an object of the same type as \code{iso}
#' @note Several of these calculations are also
#' implemented with an \code{\link{arithmetic}} shorthand. All calculatinos are
#' only permissible if the fractionation factors and isotope values have matching
#' attributes.
#' @family operations
#' @method fractionate
#' @export
setGeneric("fractionate", function(frac, iso) standardGeneric("fractionate"))

#' @method fractionate
#' @export
setMethod("fractionate", "ANY", function(frac, iso) stop("fractionate not defined for objects of class ", class(frac), ", ", class(iso), call. = FALSE)) 

setMethod("fractionate", signature("FractionationFactor", "Ratio"), function(frac, iso) {
    iso_attribs_check(frac, iso, include = c("isoname", "major"), text = "cannot generate a ratio from a fractionation factor and a ratio")
    if (frac@compound2 != iso@compound)
        stop(sprintf("cannot generate a ratio if the fractionation factor's denominator (%s) does not match the ratio compound (%s)", frac@compound2, iso@compound), call. = FALSE)
    iso@.Data <- get_value(frac, notation = "alpha") * iso@.Data # weight carried in second value
    recast_isoval(iso, "Ratio", list(compound = frac@compound))
})

# weight of first fractionation factor is carried
setMethod("fractionate", signature("FractionationFactor", "FractionationFactor"), function(frac, iso) {
    iso_attribs_check(frac, iso, include = c("isoname", "major"), text = "cannot generate a fractionation factor from two fractionation factors")
    if (frac@compound2 != iso@compound)
        stop(sprintf("cannot combine two fractionation factors if their denominator (%s) and numerator (%s) don't cancel", frac@compound2, iso@compound), call. = FALSE)
    notation <- frac@notation # original notation
    frac <- switch_notation(frac, "alpha") # convert to alphas for calculaton
    frac@.Data <- frac@.Data * get_value(iso, "alpha")
    switch_notation(recast_isoval(frac, "FractionationFactor", list(compound2 = iso@compound2)), notation)
})

# weight of first fractionation factor is carried
setMethod("fractionate", signature("FractionationFactor", "Delta"), function(frac, iso) {
    notation <- iso@notation
    a <- to_ff(iso) # convert delta to alpha
    new <- fractionate(frac, a) # fractionate
    switch_notation(to_delta(new, ref_ratio = iso@ref_ratio), notation)# convert back to delta with the proper ref_ratio and notation
})

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
#' @family operations
#' @method shift_reference
#' @export
setGeneric("shift_reference", function(iso, ref) standardGeneric("shift_reference"))

#' @method shift_reference
#' @export
setMethod("shift_reference", "ANY", function(iso, ref) stop("shift_reference not defined for objects of class ", class(iso), ", ", class(ref)))

setMethod("shift_reference", signature("Delta", "Delta"), function(iso, ref) {
    a <- to_ff(iso) # convert value to shift to an alpha value
    fractionate(a, ref) # "fractionte" new reference with this (will automatically make sure everything is correct)
})
