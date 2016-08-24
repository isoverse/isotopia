#' @include operations.R
NULL

#' Isotope arithmetic
#' 
#' @description
#' Several arithmetic operators (+, -, *, /) are implemented to work with specific
#' isotope value object to allow shorthand data type conversions and calculations.
#' Operations are generally only permitted if the two isotope objects being combined
#' have matching attributes (isotope name, major isotope, etc.).
#' @name arithmetic 
#' @rdname arithmetic
NULL


# helper function that informs about operation error
operation_error <- function(operation, e1, e2) {
    stop(sprintf("%s is not meaningful for these isotope objects (trying to combine '%s' and '%s'). ", operation, class(e1), class(e2)), call. = FALSE)
}

# helper function that compares isotope objects' attributes
iso_attribs_check <- function(e1, e2, include = names(attributes(e1)), exclude = c(), text = "", check_length = TRUE) {
    e1.attribs <- attributes(e1)
    e1.attribs <- e1.attribs[which(names(e1.attribs) %in% setdiff(include, exclude))]
    e2.attribs <- attributes(e2)
    e2.attribs <- e2.attribs[which(names(e2.attribs) %in% setdiff(include, exclude))]
    failed <- FALSE
    if (any(!names(e1.attribs) %in% names(e2.attribs)) && any(!names(e2.attribs) %in% names(e1.attribs))) 
        failed <- TRUE
    else
        e1.attribs <- e1.attribs[names(e2.attribs)] # sort properly
    
    if (failed || !identical(e1.attribs, e2.attribs)) {
        attr_names <- paste0("'", names(e1.attribs)[names(e1.attribs)!="class"], "'")
        stop(sprintf("%s that don't have matching attributes (%s):\n'%s'\n'%s'", 
                     text, paste(attr_names, collapse = " or "), get_label(e1), get_label(e2)),
             call. = FALSE)
    }
    if (check_length && length(e1) != length(e2))
        stop(sprintf("%s that don't have matching lengths: %s and %s", text, 
                     length(e1), length(e2)), call. = FALSE)
}

# Addition  ========================

setMethod("+", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Addition", e1, e2))
# FIXME: adding two Isosys should be permitted if they have matching columns and then are added colum by column! (and then each column added to the other can decide if it's permitted)

#' @usage intensity +- intensity
#' @details
#' \code{intensity+-intensity} allows the addition of intensity values, the result is a another \code{\link{intensity}} object
#' @name arithmetic 
#' @rdname arithmetic
NULL

# adding intensities
setMethod("+", signature(e1 = "Intensity", e2 = "Intensity"), function(e1, e2) {
    iso_attribs_check(e1, e2, exclude = "weight", text = "trying to combine two intensity objects")
    e1@.Data <- get_value(e1) + get_value(e2)
    validObject(e1)
    e1
})

#' @usage abundance +- abundance
#' @details
#' \code{abundance+-abundance} is a shorthand for calculating the isotopic mass balance of
#' two \code{\link{abundance}} objects, see \code{\link{mass_balance}} for details
#' @name arithmetic 
#' @rdname arithmetic
NULL

# adding abundances (i.e. isotope mixing/mass balance calculations)
setMethod("+", signature(e1 = "Abundance", e2 = "Abundance"), function(e1, e2) {
    e1 <- switch_notation(e1, "raw") # convert to raw for mass balance with the same units
    e2 <- switch_notation(e2, "raw") # convert to raw for mass balance with the same units
    
    iso_attribs_check(e1, e2, exclude = c("weight", "compound"), check_length = FALSE, 
                      text = "trying to calculate the mass balance of two abundance objects")
    # FIXME: currently allowing to add vectors of different lengths to support adding a fixed value to a vector
    # but ideally checking here that only length(e1) == length(e2) or length(e1) == 1 or length(e2) ==1 is allowed
    
    weightsum <- get_weight(e1) + get_weight(e2)
    e1@.Data <- (get_weighted_value(e1) + get_weighted_value(e2))/weightsum
    e1@weight <- weightsum
    e1@compound <- paste(sub("^$", "?", c(e1@compound, e2@compound)), collapse = "+")
    # don't propagate ?+?
    if (e1@compound == "?+?") e1@compound <- ""
    validObject(e1)
    
    # FIXME: using default notation but should use the same notation the objects have if they are both the same and different from default
    switch_notation(e1, get_iso_opts("default_ab_notation"))
})

#' @usage delta +- delta
#' @details
#' \code{delta+-delta} is a shorthand for calculating the isotopic mass balance of
#' two \code{\link{delta}} objects, see \code{\link{mass_balance}} for details
#' @name arithmetic 
#' @rdname arithmetic
NULL

# adding deltas (i.e. isotope mixing/mass balance calculations)
setMethod("+", signature(e1 = "Delta", e2 = "Delta"), function(e1, e2) {
    e1 <- switch_notation(e1, "raw") # convert to raw for mass balance with the same units
    e2 <- switch_notation(e2, "raw") # convert to raw for mass balance with the same units
    iso_attribs_check(e1, e2, exclude = c("weight", "compound"), text = "trying to calculate the mass balance of two delta values")    
    weightsum <- get_weight(e1) + get_weight(e2)
    e1@.Data <- (get_weighted_value(e1) + get_weighted_value(e2))/weightsum
    e1@weight <- weightsum
    e1@compound <- paste(sub("^$", "?", c(e1@compound, e2@compound)), collapse = "+")
    # don't propagate ?+?
    if (e1@compound == "?+?") e1@compound <- ""
    validObject(e1)
    
    # FIXME: using default notation but should use the same notation the objects have if they are both the same and different from default
    switch_notation(e1, get_iso_opts("default_delta_notation"))
})

# Subtraction  ========================

setMethod("-", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Subtraction", e1, e2))

# allow inversion of the weighting
setMethod("-", signature(e1 = "Isoval", e2 = "missing"), function(e1, e2) {
    e1@weight <- (-1) *e1@weight
    e1
})

# linking back to the equivalent addition call
setMethod("-", signature(e1 = "Intensity", e2 = "Intensity"), function(e1, e2) {
    e2@.Data <- (-1) * e2@.Data
    e1 + e2
})

# linking back to the equivalent addition call
setMethod("-", signature(e1 = "Abundance", e2 = "Abundance"), function(e1, e2) {
    e2@weight <- (-1) * e2@weight
    e1 + e2
})

# linking back to the equivalent addition call
setMethod("-", signature(e1 = "Delta", e2 = "Delta"), function(e1, e2) {
    e2@weight <- (-1) * e2@weight
    e1 + e2
})

#' @usage alpha - 1
#' @details
#' \code{alpha - 1} is a shorthand for converting a fractionation factor from 
#' alpha to epsilon notation. The ff object has to be in alpha notation,
#' otherwise this is just interpreted as a regular arithmetic operation 
#' and the result will no longer be an isotope object. 
#' \code{eps  + 1} is the reverse operation.
#' @name arithmetic 
#' @rdname arithmetic
NULL

# alpha - 1 = eps
setMethod("-", signature(e1 = "FractionationFactor", e2 = "numeric"), function(e1, e2) {
    if (e2 == 1L && is(e1@notation, "Notation_alpha"))
        return(switch_notation(e1, "eps"))
    callNextMethod(e1, NULL)
})


# eps + 1 = alpha
setMethod("+", signature(e1 = "FractionationFactor", e2 = "numeric"), function(e1, e2) {
    if (e2 == 1L && is(e1@notation, "Notation_eps"))
        return(switch_notation(e1, "alpha"))
    callNextMethod(e1, NULL)
})

# Multiplication ========================

setMethod("*", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Multiplication", e1, e2))

#' @usage delta * 1000 
#' @usage delta / 1000
#' @details
#' \code{delta * 1000} is a shorthand for converting a raw delta
#' value to permil notation or permil to ppm. The same works for fractionation factors
#' in epsilon notation. \code{delta / 1000} is the reverse
#' @name arithmetic 
#' @rdname arithmetic
NULL

setMethod("*", signature(e1 = "FractionationFactor", e2 = "numeric"), function(e1, e2) {
    if (e2 == 1000 && is(e1@notation, "Notation_eps"))
        return(switch_notation(e1, "permil")) # raw to permil
    else if (e2 == 0.001 && is(e1@notation, "Notation_permil"))
        return(switch_notation(e1, "eps")) # permil to raw
    else if (e2 == 1000 && is(e1@notation, "Notation_permil"))
        return(switch_notation(e1, "ppm")) # permil to ppm
    else if (e2 == 0.001 && is(e1@notation, "Notation_ppm"))
        return(switch_notation(e1, "permil")) # ppm to permil
    callNextMethod(e1, e2)
})
setMethod("*", signature(e1 = "numeric", e2 = "FractionationFactor"), function(e1, e2) e2*e1)
setMethod("/", signature(e1 = "FractionationFactor", e2 = "numeric"), function(e1, e2) e1 * (1/e2))

# 1000*delta - this is actually tested
setMethod("*", signature(e1 = "Delta", e2 = "numeric"), function(e1, e2) {
    if (e2 == 1000 && is(e1@notation, "Notation_raw"))
        return(switch_notation(e1, "permil")) # raw to permil
    else if (e2 == 0.001 && is(e1@notation, "Notation_permil"))
        return(switch_notation(e1, "raw")) # permil to raw
    else if (e2 == 1000 && is(e1@notation, "Notation_permil"))
        return(switch_notation(e1, "ppm")) # permil to ppm
    else if (e2 == 0.001 && is(e1@notation, "Notation_ppm"))
        return(switch_notation(e1, "permil")) # ppm to permil
    callNextMethod(e1, e2)
})
setMethod("*", signature(e1 = "numeric", e2 = "Delta"), function(e1, e2) e2*e1)
setMethod("/", signature(e1 = "Delta", e2 = "numeric"), function(e1, e2) e1 * (1/e2))



#' @usage ff * ratio
#' @details
#' \code{ff*ratio}, \code{ff*ff}, \code{ff*delta} are a shorthand for 
#' fractionating an isotope object with a factionation factor,
#' see \code{\link{fractionate}} for details
#' @name arithmetic 
#' @rdname arithmetic
NULL

# ff * ratio and ratio * ff (always the weight of the ratio is carried over, ff is considered a modifier)
setMethod("*", signature(e1 = "FractionationFactor", e2 = "Ratio"), function(e1, e2) fractionate(e1, e2))
setMethod("*", signature(e1 = "Ratio", e2 = "FractionationFactor"), function(e1, e2) e2 * e1) # just reverse

# ff * ff (weight of first ff is carried)
setMethod("*", signature(e1 = "FractionationFactor", e2 = "FractionationFactor"), function(e1, e2) fractionate(e1, e2))

# ff * delta(x) and delta(x) * ff (always the weight of the delta value is carried over, ff considered a modifier)
setMethod("*", signature(e1 = "FractionationFactor", e2 = "Delta"), function(e1, e2) fractionate(e1, e2))
setMethod("*", signature(e1 = "Delta", e2 = "FractionationFactor"), function(e1, e2) e2 * e1) # just reverse

#' @usage delta * delta
#' @details
#' \code{delta*delta}, is a shorthand for shifting the reference frame of the
#' first delta value to that of the second (requires the compound measured in the
#' second to be the reference of the first!), see \code{\link{shift_reference}} 
#' for details
#' @name arithmetic 
#' @rdname arithmetic
NULL

# delta * delta
setMethod("*", signature(e1 = "Delta", e2 = "Delta"), function(e1, e2) shift_reference(e1, e2))

# FIXME: isotope systems?
setMethod("*", signature(e1 = "FractionationFactors", e2 = "Ratios"), function(e1, e2) stop("theoretically permissible but might not be worth implementing"))
# + other Isosys expansion of this stuff



# Division ========================

setMethod("/", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Division", e1, e2))
setMethod("/", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Division", e1, e2))
setMethod("/", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Division", e1, e2))
setMethod("/", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Division", e1, e2))

#' @usage intensity / intensity
#' @details
#' \code{intensity/intensity} allows the creation of an isotope \code{\link{ratio}} object
#' @name arithmetic 
#' @rdname arithmetic
NULL

# intensity / intensity = ratio
setMethod("/", signature(e1 = "Intensity", e2 = "Intensity"), function(e1, e2) {
    iso_attribs_check(e1, e2, include = "unit", text = "cannot generate an isotope ratio from two intensity objects")
    e1@.Data <- e1@.Data / e2@.Data 
    recast_isoval(e1, "Ratio", list(unit = NULL, major = e2@isoname))
})

#' @usage ratio / ratio
#' @details
#' \code{ratio/ratio} allows the creation of an isotope \code{\link{fractionation_factor}} 
#' This is a shorthand for the \link{to_ff} function.
#' @name arithmetic 
#' @rdname arithmetic
NULL

# ratio / ratio = alpha (weight of first ratio carried)
setMethod("/", signature(e1 = "Ratio", e2 = "Ratio"), function(e1, e2) {
    iso_attribs_check(e1, e2, include = c("isoname", "major"), text = "cannot generate a fractionaton factor from two ratio objects")
    e1@.Data <- get_value(e1@.Data, "raw") / get_value(e2@.Data, "raw")
    e1@notation <- new("Notation_alpha") # keep as an alpha value
    recast_isoval(e1, "FractionationFactor", list(compound2 = e2@compound))
})

#' @usage ff / ff
#' @details
#' \code{ff/ff} allows the creation of another isotope \code{\link{fractionation_factor}} object but requires that
#' either the denominator names or numerator names of the two objects are identical (i.e. they "cancel").
#' This is a shorthand for the \link{to_ff} function.
#' @name arithmetic 
#' @rdname arithmetic
NULL

# alpha / alpha (weight of first alpha is carried)
setMethod("/", signature(e1 = "FractionationFactor", e2 = "FractionationFactor"), function(e1, e2) {
    iso_attribs_check(e1, e2, include = c("isoname", "major"), text = "cannot generate a fractionation factor from two fractionation factors")
    if (e1@compound2 == e2@compound2) 
        e1@compound2 <- e2@compound # denominators cancel
    else if (e1@compound == e2@compound) 
        e1@compound <- e2@compound2 # numerators cancel
    else
        stop(sprintf("cannot combine two fractionation factors if neither their denominators (%s and %s) nor their numerators (%s and %s) cancel", 
                     e1@compound2, e2@compound2, e1@compound, e2@compound), 
             call. = FALSE)
    
    e1 <- switch_notation(e1, "alpha") # make sure they are alpha values
    e2 <- switch_notation(e2, "alpha") # make sure they are alpha values
    e1@.Data <- e1@.Data / e2@.Data
    
    # ideally should be the same notation of the originals if they are differnt from the default
    switch_notation(recast_isoval(e1, "FractionationFactor"), get_iso_opts("default_ff_notation"))
})

#' @usage delta / delta
#' @details
#' \code{delta/delta} creates an \code{\link{fractionation_factor}} object that
#' describes the fractionation factor between the two compounds, requires the reference
#' name of the two delta values to be identical. This is a shorthand for the \link{to_ff} function.
#' @name arithmetic 
#' @rdname arithmetic
NULL

# delta/delta = alpha (weight of first delta is carried)
setMethod("/", signature(e1 = "Delta", e2 = "Delta"), function(e1, e2) {
    to_ff(e1, e2)
})


