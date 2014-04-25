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
    stop(sprintf("%s is not meaningful for these isotope objects (trying to combine '%s' and '%s'). ", operation, class(e1), class(e2)))
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
                     text, paste(attr_names, collapse = " or "), label(e1), label(e2)))
    }
    if (check_length && length(e1) != length(e2))
        stop(sprintf("%s that don't have matching lengths: %s and %s", text, length(e1), length(e2)))
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
    e1@.Data <- as.value(e1) + as.value(e2)
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
    iso_attribs_check(e1, e2, exclude = c("weight", "compound"), text = "trying to calculate the mass balance of two abundance objects")
    weightsum <- as.weight(e1) + as.weight(e2)
    e1@.Data <- (as.weighted_value(e1) + as.weighted_value(e2))/weightsum
    e1@weight <- weightsum
    e1@compound <- paste(sub("^$", "?", c(e1@compound, e2@compound)), collapse = "+")
    validObject(e1)
    e1
})

#' @usage delta +- delta
#' @details
#' \code{delta+-delta} is a shorthand for calculating the isotopic mass balance of
#' two \code{\link{delta}} objects, see \code{\link{mass_balance}} for details,
#' will use the global \code{\link{exact_mass_balance}} setting for the calculation
#' @name arithmetic 
#' @rdname arithmetic
NULL

# adding deltas (i.e. isotope mixing/mass balance calculations)
setMethod("+", signature(e1 = "Delta", e2 = "Delta"), function(e1, e2) {
    iso_attribs_check(e1, e2, exclude = c("weight", "compound"), text = "trying to calculate the mass balance of two delta values")
    weightsum <- as.weight(e1) + as.weight(e2)
    e1@.Data <- (as.weighted_value(e1) + as.weighted_value(e2))/weightsum
    e1@weight <- weightsum
    e1@compound <- paste(sub("^$", "?", c(e1@compound, e2@compound)), collapse = "+")
    validObject(e1)
    e1
})

# Subtraction  ========================

setMethod("-", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Subtraction", e1, e2))

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
#' \code{alpha - 1} is a shorthand for converting an alpha value into an epsilon value. 
#' Returns epsilon value in units depending on globally
#' defined \code{use_permil()}.
#' @name arithmetic 
#' @rdname arithmetic
NULL

# alpha - 1 = epsilon
setMethod("-", signature(e1 = "Alpha", e2 = "numeric"), function(e1, e2) {
    if (e2 == 1L)
        return(as.epsilon(e1))
    callNextMethod(e1, NULL)
})

# Multiplication ========================

setMethod("*", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Multiplication", e1, e2))

#' @usage alpha * ratio
#' @details
#' \code{alpha*ratio}, \code{alpha*alpha}, \code{alpha*delta} are a shorthand for 
#' fractionating an isotope object with an alpha fractionation factor,
#' see \code{\link{fractionate}} for details
#' @name arithmetic 
#' @rdname arithmetic
NULL

# alpha * ratio and ratio * alpha (always the weight of the ratio is carried over, alpha is considered a modifier)
setMethod("*", signature(e1 = "Alpha", e2 = "Ratio"), function(e1, e2) fractionate(e1, e2))
setMethod("*", signature(e1 = "Ratio", e2 = "Alpha"), function(e1, e2) e2 * e1) # just reverse

# alpha * alpha (weight of first alpha is carried)
setMethod("*", signature(e1 = "Alpha", e2 = "Alpha"), function(e1, e2) fractionate(e1, e2))

# alpha * delta(x) and delta(x) * alpha (always the weight of the delta value is carried over, alpha considered a modifier)
setMethod("*", signature(e1 = "Alpha", e2 = "Delta"), function(e1, e2) fractionate(e1, e2))
setMethod("*", signature(e1 = "Delta", e2 = "Alpha"), function(e1, e2) e2 * e1) # just reverse

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
setMethod("*", signature(e1 = "Alphas", e2 = "Ratios"), function(e1, e2) stop("theoretically permissible but might not be worth implementing"))
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
#' \code{ratio/ratio} allows the creation of an isotope \code{\link{alpha}} object (a fractionation factor)
#' @name arithmetic 
#' @rdname arithmetic
NULL

# ratio / ratio = alpha (weight of first ratio carried)
setMethod("/", signature(e1 = "Ratio", e2 = "Ratio"), function(e1, e2) {
    iso_attribs_check(e1, e2, include = c("isoname", "major"), text = "cannot generate a fractionaton factor from two ratio objects")
    e1@.Data <- e1@.Data / e2@.Data 
    recast_isoval(e1, "Alpha", list(compound2 = e2@compound))
})

#' @usage alpha / alpha
#' @details
#' \code{alpha/ alpha} allows the creation of another isotope \code{\link{alpha}} object but requires that
#' either the denominator names or numerator names of the two alpha objects are identical (i.e. they "cancel")
#' @name arithmetic 
#' @rdname arithmetic
NULL

# alpha / alpha (weight of first alpha is carried)
setMethod("/", signature(e1 = "Alpha", e2 = "Alpha"), function(e1, e2) {
    iso_attribs_check(e1, e2, include = c("isoname", "major"), text = "cannot generate a fractionation factor from two fractionation factors")
    if (e1@compound2 == e2@compound2) 
        e1@compound2 <- e2@compound # denominators cancel
    else if (e1@compound == e2@compound) 
        e1@compound <- e2@compound2 # numerators cancel
    else
        stop(sprintf("cannot combine two fractionation factors if neither their denominators (%s and %s) nor their numerators (%s and %s) cancel", 
                     e1@compound2, e2@compound2, e1@compound, e2@compound))
    e1@.Data <- e1@.Data / e2@.Data
    recast_isoval(e1, "Alpha")
})

#' @usage delta / delta
#' @details
#' \code{delta/delta} creates a \code{\link{epsilon}} fractionation factor object that
#' describes the fractionation factor between the two compounds, requires the reference
#' name of the two delta values to be identical. Returns epsilon value in units depending on globally
#' defined \code{use_permil()}. Works identically for two epsilon values.
#' @name arithmetic 
#' @rdname arithmetic
NULL

# delta/delta = epsilon (weight of first delta is carried)
setMethod("/", signature(e1 = "Epsilon", e2 = "Epsilon"), function(e1, e2) {
    a <- as.alpha(e1, e2)
    as.epsilon(a)
})

