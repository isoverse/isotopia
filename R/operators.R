# helper function that informs about operation error
operation_error <- function(operation, e1, e2) {
    stop(sprintf("%s is not meaningful for these isotope objects (trying to combine '%s' and '%s'). ", operation, class(e1), class(e2)))
}

# helper function that compares isotope objects' attributes with exceptions
identical_iso_attribs <- function(e1, e2, exclude = c()) {
    fun <- function(name) name %in% exclude 
    e1.attribs <- attributes(e1)
    e2.attribs <- attributes(e2)
    e1.attribs[sapply(names(e1.attribs), fun)] <- NULL
    e2.attribs[sapply(names(e2.attribs), fun)] <- NULL
    identical(e1.attribs, e2.attribs)
}

# Addition  ========================

setMethod("+", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Addition", e1, e2))
# FIXME: adding two Isosys should be permitted if they have matching columns and then are added colum by column! (and then each column added to the other can decide if it's permitted)

# adding two intensities together
setMethod("+", signature(e1 = "Intensity", e2 = "Intensity"), function(e1, e2) {
    if (!identical_iso_attribs(e1, e2, exclude = "weight"))
        stop(sprintf("trying to combine two intensity objects that don't have matching attributes:\n%s\n%s", label(e1), label(e2)))
    if (length(e1) != length(e2))
        stop(sprintf("trying to combine two intensity objects that don't have matching lengths: %s and %s", length(e1), length(e2)))
    e1@.Data <- as.value(e1) + as.value(e2)
    e1
})

# adding abundances (i.e. isotope mixing/mass balance calculations)
setMethod("+", signature(e1 = "Abundance", e2 = "Abundance"), function(e1, e2) {
    if (!identical_iso_attribs(e1, e2, exclude = c("weight", "compound")))
        stop(sprintf("trying to mix two isotope objects that don't have matching attributes:\n%s\n%s", label(e1), label(e2)))
    if (length(e1) != length(e2))
        stop(sprintf("trying to mix two isotope objects that don't have matching lengths: %s and %s", length(e1), length(e2)))
    weightsum <- as.weight(e1) + as.weight(e2)
    e1@.Data <- (as.weighted_value(e1) + as.weighted_value(e2))/weightsum
    e1@weight <- weightsum
    e1@compound <- paste(sub("^$", "?", c(e1@compound, e2@compound)), collapse = "+")
    e1
})

# adding ratios (i.e. isotope mixing/mass balance calculations --> convert to abundance and then back)
# FIXME: this might be tricky because what if it's not a whole system? and only a single ratio?
setMethod("+", signature(e1 = "Ratio", e2 = "Ratio"), function(e1, e2) {
    ab <- as.abundance(e1) + as.abundance(e2)
    as.ratio(ab)
})
setMethod("+", signature(e1 = "Ratios", e2 = "Ratios"), function(e1, e2) {
    stop("this really needs to be implemented!")
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

# Multiplication ========================

setMethod("*", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Multiplication", e1, e2))

setMethod("*", signature(e1 = "Alpha", e2 = "Ratio"), function(e1, e2) {
    stop("this is permissible but not implemented yet")
    # check that the Ratio and Alpha.compound2 are the same and convert Alpha into Ratio
})
setMethod("*", signature(e1 = "Alphas", e2 = "Ratios"), function(e1, e2) stop("theoretically permissible but might not be worth implementing"))
# + other Isosys expansion of this stuff

setMethod("*", signature(e1 = "Alpha", e2 = "Alpha"), function(e1, e2) {
    stop("this is permissible but not implemented yet")
    # check that the e1.compound2 and e2.compound1 are the same and convert Alpha with compound1 = e1.compound1 and compound2 = e2.compound2
})

setMethod("*", signature(e1 = "Delta", e2 = "Delta"), function(e1, e2) {
    stop("this is permissible but not implemented yet")
    # essentially the same as multiplying two Alpha values (in fact should convert to alpha), except the outcome is a Delta value
})

setMethod("*", signature(e1 = "Alpha", e2 = "Delta"), function(e1, e2) stop("theoretically permissible but might not be worth implementing"))
setMethod("*", signature(e1 = "Delta", e2 = "Alpha"), function(e1, e2) stop("theoretically permissible but might not be worth implementing"))
# FIXME + all the variations wit Delta x

# Division ========================

setMethod("/", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Division", e1, e2))
setMethod("/", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Division", e1, e2))
setMethod("/", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Division", e1, e2))
setMethod("/", signature(e1 = "Isosys", e2 = "Isosys"), function(e1, e2) operation_error("Division", e1, e2))

setMethod("/", signature(e1 = "Intensity", e2 = "Intensity"), function(e1, e2) {
    if (e1@unit != e2@unit)
        stop(sprintf("cannot generate an isotope ratio from two intensity objects with differing units: '%s', '%s'", e1@unit, e2@unit))
    
    oldopt <- options(warn = 2); on.exit(options(oldopt))  # throw warnings as errors for the calculations
    e1@.Data <- e1@.Data / e2@.Data 
    recast_isoval(e1, "Ratio", list(unit = NULL, major = e2@isoname))
})

