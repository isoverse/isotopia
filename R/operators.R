# helper function that informs about operation error
operation_error <- function(operation, e1, e2) {
    stop(sprintf("%s is not meaningful for these isotope objects (trying to combine '%s' and '%s'). ", operation, class(e1), class(e2)))
}

# Addition  ========================

setMethod("+", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Addition", e1, e2))
setMethod("+", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Addition", e1, e2))

# Subtraction  ========================

setMethod("-", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Subtraction", e1, e2))
setMethod("-", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Subtraction", e1, e2))

# Multiplication ========================

setMethod("*", signature(e1 = "Isoval", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "Isoval", e2 = "ANY"), function(e1, e2) operation_error("Multiplication", e1, e2))
setMethod("*", signature(e1 = "ANY", e2 = "Isoval"), function(e1, e2) operation_error("Multiplication", e1, e2))

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

setMethod("/", signature(e1 = "Intensity", e2 = "Intensity"), function(e1, e2) {
    if (e1@unit != e2@unit)
        stop(sprintf("cannot generate an isotope ratio from two intensity objects with differing units: '%s', '%s'", e1@unit, e2@unit))
    
    oldopt <- options(warn = 2); on.exit(options(oldopt))  # throw warnings as errors for the calculations
    value <- e1@.Data / e2@.Data 
    ratio(setNames(list(value), e1@isoname), major = e2@isoname)
})

