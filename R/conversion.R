#' @include validation.R
NULL

# general conversion function ===================================

#' generic function to convert an isotope system that is part of a data frame
#' and stitch it back together with the columns in the proper positions. uses
#' a callback function that has to do the conversion of the isotope values
#' 
#' @param iso - the isotope system object
#' @param class_isosys - the class of the isotope system to convert to 
#' @param conv_fun - the function which converts the isotope value objects of the 
#' data frame, has to accept one parameter that is a data.frame of only the
#' isotope value objects
convert_isosys <- function(iso, class_isosys, conv_fun) {
    # make sure it's valid in case user modified the Isosys
    validObject(iso) 
    
    # pull out just the isotope version
    iso_idx <- which(sapply(iso, is.isoval))
    
    # converted isovals
    values <- do.call(conv_fun, list(df = iso[iso_idx, drop = F]))
    
    # create new object
    rs <- new(class_isosys, cbind(values, iso[-iso_idx, drop = FALSE])) # keep additional data around
    
    # name and reorder
    names(rs) <- c(names(iso)[iso_idx], names(iso)[-iso_idx])
    rs[names(iso), drop = F]
}
 
#' generic function to recast an isotopic value object during conversions
#' @param iso object
#' @param to_class - which class to cast to
#' @param mods - list of modifications to existing attributes (can be list(x = NULL) for removing attribute x)
#' @param validate - whether to validate after the recast, default TRUE
recast_isoval <- function(iso, to_class, mods = list(), validate = TRUE){
    if (!is.isoval(iso))
        stop("don't use this to modify non isoval objects, can be tricky to modify attributes")
    attr(iso, "class") <- attr(new(to_class), "class")
    attributes(iso) <- modifyList(attributes(iso), mods)
    if (validate)
        validObject(iso)
    iso
}

# small function that informs about conversion errors
conversion_error <- function(from, to) {
    stop(sprintf("Don't know how to convert object of class %s to %s. ", class(from)[1], to),
         if (is(to, "numeric")) "Please us the appropriate functions - ratio(), abundance(), delta(), etc. - to initialize new isotope objects.")
}

# to.ratio =============================================

#' Convert to isotope ratio
#' 
#' \code{as.ratio} converts another isotopic data type to a ratio.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{ratio}} object if iso can be converted to a \code{\link{ratio}}, an error otherwise
#' @rdname as.ratio
#' @family data type conversions
#' @method as.ratio
#' @export
setGeneric("as.ratio", function(iso) standardGeneric("as.ratio"))

#' @method as.ratio
#' @export
setMethod("as.ratio", "ANY", function(iso) conversion_error(iso, "isotope ratio"))
setMethod("as.ratio", "Ratio", function(iso) iso)
setMethod("as.ratio", "Ratios", function(iso) iso)

# abundance to ratio
setMethod("as.ratio", "Abundance", function(iso) as.ratio(new("Abundances", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.ratio", "Abundances", function(iso) {
    convert_isosys(iso, "Ratios", 
                   function(df) {
                       # convert abundance to ratio
                       abs <- rowSums(get_value(df))
                       lapply(df, function(ab) {
                           ab@.Data <- ab@.Data / (1 - abs) # abundance to ratio
                           recast_isoval(ab, "Ratio")
                       })
                   })
})

# intensity to ratio ========
setMethod("as.ratio", "Intensities", function(iso) {
    fun <- function(df) {
        df <- as.data.frame(df) # will be manipulating the isotope objects inside
        
        # find major isotope in system
        major <- sapply(df, function(i) {
            if (is.isoval(i) && nchar(i@isoname) > 0)
                i@major == i@isoname
            else
                FALSE
        })
        
        if ( (s <- sum(major)) == 0)
            stop("none of the isotopes in this system of intensities could be identified as the major ion")
        else if (s > 1)
            stop("there was more than one isotope identified as the major ion")
        
        # convert intensities to ratios
        major_i <- df[[which(major)]]
        df[[which(major)]]@isoname <- ".MAJORISOTOPE" # will be discarded later on
        lapply(df, function(i) i/major_i) # intensity to ratio defined by the / operator for intensities
    }
    
    con_val <- convert_isosys(iso, "Ratios", fun)
    con_val$.MAJORISOTOPE <- NULL # remove from ratio system
    con_val
})

# delta to ratio ======
setMethod("as.ratio", "Delta", function(iso) {
    if (length(iso@ref_ratio) == 0) {
        # no, reference, let's see if we can find one
        stds <- get_standards(minor = iso@isoname, major = iso@major, name = iso@compound2)
        if (length(stds) == 0)
            message("No reference ratio registered with the delta value, tried to find one from the registered standards but none were found, delta: ", label(iso))
        else if (length(stds) > 1)
            message("No reference ratio registered with the delta value, tried to find one from the registered standards but found multiple, delta: ", label(iso))
        else if (length(stds) == 1) {
            message("Successfully found a matching standard to convert delta value without registered reference: ", label(stds[[1]]), ": ", get_value(stds[[1]]))
            iso@ref_ratio <- get_value(stds[[1]])
        }
    }
    
    # continue as usual
    if (length(iso@ref_ratio) != 1)
        stop("cannot convert from a ratio to a delta value without the reference ratio set in the delta value object")
    a <- as.alpha(iso)
    iso@.Data <- a@.Data * iso@ref_ratio # could do it by multiplying alpha value by ratio but then have to generate appropriate ratio object first
    recast_isoval(iso, "Ratio", list(compound2 = NULL, permil = NULL, ref_ratio = NULL))
})

# to.abundance =============================================

#' Convert to isotope abundance
#' 
#' \code{as.abundance} converts another isotopic data type to an abundance.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{abundance}} object if iso can be converted to a \code{\link{abundance}}, an error otherwise
#' @rdname as.abundance
#' @family data type conversions
#' @method as.abundance
#' @export
setGeneric("as.abundance", function(iso) standardGeneric("as.abundance"))

#' @method as.abundance
#' @export
setMethod("as.abundance", "ANY", function(iso) conversion_error(iso, "isotope abundance"))
setMethod("as.abundance", "Abundance", function(iso) iso)
setMethod("as.abundance", "Abundances", function(iso) iso)

# ratio to abundance
setMethod("as.abundance", "Ratio", function(iso) as.abundance(new("Ratios", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.abundance", "Ratios", function(iso) {
    convert_isosys(iso, "Abundances", 
                   function(df) {
                       # convert ratio to abundance
                       rs <- rowSums(get_value(df))
                       lapply(df, function(r) {
                           r@.Data <- r@.Data / (1 + rs) # ratio to abundance
                           recast_isoval(r, "Abundance")
                       })
                   })
})

# intensity to abundance
setMethod("as.abundance", "Intensities", function(iso) as.abundance(as.ratio(iso)))

# delta to abundance
setMethod("as.abundance", "Delta", function(iso) {
    r <- as.ratio(iso)
    as.abundance(r)
})

# to.alpha =============================================

#' Convert to alpha value
#' 
#' \code{as.alpha} converts other isotopic data types to alpha values (fractionation factors)
#' 
#' Some of the conversions are also implemented in arithmetic shorthand, for example to generate
#' an alpha value from two ratios, \code{as.alpha(ratio(), ratio())} is the same as \code{ratio() / ratio()}.
#' See \link{arithmetic} for details.
#'
#' @param iso1 isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @param iso2 isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{alpha}} object if iso can be converted to a \code{\link{alpha}}, an error otherwise
#' @rdname as.alpha
#' @family data type conversions
#' @method as.alpha
#' @export
setGeneric("as.alpha", function(iso1, iso2) standardGeneric("as.alpha"))

#' @method as.alpha
#' @export
setMethod("as.alpha", "ANY", function(iso1, iso2) conversion_error(iso1, "alpha value (ratio of ratios)"))

# two ratios to alpha (uses the arithmetic shorthand) 
setMethod("as.alpha", signature("Ratio", "Ratio"), function(iso1, iso2) iso1/iso2)
# Note: to allow this for entire isotope systems --> need to implement it properly with matching the right columns

# delta/epsilon to alpha
setMethod("as.alpha", signature("Epsilon", "missing"), function(iso1, iso2) {
    iso1@.Data <- as.epsilon(iso1, permil = FALSE)@.Data + 1 # make sure converting from an epsilon/delta value stripped of it's 1000x factor!
    recast_isoval(iso1, "Alpha", list(permil = NULL, ref_ratio = NULL))
})

# two epsilon/deltas to alpha (fractionation factor between the two compounds)
setMethod("as.alpha", signature("Epsilon", "Epsilon"), function(iso1, iso2) {
    as.alpha(iso1) / as.alpha(iso2) # arithmetic operator is defined and takes care of the all the proper type checks
})

# to.epsilon =============================================

#' Convert to epsilon value
#' 
#' \code{as.delta} converts another isotopic data type to an epsilon values
#'
#' @param iso isotopic data object (e.g. \code{\link{alpha}})
#' @param permil whether to generate epsilon object in permil values (x1000) or not
#' @return isotope \code{\link{epsilon}} object if iso can be converted to an \code{\link{epsilon}}, an error otherwise
#' @rdname as.epsilon
#' @family data type conversions
#' @method as.epsilon
#' @export
setGeneric("as.epsilon", function(iso, permil = use_permil()) standardGeneric("as.epsilon"))

#' @method as.epsilon
#' @export
setMethod("as.epsilon", "ANY", function(iso, permil = use_permil()) conversion_error(iso, "epsilon value"))
setMethod("as.epsilon", "Isosys", function(iso, permil = use_permil()) 
    convert_isosys(iso, "Epsilons", function(df) lapply(as.data.frame(df), function(i) as.epsilon(i, permil = permil))))

# alpha to epsilon
setMethod("as.epsilon", signature(iso = "Alpha"), function(iso, permil = use_permil()) {
    iso@.Data <- iso@.Data - 1
    as.epsilon(recast_isoval(iso, "Epsilon", list(permil = FALSE)), permil = permil)
})

# epsilon/delta to epsilon
setMethod("as.epsilon", signature(iso = "Epsilon"), function(iso, permil = use_permil()) {
    if (!permil && iso@permil) 
        iso@.Data <- iso@.Data/1000 # convert from permil to non-permil
    else if (permil && !iso@permil)
        iso@.Data <- iso@.Data * 1000 # convert from non-permil to permil value
    iso@permil <- permil
    recast_isoval(iso, "Epsilon", list(ref_ratio = NULL))
})


# to.delta =============================================

#' Convert to delta value
#' 
#' \code{as.delta} converts another isotopic data type to a delta value
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @param ref_ratio the refernce ratio associated with the delta value. This is optional but required if planning
#' later conversions back to ratios or abundane values. Can be supplied as a raw numeric numer or a Ratio object
#' (in the case of the latter, the compound name of the Ratio object will be registered as the name of the
#' reference).
#' @param permil whether to generate epsilon object in permil values (x1000) or not
#' @return isotope \code{\link{delta}} object if iso can be converted to a \code{\link{delta}}, an error otherwise
#' @rdname as.delta
#' @family data type conversions
#' @method as.delta
#' @export
setGeneric("as.delta", function(iso, ref_ratio, permil = use_permil()) standardGeneric("as.delta"))

#' @method as.delta
#' @export
setMethod("as.delta", "ANY", function(iso, ref_ratio, permil = use_permil()) conversion_error(iso, "delta value"))
setMethod("as.delta", signature(iso = "Isosys", ref_ratio = "missing"), function(iso, ref_ratio, permil = use_permil()) {
    convert_isosys(iso, "Deltas", function(df) lapply(as.data.frame(df), function(i) as.delta(i, permil = permil)))
})

# epsilon/delta to delta =========
setMethod("as.delta", signature(iso = "Epsilon", ref_ratio = "Ratio"), function(iso, ref_ratio, permil = use_permil()) {
    # make sure Ratio has the appropriate values
    if (length(ref_ratio) != 1)
        stop("reference ratio for a delta value object must be exactly one numeric value, supplied ", length(ref_ratio))
    if (nchar(ref_ratio@isoname) > 0 && nchar(iso@isoname) > 0 && iso@isoname != ref_ratio@isoname)
        stop(sprintf("reference ratio for a delta value cannot be for a different isotope, found '%s' vs '%s'", iso@isoname, ref_ratio@isoname))
    if (nchar(ref_ratio@major) > 0 && nchar(iso@major) > 0 && iso@major != ref_ratio@major)
        stop(sprintf("reference ratio for a delta value cannot have a different major isotope, found '%s' vs '%s'", iso@major, ref_ratio@major))
    if (nchar(ref_ratio@compound) > 0 && nchar(iso@compound2) > 0 && iso@compound2 != ref_ratio@compound)
        stop(sprintf("reference ratio for a delta value cannot be a different compound than already specified, found '%s' vs '%s'", 
                     iso@compound2, ref_ratio@compound))
    old_ref_ratio <- if (is.null(attr(iso, "ref_ratio"))) numeric() else iso@ref_ratio
    if (length(old_ref_ratio) > 0 && old_ref_ratio != get_value(ref_ratio))
        stop(sprintf("reference ratio for a delta value cannot be different than previous specification, found '%s' vs '%s'", 
                     old_ref_ratio, get_value(ref_ratio)))
    
    # cast as delta value
    iso <- recast_isoval(iso, "Delta", list(ref_ratio = get_value(ref_ratio)))
    if (nchar(ref_ratio@compound) > 0) # update compound name
        iso@compound2 <- ref_ratio@compound
    as.delta(iso, permil = permil) # convert to correct permil notation
})

setMethod("as.delta", signature(iso = "Epsilon", ref_ratio = "numeric"), function(iso, ref_ratio, permil = use_permil()) {
    as.delta(iso, ratio(ref_ratio), permil = permil)
})

setMethod("as.delta", signature(iso = "Epsilon", ref_ratio = "missing"), function(iso, ref_ratio, permil = use_permil()) {
    # convert to permil/nonpermil
    if (!permil && iso@permil) 
        iso@.Data <- iso@.Data/1000 # convert from permil to non-permil
    else if (permil && !iso@permil)
        iso@.Data <- iso@.Data * 1000 # convert from non-permil to permil value    
    
    recast_isoval(iso, "Delta", 
      list(permil = permil,
           ref_ratio = if (is.null(attr(iso, "ref_ratio"))) numeric() else iso@ref_ratio))
})

# alpha to delta ====
setMethod("as.delta", signature(iso = "Alpha", ref_ratio = "ANY"), function(iso, ref_ratio, permil = use_permil()) {
    e <- as.epsilon(iso)
    if (missing(ref_ratio) || length(ref_ratio) == 0)
        as.delta(e, permil = permil)
    else
        as.delta(e, ref_ratio, permil = permil)
})


# ratio to delta =========

# ratio to delta (with numeric ref ratio)
setMethod("as.delta", signature("Ratio", "numeric"), function(iso, ref_ratio, permil = use_permil()) {
    as.delta(iso, update_iso(ratio(ref_ratio), list(isoname = iso@isoname, major = iso@isoname)), permil = permil)
})

# ratio to delta (with Ratio object as ref ratio)
setMethod("as.delta", signature("Ratio", "Ratio"), function(iso, ref_ratio, permil = use_permil()) {
    if (length(ref_ratio) != 1)
        stop("reference ratio for a delta value object must be exactly one numeric value, supplied ", length(ref_ratio))
    
    # convert to alpha value
    iso2 <- ref_ratio
    iso2@.Data <- rep(get_value(ref_ratio), length(iso)) # get ref_ratio to the right length
    a <- as.alpha(iso, iso2)
    
    # to delta
    as.delta(a, ref_ratio = ref_ratio, permil = permil)
})

setMethod("as.delta", signature(iso = "Ratios", ref_ratio = "Ratios"), function(iso, ref_ratio) {
    stop("This is the proper way to convert from a system of ratios to Deltas",
         " but not implemented yet: have to match the ref_ratio Ratios apppropriately")
})

# abundance to delta =====
setMethod("as.delta", signature("Abundance", "ANY"), function(iso, ref_ratio) {
    stop("not currently implemented, probably won't permit because it's dangerous transforming to ratio without taking the",
         "whole isotope system into consideration --> implement on the Abundances/Ratios level instead")
})





