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
        stop("don't use this to modify non isoval objects, can be tricky to modify attributes",
             call. = FALSE)
    attr(iso, "class") <- attr(new(to_class), "class")
    attributes(iso) <- modifyList(attributes(iso), mods)
    if (validate)
        validObject(iso)
    iso
}

# small function that informs about conversion errors
conversion_error <- function(from, to) {
    stop(sprintf("Don't know how to convert object of class %s to %s. ", class(from)[1], to),
         if (is(from, "numeric")) "Please us the appropriate functions - ratio(), abundance(), delta(), etc. - to initialize new isotope objects.", call. = FALSE)
}

# to.ratio =============================================

#' Convert to isotope ratio
#' 
#' \code{to_ratio} converts another isotopic data type to a ratio. 
#' The \code{to_r} function is a shorthand for \code{to_ratio} but otherwise identical.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{ratio}} object if iso can be converted to a \code{\link{ratio}}, an error otherwise
#' @rdname to_ratio
#' @family data type conversions
#' @method to_ratio
#' @export
setGeneric("to_ratio", function(iso) standardGeneric("to_ratio"))

#' @method to_ratio
#' @export
setMethod("to_ratio", "ANY", function(iso) conversion_error(iso, "isotope ratio"))

#' @rdname to_ratio
#' @export
to_r <- function(iso) to_ratio(iso)

setMethod("to_ratio", "Ratio", function(iso) iso)
setMethod("to_ratio", "Ratios", function(iso) iso)

# abundance to ratio
setMethod("to_ratio", "Abundance", function(iso) {
    # could convert to an Abundances system but that's slightly slower and you introduce a lot of overhead with the data frame initialization
    iso <- switch_notation(iso, "raw") # first convert to raw before doing any math
    iso@.Data <- iso@.Data / (1 - iso@.Data) # abundance to ratio
    recast_isoval(iso, "Ratio")
}) 

setMethod("to_ratio", "Abundances", function(iso) {
    convert_isosys(iso, "Ratios", 
       function(df) {
           # convert abundance to ratio
           abs <- rowSums(get_value(df)) # FIXME this requires a switch_notation!!
           lapply(df, function(ab) {
               ab <- switch_notation(ab, "raw") # first convert to raw before doing any math
               ab@.Data <- ab@.Data / (1 - abs) # abundance to ratio
               recast_isoval(ab, "Ratio")
           })
       })
})

# intensity to ratio ========
setMethod("to_ratio", "Intensities", function(iso) {
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
            stop("none of the isotopes in this system of intensities could be identified as the major ion", call. = FALSE)
        else if (s > 1)
            stop("there was more than one isotope identified as the major ion",
                 call. = FALSE)
        
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
setMethod("to_ratio", "Delta", function(iso) {
    if (length(iso@ref_ratio) == 0) {
        # no reference, let's see if we can find one
        stds <- get_standards(minor = iso@isoname, major = iso@major, name = iso@compound2)
        if (length(stds) == 0)
            message("No reference ratio registered with the delta value, tried to find one from the registered standards but none were found, delta: ", get_label(iso))
        else if (length(stds) > 1)
            message("No reference ratio registered with the delta value, tried to find one from the registered standards but found multiple, delta: ", get_label(iso))
        else if (length(stds) == 1) {
            message("Successfully found a registered standard to convert delta value: ", get_label(stds[[1]]), ": ", signif(get_value(stds[[1]]), 4))
            iso@ref_ratio <- get_value(stds[[1]])
        }
    }
    
    # continue as usual
    if (length(iso@ref_ratio) != 1)
        stop("cannot convert from a ratio to a delta value without the reference ratio set in the delta value object", call. = FALSE)
    iso <- switch_notation(iso, "raw") # convert delta value to raw (more for final object to be of type 'raw')
    a <- to_ff(iso) # switch delta to alpha value
    iso@.Data <- get_value(a, "alpha") * iso@ref_ratio # could do it by multiplying alpha value by ratio but then have to generate appropriate ratio object first
    recast_isoval(iso, "Ratio", list(compound2 = NULL, ref_ratio = NULL))
})

# to.abundance =============================================

#' Convert to isotope abundance
#' 
#' \code{to_abundance} converts another isotopic data type to an abundance.
#' The \code{to_ab} function is a shorthand for \code{to_abundance} but otherwise identical.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{abundance}} object if iso can be converted to a \code{\link{abundance}}, an error otherwise
#' @rdname to_abundance
#' @family data type conversions
#' @method to_abundance
#' @export
setGeneric("to_abundance", function(iso) standardGeneric("to_abundance"))

#' @method to_abundance
#' @export
setMethod("to_abundance", "ANY", function(iso) conversion_error(iso, "isotope abundance"))
setMethod("to_abundance", "Abundance", function(iso) iso)
setMethod("to_abundance", "Abundances", function(iso) iso)

#' @rdname to_abundance
#' @export
to_ab <- function(iso) to_abundance(iso)

# ratio to abundance
setMethod("to_abundance", "Ratio", function(iso)  {
    iso <- switch_notation(iso, "raw") # first convert to raw before doing any math
    iso@.Data <- iso@.Data / (1 + iso@.Data) # ratio to abundance
    switch_notation(recast_isoval(iso, "Abundance"), get_iso_opts("default_ab_notation"))
}) 

setMethod("to_abundance", "Ratios", function(iso) {
    convert_isosys(iso, "Abundances", 
                   function(df) {
                       # convert ratio to abundance
                       rs <- rowSums(get_value(df))
                       lapply(df, function(r) {
                           r@.Data <- r@.Data / (1 + rs) # ratio to abundance
                           switch_notation(recast_isoval(r, "Abundance"), get_iso_opts("default_ab_notation"))
                       })
                   })
})

# intensity to abundance
setMethod("to_abundance", "Intensities", function(iso) to_abundance(to_ratio(iso)))

# delta to abundance
setMethod("to_abundance", "Delta", function(iso) {
    # FIXME: add warning for isotope systems with more than one ratio?
    r <- to_ratio(iso)
    to_abundance(r)
})

# to.ff =============================================


#' Fractionation factor
#' 
#' @description
#' Calculate/convert to an isotope \code{\link{fractionation_factor}}
#' 
#' @usage to_ff(iso1, iso2)
#' @details
#' The \code{to_ff(...)} function calculates the fractionation factor between two isotope data objects
#' (for example two \code{\link{delta}} values, two \code{\link{ratio}}, or two \code{\link{ff}}).
#' All calculatinos are only permissible if the isotope values have matching
#' attributes and fractionation factors will be returend in the default notation
#' (see \code{\link{set_iso_opts}} for details)
#'
#' @param iso1 the top compound in the fractionation factor
#' @param iso2 the bottom compound in the fractionation factor
#' @return isotope \code{\link{fraction_factor}} object if parameters can be converted, an error otherwise 
#' @note 
#' Some of the conversions are also implemented in arithmetic shorthand, for example to generate
#' an fractionation factor in alpha notation from two ratios 
#' \code{to_ff(ratio(), ratio())} is the same as \code{ratio() / ratio()}.
#' See \link{arithmetic} for details.
#' @family data type conversions
#' @method to_ff
#' @export
setGeneric("to_ff", function(iso1, iso2) standardGeneric("to_ff"))

#' @method to_ff
#' @export
setMethod("to_ff", "ANY", function(iso1, iso2) conversion_error(iso1, "fractionation factor value (ratio of ratios)"))
setMethod("to_ff", signature("FractionationFactor", "missing"), function(iso1, iso2) iso)
setMethod("to_ff", signature("FractionationFactors", "missing"), function(iso1, iso2) iso)

# two ratios to fractionation factor in alpha notation (uses the arithmetic shorthand) 
setMethod("to_ff", signature("Ratio", "Ratio"), function(iso1, iso2) iso1/iso2)
# Note: to allow this for entire isotope systems --> need to implement it properly with matching the right columns


# delta/epsilon to alpha
setMethod("to_ff", signature("Delta", "missing"), function(iso1, iso2) {
    iso1 <- switch_notation(iso1, "permil") # deltas and ffs are the same when in permil notation
    iso1 <- recast_isoval(iso1, "FractionationFactor", list(ref_ratio = NULL))
    switch_notation(iso1, get_iso_opts("default_ff_notation"))
})

# two delta to fractionation factor (ff between the two compounds)
setMethod("to_ff", signature("Delta", "Delta"), function(iso1, iso2) {
    to_ff(iso1) / to_ff(iso2) # arithmetic operator is defined and takes care of the all the proper type checks
})


# to.delta =============================================

#' Convert to delta value
#' 
#' \code{to_delta} converts another isotopic data type to a delta value.
#' The \code{to_d} function is a shorthand for \code{to_delta} but otherwise identical.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @param ref_ratio the refernce ratio associated with the delta value. This is optional but required if planning
#' later conversions back to ratios or abundane values. Can be supplied as a raw numeric numer or a Ratio object
#' (in the case of the latter, the compound name of the Ratio object will be registered as the name of the
#' reference).
#' @return isotope \code{\link{delta}} object if iso can be converted to a \code{\link{delta}}, an error otherwise
#' @rdname to_delta
#' @family data type conversions
#' @method to_delta
#' @export
setGeneric("to_delta", function(iso, ref_ratio) standardGeneric("to_delta"))

#' @rdname to_delta
#' @export
to_d <- function(iso, ref_ratio) to_delta(iso, ref_ratio)

#' @method to_delta
#' @export
setMethod("to_delta", "ANY", function(iso, ref_ratio) conversion_error(iso, "delta value"))
setMethod("to_delta", signature(iso = "Isosys", ref_ratio = "missing"), function(iso, ref_ratio) {
    convert_isosys(iso, "Deltas", function(df) lapply(as.data.frame(df), function(i) to_delta(i)))
})
setMethod("to_delta", signature(iso = "Delta", ref_ratio = "missing"), function(iso, ref_ratio) {
    iso
})


# fractionation factor to delta ====
setMethod("to_delta", signature(iso = "FractionationFactor", ref_ratio = "missing"), function(iso, ref_ratio) {
    to_delta(iso, numeric())
})

setMethod("to_delta", signature(iso = "FractionationFactor", ref_ratio = "numeric"), function(iso, ref_ratio) {
    to_delta(iso, ratio(ref_ratio))
})

# epsilon/delta to delta =========
setMethod("to_delta", signature(iso = "FractionationFactor", ref_ratio = "Ratio"), function(iso, ref_ratio) {
    # make sure Ratio has the appropriate values
    if (length(ref_ratio) > 1 && length(ref_ratio) != length(iso))
        stop("a vector of reference ratios for a delta value must have the same number of entries as the value, found ",
             length(iso), " vs ", length(ref_ratio), call. = FALSE)
    if (nchar(ref_ratio@isoname) > 0 && nchar(iso@isoname) > 0 && iso@isoname != ref_ratio@isoname)
        stop(sprintf("reference ratio for a delta value cannot be for a different isotope, found '%s' vs '%s'", iso@isoname, ref_ratio@isoname), call. = FALSE)
    if (nchar(ref_ratio@major) > 0 && nchar(iso@major) > 0 && iso@major != ref_ratio@major)
        stop(sprintf("reference ratio for a delta value cannot have a different major isotope, found '%s' vs '%s'", iso@major, ref_ratio@major), call. = FALSE)
    if (nchar(ref_ratio@compound) > 0 && nchar(iso@compound2) > 0 && iso@compound2 != ref_ratio@compound)
        stop(sprintf("reference ratio for a delta value cannot be a different compound than already specified, found '%s' vs '%s'", 
                     iso@compound2, ref_ratio@compound), call. = FALSE)
    
    # cast as delta value
    iso <- switch_notation(iso, "permil") # switch fractionation factor to permil (same as a delta value but without ref_ratio)
    iso <- recast_isoval(iso, "Delta", list(ref_ratio = get_value(ref_ratio)))
    if (nchar(ref_ratio@compound) > 0) # update compound name
        iso@compound2 <- ref_ratio@compound
    switch_notation(iso, get_iso_opts("default_delta_notation"))
})


# ratio to delta =========

# ratio to delta (with numeric ref ratio)
setMethod("to_delta", signature("Ratio", "missing"), function(iso, ref_ratio) {
    stop("Can't convert from a ratio to a delta value without a reference ratio.",
         call. = FALSE)
})


# ratio to delta (with numeric ref ratio)
setMethod("to_delta", signature("Ratio", "numeric"), function(iso, ref_ratio) {
    to_delta(iso, update_iso(ratio(ref_ratio), list(isoname = iso@isoname, major = iso@major)))
})


#FIXME: should allow multiple values for a ref_ratio!?

# ratio to delta (with Ratio object as ref ratio)
setMethod("to_delta", signature("Ratio", "Ratio"), function(iso, ref_ratio) {
    if (length(ref_ratio) != 1)
        stop("reference ratio for a delta value object must be exactly one numeric value, supplied ", length(ref_ratio), call. = FALSE)
    
    # convert to fractionation factor
    iso2 <- switch_notation(ref_ratio, "raw") # first convert to raw before doing any math
    iso2@.Data <- rep(get_value(ref_ratio), length(iso)) # get ref_ratio to the right length
    
    # allow for undefined ratios to adopt the ref ratio attributes
    if (iso@isoname == "" && iso@major == "") {
      iso@isoname <- iso2@isoname
      iso@major <- iso2@major
    }
    a <- to_ff(iso, iso2) 
    
    # to delta
    to_delta(a, ref_ratio = ref_ratio)
})

setMethod("to_delta", signature(iso = "Ratios", ref_ratio = "Ratios"), function(iso, ref_ratio) {
    stop("This is the proper way to convert from a system of ratios to Deltas",
         " but not implemented yet: have to match the ref_ratio Ratios apppropriately",
         call. = FALSE)
})

# abundance to delta =====
setMethod("to_delta", signature("Abundance", "ANY"), function(iso, ref_ratio) {
    stop("not currently implemented, probably won't permit because it's dangerous transforming to ratio without taking the",
         "whole isotope system into consideration --> implement on the Abundances/Ratios level instead", call. = FALSE)
})





