#' @include classes.R
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

# definition of generics and identity conversions ===================================

#' Convert to isotope ratio
#' 
#' \code{as.ratio} converts another isotopic data type to a ratio.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{ratio}} object if iso can be converted to a \code{\link{ratio}}, an error otherwise
#' @rdname as.ratio
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.ratio", function(iso) standardGeneric("as.ratio"))
setMethod("as.ratio", "ANY", function(iso) conversion_error(iso, "isotope ratio"))
setMethod("as.ratio", "Ratio", function(iso) iso)
setMethod("as.ratio", "Ratios", function(iso) iso)

#' Convert to isotope abundance
#' 
#' \code{as.abundance} converts another isotopic data type to an abundance.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{abundance}} object if iso can be converted to a \code{\link{abundance}}, an error otherwise
#' @rdname as.abundance
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.abundance", function(iso) standardGeneric("as.abundance"))
setMethod("as.abundance", "ANY", function(iso) conversion_error(iso, "isotope abundance"))
setMethod("as.abundance", "Abundance", function(iso) iso)
setMethod("as.abundance", "Abundances", function(iso) iso)

#' Convert to alpha value
#' 
#' \code{as.alpha} converts other isotopic data types to alpha values
#'
#' @param iso1 isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @param iso2 isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{alpha}} object if iso can be converted to a \code{\link{alpha}}, an error otherwise
#' @rdname as.alpha
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.alpha", function(iso1, iso2) standardGeneric("as.alpha"))
setMethod("as.alpha", "ANY", function(iso1, iso2) conversion_error(iso, "alpha value (ratio of ratios)"))

# two ratios to alpha
setMethod("as.alpha", signature("Ratio", "Ratio"), function(iso1, iso2) {
    stop("make alpha value, check for isoname and major properly, store names")
})
### consider adding combinations Ratio / Abundance and Abundance / Abundance and Abundance / Ratio

# delta to alpha
setMethod("as.alpha", signature("Delta", "missing"), function(iso1, iso2) {
    stop("make alpha value")
})

# two deltas to alpha
setMethod("as.alpha", signature("Delta", "Delta"), function(iso1, iso2) {
    stop("make alpha value, check that the references are the same!")
    # this one can also be defined by the / operator for two delta values!
})


#' Convert to delta value
#' 
#' \code{as.delta} converts another isotopic data type to a delta value (raw, not permil!)
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{delta}} object if iso can be converted to a \code{\link{delta}}, an error otherwise
#' @rdname as.delta
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.delta", function(iso, ref_ratio) standardGeneric("as.delta"))
setMethod("as.delta", "ANY", function(iso, ref_ratio) conversion_error(iso, "delta value"))

#' \code{as.deltax} converts another isotopic data type to a delta value in permil (i.e. x 1000)
#' @rdname as.delta
#' @export
#' @genericMethods
setGeneric("as.deltax", function(iso, ref_ratio) standardGeneric("as.deltax"))
setMethod("as.deltax", "ANY", function(iso, ref_ratio) conversion_error(iso, "delta value"))

# ratio conversions ===================================

# ratio to abundance
setMethod("as.abundance", "Ratio", function(iso) as.abundance(new("Ratios", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.abundance", "Ratios", function(iso) {
    convert_isosys(iso, "Abundances", 
                   function(df) {
                       # convert ratio to abundance
                       rs <- rowSums(as.value(df))
                       lapply(df, function(r) {
                           r@.Data <- r@.Data / (1 + rs) # ratio to abundance
                           recast_isoval(r, "Abundance")
                       })
                   })
})

# ratio to delta (with numeric ref ratio)
setMethod("as.delta", signature("Ratio", "numeric"), function(iso, ref_ratio) {
    as.delta(iso, update_iso(ratio(ref_ratio), list(isoname = iso@isoname, major = iso@isoname)))
})

# ratio to delta (with Ratio object as ref ratio)
setMethod("as.delta", signature("Ratio", "Ratio"), function(iso, ref_ratio) {
    if (length(ref_ratio) != 1)
        stop("reference ratio for a delta value object must be exactly one numeric value, supplied ", length(ref_ratio))
    a <- as.alpha(iso, ref_ratio) # convert via alpha (for attribute checks)
    d <- as.delta(a)
    d@ref_ratio <- ref_ratio@compound # store reference ratio value
    d
})



# ratio to deltax
setMethod("as.deltax", signature("Ratio", "ANY"), function(iso, ref_ratio) as.deltax(as.delta(iso, ref_ratio)))

# abundance conversions ===================================

setMethod("as.ratio", "Abundance", function(iso) as.ratio(new("Abundances", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.ratio", "Abundances", function(iso) {
    convert_isosys(iso, "Ratios", 
        function(df) {
            # convert abundance to ratio
            abs <- rowSums(as.value(df))
            lapply(df, function(ab) {
                ab@.Data <- ab@.Data / (1 - abs) # abundance to ratio
                recast_isoval(ab, "Ratio")
            })
        })
})

# delta conversions ===================================

# delta to delta
setMethod("as.delta", signature(iso = "Delta", ref_ratio = "missing"), function(iso, ref_ratio) {
    if (iso@permil) { 
        iso@.Data <- iso@.Data/1000 # convert from permil to non-permil
        iso@permil <- FALSE
    }
    iso
})
setMethod("as.delta", signature(iso = "Deltas", ref_ratio = "missing"), function(iso, ref_ratio) {
    convert_isosys(iso, "Deltas", function(df) lapply(as.data.frame(df), as.delta))
})

# delta to deltax
setMethod("as.deltax", signature(iso = "Delta", ref_ratio = "missing"), function(iso, ref_ratio) {
    if  (!iso@permil) {
        iso@.Data <- iso@.Data * 1000 # convert from non-permil to permil value
        iso@permil <- TRUE
    }
    iso
})
setMethod("as.deltax", signature(iso = "Deltas", ref_ratio = "missing"), function(iso, ref_ratio) {
    convert_isosys(iso, "Deltas", function(df) lapply(as.data.frame(df), as.deltax))
})




# intensity conversions ===================================

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

setMethod("as.abundance", "Intensities", function(iso) as.abundance(as.ratio(iso)))




