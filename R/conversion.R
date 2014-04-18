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

#' Convert to delta value
#' 
#' \code{as.delta} converts another isotopic data type to a delta value.
#'
#' @param iso isotopic data object (\code{\link{ratio}}, \code{\link{abundance}}, \code{\link{delta}}, etc.)
#' @return isotope \code{\link{delta}} object if iso can be converted to a \code{\link{delta}}, an error otherwise
#' @rdname as.delta
#' @family data type conversions
#' @export
#' @genericMethods
setGeneric("as.delta", function(iso) standardGeneric("as.delta"))
setMethod("as.delta", "ANY", function(iso) conversion_error(iso, "delta value"))
setMethod("as.delta", "Delta", function(iso) iso)
setMethod("as.delta", "Deltas", function(iso) iso)

# ratio conversions ===================================

setMethod("as.abundance", "Ratio", function(iso) as.abundance(new("Ratios", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.abundance", "Ratios", function(iso) {
    convert_isosys(iso, "Abundances", 
                   function(df) {
                       # convert ratio to abundance
                       rs <- rowSums(as.value(df))
                       lapply(df, function(r) {
                           r@.Data <- r@.Data / (1 + rs) # ratio to abundance
                           attr(r, "class") <- attributes(new("Abundance"))$class
                           validObject(r)
                           r
                           #recast_isoval(r, "abundance")
                       })
                   })
})

# abundance conversions ===================================

setMethod("as.ratio", "Abundance", function(iso) as.ratio(new("Abundances", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.ratio", "Abundances", function(iso) {
    convert_isosys(iso, "Ratios", 
        function(df) {
            # convert abundance to ratio
            abs <- rowSums(as.value(df))
            lapply(df, function(ab) {
                ab@.Data <- ab@.Data / (1 - abs) # abundance to ratio
                attr(ab, "class") <- attributes(new("Ratio"))$class
                validObject(ab)
                ab
            })
        })
})

# delta conversions ===================================

setMethod("as.ratio", "Deltas", function(iso) {
    stop("not implemented")
    # now can do proper conversion
    # --> validation whether everything is in order for conversion will happen at this point!
    # i.e. checks that all have same major isotope, have corrent ratio defined, etc., etc.
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




