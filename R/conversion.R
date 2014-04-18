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
    df <- as.data.frame(iso[iso_idx], stringsAsFactors = F)
    
    # converted isovals
    values <- do.call(conv_fun, list(df))
    
    # create new object
    rs <- new(class_isosys, cbind(values, iso[-iso_idx, drop = FALSE])) # keep additional data around
    
    # name and reorder
    names(rs) <- c(names(iso)[iso_idx], names(iso)[-iso_idx])
    rs[names(iso), drop = F]
}
 
#' generic function to recast an isotope value object
#' 
#' This is strictly an internal function to facilitate recasting 
#' converted data objects. It transfers all relevant slot
#' information and adds extra attributes as passed in
#' 
#' @param init_fun - initialization function
#' @param iso - isotope value object to be recast
#' @param attribs - list of attributes to overwrite
#' @note perhaps make this one faster rather than so dynamic
recast_isoval <- function(iso, init_fun, attribs = list()) {
    if (!is.isoval(iso))
        stop("Can't recast an object that isn't an isotopic value, class: ", class(iso))
    
    # find arguments of the init function and look for slots to fill with
    args <- names(formals(init_fun)) 
    args <- args[! args %in% c("...", "single_as_df")]
    slots <- lapply(
        setNames(as.list(args), args), 
        function(i) if (i %in% slotNames(iso)) slot(iso, i) else NULL) # find matching slot values
    slots[sapply(slots, is.null)] <- NULL # remove slots that were not found
    
    # add value and name if designated
    value <- list(as.numeric(iso))
    if (nchar(iso@isoname) > 0)
        names(value) <- iso@isoname
    
    # combine all attributes and call init function
    attribs <- modifyList(c(value, slots), attribs)
    do.call(init_fun, attribs)
}

# small function that informs about conversion errors
conversion_error <- function(from, to) {
    stop(sprintf("Don't know how to convert object of class %s to %s. ", class(from)[1], to),
         "Please us the approriate functions - ratio(), abundance(), delta(), etc. - to initialize new isotope objects.")
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
setMethod("as.ratio", "Ratio", identity)
setMethod("as.ratio", "Ratios", identity)

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
setMethod("as.abundance", "Abundance", identity)
setMethod("as.abundance", "Abundances", identity)

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
setMethod("as.delta", "Delta", identity)
setMethod("as.delta", "Deltas", identity)

# ratio conversions ===================================

setMethod("as.abundance", "Ratio", function(iso) as.abundance(new("Ratios", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.abundance", "Ratios", function(iso) {
    convert_isosys(iso, "Abundances", 
                   function(df) {
                       # convert ratio to abundance
                       lapply(df / (1 + rowSums(df)), recast_isoval, "abundance")
                   })
})

# abundance conversions ===================================

setMethod("as.ratio", "Abundance", function(iso) as.ratio(new("Abundances", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.ratio", "Abundances", function(iso) {
    convert_isosys(iso, "Ratios", 
        function(df) {
            # convert abundance to ratio
            lapply(df / (1 - rowSums(df)), recast_isoval, "ratio")
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
        
        major_i <- which(major)
        
        # convert intensities to ratios
        values <- df / df[[major_i]]
        values[[major_i]]@isoname <- ".MAJORISOTOPE" # will be discarded later on
        lapply(values, recast_isoval, "ratio")
    }
    
    con_val <- convert_isosys(iso, "Ratios", fun)
    con_val$.MAJORISOTOPE <- NULL # remove from ratio system
    con_val
})

setMethod("as.abundance", "Intensities", function(iso) as.abundance(as.ratio(iso)))




