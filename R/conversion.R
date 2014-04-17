#' @include classes.R
NULL

#' generic function to recast an isotope value object
#' 
#' This is strictly an internal function to facilitate recasting 
#' converted data objects. It transfers all relevant slot
#' information and adds extra attributes as passed in
#' 
#' @param init_fun - initialization function
#' @param attribs - list of attributes to overwrite
#' @note perhaps make this one faster rather than so dynamic
recast_isoval <- function(init_fun, iso, attribs = list()) {
    if (!is.isoval(iso))
        stop("Can't recast an object that isn't an isotopic value, class: ", class(iso))
    
    # find arguments of the init function and look for slots to fill with
    args <- names(formals(init_fun)) 
    args <- args[! args %in% c("...", "single_as_df")]
    slots <- as.list(args)
    names(slots) <- args
    slots <- lapply(slots, function(i) if (i %in% slotNames(iso)) slot(iso, i) else NULL) # find matching slot values
    slots[sapply(slots, is.null)] <- NULL # remove slots that were not found
    
    # add value and name if designated
    value <- list(as.numeric(iso))
    if (nchar(iso@isoname) > 0)
        names(value) <- iso@isoname
    
    # combine all attributes and call init function
    attribs <- modifyList(c(value, slots), attribs)
    do.call(init_fun, attribs)
}

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
setMethod("as.ratio", "ANY", function(iso) stop(sprintf("Don't know how to convert object of class %s to isotope ratio.", class(iso)[1])))
setMethod("as.ratio", "Ratio", function(iso) iso)
setMethod("as.ratio", "Ratios", function(iso) iso)
setMethod("as.ratio", "Abundance", function(iso) as.ratio(new("Abundances", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.ratio", "Abundances", function(iso) {
    validObject(iso) # make sure it's valid in case user modified the Isosys
    iso_idx <- which(sapply(iso, is.isoval))
    df <- as.data.frame(iso[iso_idx])
    
    # create new object
    rs <- new("Ratios",
        cbind(
            lapply(
                df / (1 + rowSums(df)), # convert abundance to ratio
                function(value) recast_isoval("ratio", value)),
            iso[-iso_idx, drop = FALSE]) # keep additional data around
    )
    
    # names and reorder
    names(rs) <- c(names(iso)[iso_idx], names(iso)[-iso_idx])
    rs[names(iso)]
})

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
setMethod("as.abundance", "ANY", function(iso) stop(sprintf("Don't know how to convert object of class %s to isotope abundance.", class(iso)[1])))
setMethod("as.abundance", "Abundance", function(iso) iso)
setMethod("as.abundance", "Abundances", function(iso) iso)
setMethod("as.abundance", "Ratio", function(iso) as.abundance(new("Ratios", data.frame(iso)))[1]) # converts to a system (slightly slower but tidier)
setMethod("as.abundance", "Ratios", function(iso) {
    validObject(iso) # make sure it's valid in case user modified the Isosys
    iso_idx <- which(sapply(iso, is.isoval))
    df <- as.data.frame(iso[iso_idx])
    
    # create new object
    rs <- new("Abundances",
              cbind(
                  lapply(
                      df / (1 - rowSums(df)), # convert ratio to abundance
                      function(value) recast_isoval("abundance", value)),
                  iso[-iso_idx, drop = FALSE]) # keep additional data around
    )
    
    # names and reorder
    names(rs) <- c(names(iso)[iso_idx], names(iso)[-iso_idx])
    rs[names(iso)]
})


setMethod("as.ratio", "data.frame", function(iso) {
    stop("not implemented yet")
    # implement that it basically takes all parts of the df. that are Isoval and tries to make an isosys
    # out of that portion of the data frame --> apply the as.ratio on that part of the df and cbind the thing
    # back together - only allow one type of system (they must all be the same)
})
setMethod("as.ratio", "Deltas", function(iso) {
    stop("not implemented")
    # now can do proper conversion
    # --> validation whether everything is in order for conversion will happen at this point!
    # i.e. checks that all have same major isotope, have corrent ratio defined, etc., etc.
})

