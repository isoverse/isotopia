# Detailed documentation is in the functions that generate instances of these classes.

# Isotope value as the basis for any ratio, abundance, delta value or ion count
setClass("Isoval", representation(isoname = "character", major = "character"), contains = "numeric", 
         prototype = prototype(numeric(), isoname = "", major = ""))
setMethod("initialize", "Isoval", function(.Object, ...){
    if (nargs() > 1 && is(..1, "Isoval"))
        stop("Cannot initialize an isotope value with another isotope value.\n",
             " To convert between isotope data types, please use as.ratio(), as.abundance(), etc. instead")
    callNextMethod(.Object, ...)
})

# Enable regular subsetting of all isotope values (while maintaining their status as an isotope value class)
setMethod("[", "Isoval", function(x, i) { 
    x@.Data <- x@.Data[i]
    x 
})

# Abundance
setClass("Abundance", contains="Isoval")

# Ratio
setClass("Ratio", contains = "Isoval")

# Delta
setClass("Delta", representation(ref = "numeric"), contain = "Isoval",
         prototype = prototype(new("Isoval"), ref = NA_real_))

# Ion intensity
setClass("Intensity", representation(unit = "character"), contains = "Isoval",
         prototype = prototype(new("Isoval"), unit = ""))

# Isotope Systems
setClass("Isosys", representation(isoval_class = "character"), contains = "data.frame",
         prototype = prototype(data.frame(), isoval_class = "Isoval"))
setMethod("initialize", "Isosys", function(.Object, ...){
    # generate data frame and with the correct column names
    params <- list(...)
    
    # update isovalue column names with the names stored in the isotope value objects
    if (length(params) > 0 && any(val <- sapply(params[[1]], is.isoval))) {
        iso_idx <- which(val)
        names(params[[1]])[iso_idx] <- make.unique(
            sapply(params[[1]][iso_idx], function(i) {
                if (nchar(i@isoname) == 0) 'iso' else i@isoname
            }, simplify = TRUE))
    }
    
    do.call(callNextMethod, c(list(.Object), params))
})

# Enable regular subsetting of an Isosys class (as if it was a regulr data.frame) --> also enables proper subsetting with subset
setMethod("[", "Isosys", function(x, i, j, ..., drop = TRUE) { 
    if (nargs() == 2 || (nargs() == 3 && !missing(drop))) { # single paramter provided --> use as column marker like in regular data frame
        j <- if (missing(i)) 1:length(x) else i
        i <- 1:nrow(x)
    } else { # both i and j
        j <- if (missing(j)) 1:length(x) else j
        i <- if (missing(i)) 1:nrow(x) else i
    }
    if (is.character(j))
        j <- match(j, names(x))
    
    #message("selection i: ", paste(i, collapse=", "), " and j: ", paste(j, collapse=", "))
    
    df <- data.frame(x@.Data, stringsAsFactors = F)
    names(df) <- names(x) 
    df <- df[i, j, drop = FALSE]
    if (drop && ncol(df) == 1) {
        return (df[,,drop = TRUE])
    } else {
        if (!any(sapply(df, is.isoval)))
            return(df) # no longer any isotope values in the subset data frame
        else 
            return(new(class(x), df)) # return the Isosys object
    }
})

setClass("Abundances", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "Abundance"))
setClass("Ratios", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "Ratio"))
setClass("Deltas", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "Delta"))
setClass("Intensities", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "Intensity"))

#' enable conversion back to a normal data frame
#' this can also be done simply by running data.frame(x)
#' @export
as.data.frame.Isosys <- function(x, ..., stringsAsFactors = default.stringsAsFactors()){
    df <- data.frame(x@.Data, stringsAsFactors = stringsAsFactors)
    names(df) <- names(x) 
    df
}
    