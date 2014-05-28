#' @include utils.R
NULL

# little helper function for isotope letters
# --> to be expanded for proper formatting in the future
get_iso_letter <- function(letter = c("alpha", "delta", "epsilon", "permil")) {
    letter <- match.arg(letter)
    # ideally these would be like this but this greek alphabet support doesn't really work
    #switch(letter,
    #   alpha = "α",
    #   delta = "δ",
    #   epsilon = "ε",
    #   permil = "‰")
    switch(letter,
           alpha = "alpha",
           delta = "d",
           epsilon = "eps",
           permil = "permil")
}

# Notation classes
setClass("Notation", representation(unit = "character"))
# alpha fractionation factor
setClass("Notation_alpha", contains = "Notation", prototype = prototype(unit = ""))
# alpha fractionation factor in log normal notation (NOT IMPLEMENTED YET!)
#setClass("Notation_ln", contains = "Notation", prototype = prototype(unit = ""))
# epsilon factionation factor (raw value, no multiplication)
setClass("Notation_eps", contains = "Notation", prototype = prototype(unit = ""))
# plain delta or plan fractional abundance
setClass("Notation_raw", contains = "Notation", prototype = prototype(unit = ""))
# permil delta or permil fractionation factor
setClass("Notation_permil", contains = "Notation", prototype = prototype(unit = get_iso_letter("permil")))
# ppm delta or ppm fractionation factor
setClass("Notation_ppm", contains = "Notation", prototype = prototype(unit = "ppm"))
# percent fractional abundance
setClass("Notation_percent", contains = "Notation", prototype = prototype(unit = "%"))


# Detailed documentation is in the functions that generate instances of these classes.

# Isotope value as the basis for any ratio, abundance, delta value or ion count
setClass("Isoval", 
         representation(isoname = "character", major = "character", compound = "character", notation = "Notation", 
                        weight = "numeric"), contains = "numeric", 
         prototype = prototype(numeric(), isoname = "", major = "", compound = "", notation = new("Notation_raw"), weight = numeric()))
setMethod("initialize", "Isoval", function(.Object, ...){
    if (nargs() > 1 && is(..1, "Isoval"))
        stop("Cannot initialize an isotope value with another isotope value.\n",
             " To convert between isotope data types, please use to_ratio(), to_abundance(), etc. instead")
    obj <- callNextMethod(.Object, ...)
    
    # initialize with weights = 1 if not specified
    if (length(obj@weight) == 0) 
        obj@weight <- rep(1, length(obj@.Data))
    
    obj
})

# Enable regular subsetting of all isotope values (while maintaining their status as an isotope value class)
# and keeping the weights around
setMethod("[", "Isoval", function(x, i) { 
    x@.Data <- x@.Data[i]
    x@weight <- x@weight[i]
    x 
})

setMethod("[<-", "Isoval", function(x, i, value) { 
    if (is.isoval(value) && !identical(class(x), class(value)))
        stop("cannot assign a ", class(value), " value to a ", class(x), " value")
    x@.Data[i] <- as.numeric(value)
    if (is.isoval(value))
        x@weight[i] <- value@weight
    x
})


# Abundance
setClass("Abundance", contains="Isoval")

# Ratio
setClass("Ratio", contains = "Isoval")

# FractionationFactor
setClass("FractionationFactor", representation(compound2 = "character"), contains = "Isoval",
         prototype = prototype(new("Isoval"), compound2 = ""))

# Delta
setClass("Delta", representation(compound2 = "character", ref_ratio = "numeric"), contains = "Isoval",
         prototype = prototype(new("Isoval"), compound2 = "", ref_ratio = numeric()))

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
setClass("FractionationFactors", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "FractionationFactor"))
setClass("Deltas", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "Delta"))
setClass("Intensities", contains = "Isosys",
         prototype = prototype(new("Isosys"), isoval_class = "Intensity"))

