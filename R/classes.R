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

#' Update the attributes of an isotope value object
#' @genericMethods
setGeneric("update", function(obj, isoname, attribs) standardGeneric("update"))
setMethod("update", "Isoval", function(obj, isoname, attribs) {
    if (!is.null(isoname) && nchar(isoname) > 0){
        if (nchar(obj@isoname) > 0 && obj@isoname != isoname) 
            warning("changing the name of a '", class(obj), " value' object from '", obj@isoname, "' to '", isoname, "'")
        obj@isoname <- isoname
    }
    if (!is.null(major <- attribs$major) && nchar(major) > 0) {
        if (nchar(obj@major) > 0 && obj@major != major)
            warning("changing the major isotope of a '", class(obj), " value' object from '", obj@major, "' to '", major, "'")
        obj@major <- major
    }
    obj
})


# Abundance
setClass("Abundance", contains="Isoval")

# Ratio
setClass("Ratio", contains = "Isoval")

# Delta
setClass("Delta", representation(ref = "numeric"), contain = "Isoval",
         prototype = prototype(new("Isoval"), ref = NA_real_))
setMethod("update", "Delta", function(obj, isoname, attribs) {
    obj <- callNextMethod(obj, isoname, attribs)
    # IMPELEMENT ME - take different ref options from attribs and match them to this objects isoname
})


# Ion intensity
setClass("Intensity", representation(unit = "character"), contains = "Isoval",
         prototype = prototype(new("Isoval"), unit = ""))
setMethod("update", "Intensity", function(obj, isoname, attribs) {
    obj <- callNextMethod(obj, isoname, attribs)
    if (!is.null(unit <- attribs$unit) && nchar(unit) > 0) {
        if (nchar(obj@unit) > 0 && obj@unit != unit)
            warning("changing the unit of a '", class(obj), " value' object from '", obj@unit, "' to '", unit, "'")
        obj@unit <- unit
    }
    obj
})

# Isotope Systems
setClass("Isosys", contains = "data.frame")
setClass("Abundances", contains = "Isosys")
setClass("Ratios", contains = "Isosys")
setClass("Deltas", contains = "Isosys")
setClass("Intensities", contains = "Isosys")


# FIXME
# FIXME
# FIXME
# implement subset and [[ ]] for isosys objects? so that they columns remain Isoval and the
# data frame remains Isosys
# --> I think this could be done simply by modifying the [] of Isoval and hopefully
# this will also take care of maintaning the Ratio object if the whole data frame is subset
# but that I will have to find out
# something like this: setMethod("[", "Ratio", function(x) { print("hello"); callNextMethod(x) })


# Initialize Isosys 
# FIXME - none of this should be necessary anymore!
# setMethod(initialize, "Isosys", function(.Object, ...) { 
#     object <- callNextMethod(.Object, ...) 
#     # --> update column names with data type names
#     names(object) <- make.unique(
#         sapply(object, function(i) {
#             if (nchar(i@isoname) == 0) 'iso' else i@isoname
#         }, simplify = TRUE))
#     
#     # --> propagate major isotope name to data types
#     if (nchar(object@major) > 0) {
#         cols_with_major <- which(sapply(object, function(i) "major" %in% slotNames(i)))
#         for (i in names(object[cols_with_major])) {
#             column <- object[[i]]
#             column@major <- object@major
#             validObject(column)
#             # unfortunately, using regular [[]] syntax drops the S4 class, hence this workaround
#             # maybe there is a better way to preserve Isosys object and make the change?
#             eval(parse(text = paste0("object$`", i, "` <- column")))
#         }
#     }
#     
#     return(object)
# }) 

