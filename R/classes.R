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
