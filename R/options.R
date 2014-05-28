

#' Isotopia options
#' 
#' This allows specifying and retrieving default values for newly
#' created isotopia objects.
#' 
#' @param default_ab_notation default notation of abundance objects,
#' see \code{\link{switch_notation}} for details
#' @param default_ff_notation default notation of fractionation factors
#' @param default_delta_notation default notation of delta values
#' @param default_intensity_unit default unit for intensity values
#' @param default_major default major isotope on all isotope objects
#' @param default minor default minor isotope on all isotope objects
#' @param standards isotope ratio objects to register as standards
#' @param exact_mass_balance NOT IMPLEMENTED YET!
#' If enabled, mass balance calculations with delta values (i.e. \code{mass_balance(delta, delta, delta...} 
#' or \code{\link{delta}() + \link{delta}()}) will always
#' be performed exact by converting to natural abundances first and making the addition in 
#' abundance space (will be converted back to delta value afterwards). 
#' This is only possible if the \code{ref_ratio} in the delta values is
#' set and will lead to an error if attempted without the reference ratios set.
#' 
#' If disabled, mass balance calculations with delta values (\code{\link{delta}() + \link{delta}()}) will
#' be performed in delta space (which is not exact but the discrepancy is negligible unless
#' the minor isotopes in an isotope system make up a significant portion)
#' 
#' see \code{\link{register_standard}} for details
#' @family options
#' @rdname iso_opts
#' @export
set_iso_opts <- function (
    default_ab_notation = c("raw", "percent"), 
    default_ff_notation = c("alpha", "eps", "permil", "ppm"), 
    default_delta_notation = c("raw", "permil", "ppm"), 
    default_intensity_unit = "", 
    default_major = "", 
    default_minor = "",
    exact_mass_balance = FALSE,
    standards = c()) {
    
    # new options
    opts <- as.list(match.call())[-1] #FIXME: this might not work with lazy evaluation!
    
    # check consistency of notations
    if (!missing(default_ab_notation)) 
        opts$default_ab_notation <- match.arg(default_ab_notation)
    if (!missing(default_ff_notation)) 
        opts$default_ff_notation <- match.arg(default_ff_notation)
    if (!missing(default_delta_notation)) 
        opts$default_delta_notation <- match.arg(default_delta_notation)
    
    # register standards
    if (!missing(standards)) {
        opts$standards <- NULL # standards are set via register
        sapply(standards, register_standard)
    }
    
    # set new options
    if (length(opts) > 0) {
        names(opts) <- paste0("isotope_", names(opts))
        do.call(options, opts) # store in R options
    }
}

#' @details
#' \code{get_iso_opts} allows retrieval of all or individual isotopia options.
#' Returns a single value if only one option is requested, a named list if multiple
#' 
#' @note 
#' Default options are the following and are set during package loading together
#' with the default standards
#' \code{\cr
#' set_iso_opts(\cr
#'    default_ab_notation = "raw", \cr
#'    default_ff_notation = "alpha", \cr
#'    default_delta_notation = "permil", \cr
#'    default_intensity_unit = "", \cr
#'    default_major = "", \cr
#'    default_minor = "",\cr
#'    exact_mass_balance = FALSE\cr
#' )
#' }
#' @examples
#' get_iso_opts("standards") # get a table of all standards
#' get_iso_opts(c("default_major", "default_minor")) # get a named list with the 
#' default major and minor isotopes
#' @rdname iso_opts
#' @export
get_iso_opts <- function (opts) {
    opts <- list(
        default_ab_notation = "raw", 
        default_ff_notation = "alpha", 
        default_delta_notation = "permil", 
        default_intensity_unit = "", 
        default_major = "", 
        default_minor = "",
        exact_mass_balance = FALSE,
        standards = 
            data.frame(minor = character(), major = character(), name = character(), ratio = numeric(), stringsAsFactors = F)
    )[opts]
    
    for (i in names(opts))
        opts[[i]] <- getOption(paste0("isotope_", i)) %||% opts[[i]]
    drop_list(opts)
}

#' Isotope standards
#' 
#' Isotopia provides functionality to register and retrieve isotope standards.
#' Registered standards can be used for automatic conversions of, for example,
#' delta values which have attributes that match a standard.
#' 
#' @details
#' Use \code{register_standard()} to register an isotope standard. This can be useful
#' for keeping track of standards you use internally and will also allow
#' conversions from \code{\link{delta}} to e.g. \code{\link{ratio}} to 
#' automatically try to find the approriate standard for the conversion
#' from the registered values.
#' 
#' @param ratio - a \code{\link{ratio}} object with 'minor', 
#' and 'major' isotope as well as 'compound' (the name of the standard) attributes all defined 
#' @family options
#' @rdname standards
#' @export
register_standard <- function(ratio) {
    if (!is.ratio(ratio))
        stop("can only register standards that are ratio isotope objects")
    
    if (length(ratio) != 1L)
        stop("must be a single ratio value, found ", length(ratio))
    
    if (nchar(ratio@isoname) == 0 || nchar(ratio@major) == 0 || nchar(ratio@compound) == 0)
        stop("can only register ratios that have minor, major isotope and compound name set")
    
    refs <- getOption("isotope_standards") %||%
        data.frame(minor = character(), major = character(), name = character(), ratio = numeric(), stringsAsFactors = F)
    
    index <- which(refs$minor == ratio@isoname & refs$major == ratio@major & refs$name == ratio@compound)
    if (length(index) > 1)
        stop("more than one reference already exists with these characteristics, they must be unique!")
    else if (length(index) == 1) {
        index <- index
        if (get_value(ratio) != refs$ratio[index])
            warning("overwriting an existing standard with ratio: ", refs$ratio[index])
    } else
        index <- nrow(refs) + 1
    
    refs[index,] <- list(minor = ratio@isoname, major = ratio@major, name = ratio@compound, ratio = get_value(ratio))
    options(isotope_standards = refs)
    invisible(get_standards())
}



#' @details
#' Use \code{get_stanards} to retrieve any number of registered isotope standards
#' that can be identified with the provided search terms. For an overview
#' table of all standards (rather than the actual ratio objects), 
#' use \code{get_iso_opts("standards")} instead.
#' 
#' @param minor - character vector of minor isotope names to search for
#' @param major - character vector of major isotope names to search for
#' @param name - character vector of standards names to search for
#' @return list of ratio objects
#' @rdname standards
#' @export
get_standards <- function(minor = NULL, major = NULL, name = NULL) {
    if (is.null(refs <- getOption("isotope_standards")))
        return (list())
    
    index <- rep(TRUE, nrow(refs))
    if (!is.null(minor))
        index <- index & refs$minor %in% minor
    if (!is.null(major))
        index <- index & refs$major %in% major
    if (!is.null(name))
        index <- index & refs$name %in% name
    stds <- list()
    for (i in which(index)) {
        stds <- c(stds, list(update_iso(ratio(refs[i, "ratio"]), 
                                        list(isoname = refs[i, "minor"], major = refs[i, "major"], compound = refs[i, "name"]))))
    }
    stds
}

#' @details
#' \code{get_standard} is the same as \code{get_standards} except that it
#' returns a single object from the found standards and throws an error
#' if the search criteria did not yield exactly one.
#' @rdname standards
#' @export
get_standard <- function(minor = NULL, major = NULL, name = NULL) {
    stds <- get_standards(minor = minor, major = major, name = name)
    
    if (length(stds) == 0)
        stop("No reference ratio registered for these search parameters: ", paste(c(minor, major, name), collapse = ", "))
    else if (length(stds) > 1)
        stop("More than one reference ratio (", length(stds), ") found for these search parameters: ", paste(c(minor, major, name), collapse = ", "))
    
    return(stds[[1]])
}


#' @note
#' \code{use_permil} is a function to globally enable/disable the use of permil values
#' in conversions from/to delta and epsilon values. It can always be overwritten in
#' individual conversions using the \code{permil} parameter.
#' 
#' @rdname epsilon
#' @export
use_permil <- function(permil) {
    if (!missing(permil)) {
        options(use_permil = permil)
        return(invisible(permil))
    } else
        return(options("use_permil")[[1]])
}
