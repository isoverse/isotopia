# package on load function
.onLoad <- function(lib, pkg){
    # set default use permil
    use_permil(TRUE)
    
    # set default exact calculations
    exact_mass_balance(FALSE)
    
    # register default minor/major isotope
    default_minor_isotope("")
    default_major_isotope("")
    
    #  register default standards
    suppressWarnings({
        register_standard(ratio(`2H` = 0.00015575, major = "1H", compound = "VSMOW"))
        register_standard(ratio(`13C` = 0.011237, major = "12C", compound = "VPDB"))
        register_standard(ratio(`15N` = 0.003677, major = "14N", compound = "Air"))
        register_standard(ratio(`18O` = 0.0020052, major = "16O", compound = "VSMOW"))
        register_standard(ratio(`34S` = 0.0045005, major = "32S", compound = "CDT"))
    })
}