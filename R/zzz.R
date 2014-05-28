# package on load function
setLoadActions(function(ns) {
    
    # default startup options (documented in options.R get_iso_opts !!)
    set_iso_opts(
        default_ab_notation = "raw", 
        default_ff_notation = "alpha", 
        default_delta_notation = "permil", 
        default_intensity_unit = "", 
        default_major = "", 
        default_minor = "",
        exact_mass_balance = FALSE
    )
  
    suppressWarnings({
        register_standard(ratio(`2H` = to_ratio(abundance(0.00015574)), major = "1H", compound = "VSMOW")) # from IUPAC report
        register_standard(ratio(`13C` = 0.011237, major = "12C", compound = "VPDB"))
        register_standard(ratio(`15N` = 0.003677, major = "14N", compound = "Air"))
        register_standard(ratio(`18O` = 0.0020052, major = "16O", compound = "VSMOW"))
        register_standard(ratio(`34S` = 0.0045005, major = "32S", compound = "CDT"))
    })
})
