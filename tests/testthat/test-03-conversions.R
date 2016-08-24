context("Data Conversion")

test_that("Testing that isotope data type conversations behave correctly", {
    expect_error(abundance(ratio(.1)), "Cannot initialize an isotope value with another isotope value")
    
    #  initialization tests and keeping attributes
    expect_error(to_ratio("test"), "Don't know how to convert object of class .* to isotope ratio")
    expect_identical(to_ratio(ratio(.1)), ratio(.1))
    expect_identical(to_ratio(ratio(.1, .2)), ratio(.1, .2))
    expect_error(to_abundance("test"), "Don't know how to convert object of class .* to isotope abundance")
    expect_identical(to_abundance(abundance(.1)), abundance(.1))
    expect_identical(to_abundance(abundance(.1, .2)), abundance(.1, .2))
    expect_is(r <- to_ratio(a <- abundance(`13C` = .1, major = "12C", compound = "CO2")), "Ratio")
    expect_equal(r@isoname, a@isoname)
    expect_equal(r@major, a@major)
    expect_equal(r@compound, a@compound)
    expect_error(to_delta("test"), "Don't know how to convert object of class .* to delta value")
    
    # conversions to primtivie
    expect_identical(get_value(ratio(0.1*(1:5))), 0.1*(1:5))
    expect_equal(get_value(ratio(a = 1:5, b = 6:10)), data.frame(a = 1:5, b = 6:10))
    
    # conversation from abundance to ratio
    expect_equal(to_ratio(abundance(.4)), ratio(.4/.6)) # convertion of single abundance to ratio
    x <- c(0.0001, 0.001, 0.01, 0.1, 0.5) 
    expect_equal(to_ratio(abundance(x)), ratio(x / (1 - x))) # converting multiple values with the formula
    expect_equal(to_ratio(abundance(.1, .3)), ratio(.1/.6, .3/.6)) # convertion of abundance system to ratio system
    y <- sample(x)/5 
    expect_equal(to_ratio(abundance(x, y)), ratio(x / (1 - x - y), y / (1 - x - y))) # converting multiple values in a system
    
    # conversation from ratio to abundance
    expect_equal(to_abundance(ratio(.2)), abundance(.2/1.2)) # convertion of single ratio to abundance
    expect_equal(to_abundance(ratio(x)), abundance(x / (1 + x))) # converting multiple values with the formula
    expect_equal(to_abundance(ratio(.2, .3)), abundance(.2/1.5, .3/1.5)) # convertion of ratio system to abundance system
    expect_equal(to_abundance(ratio(x, y)), abundance(x / (1 + x + y), y / (1 + x + y))) # converting multiple values in a system
    
    # back and forth conversions
    ab <- abundance(x, y)
    expect_equal(to_abundance(to_ratio(ab)), ab)
    expect_true(all(abs(get_value(to_abundance(to_ratio(ab))) - get_value(ab)) < 10^(-15))) # test that machine error from back and forth conversion is smaller than 10^-15
    
    # conversion from intensity to ratio and abundace
    expect_error(to_ratio(intensity(100)), "Don't know how to convert object of class Intensity to isotope ratio")
    expect_error(to_ratio(intensity(100, 1000)), "none of the isotopes .* could be identified as the major ion")
    expect_error(intensity(intensity(`13C` = 100, major = "13C"), intensity(`12C` = 1000, major = "12C")), "major ion of all isotope value object in an isotope system must be the same")
    expect_is({ # single ratio conversion
        is <- intensity(`12C` = 1000, `13C` = 100, major = "12C", unit = "#", compound = "CO2")
        rs <- to_ratio(is)
    }, "Ratios")
    expect_equal(names(rs)[1], "13C")
    expect_equal(rs$`13C`@major, "12C")
    expect_equal(rs$`13C`@compound, "CO2")
    expect_equal(rs$`13C`, ratio(`13C` = 100/1000, major = "12C", compound = "CO2"))
    expect_is({ # multiple ratio conversions
        is <- intensity(`32S` = 9502, `33S` = 75, `34S` = 421, `36S` = 2, major = "32S", unit = "#")
        rs <- to_ratio(is)
    }, "Ratios")
    expect_equal(rs, ratio(`33S` = 75/9502, `34S` = 421/9502, `36S` = 2/9502, major = "32S")) # value check 
    expect_equal(to_ratio(intensity(x = x, y = y, major = "x")), ratio(y = y/x, major = "x", single_as_df = T)) # formula check
    expect_is(ab <- to_abundance(is), "Abundances")
    expect_equal(ab, abundance(`33S` = 0.0075, `34S` = 0.0421, `36S` = 0.0002, major = "32S")) # value check
    expect_equal(to_abundance(intensity(x = x, y = y, major = "x")), abundance(y = y/(y + x), major = "x", single_as_df = T)) # formula check

    
    # to_delta with ratio specified
    expect_error(to_delta(ff(c(0.9, 0.92, 0.93)), ref_ratio = c(0.1, 0.2)), "must have the same number of entries as the value")
    expect_error(to_delta(ff(`13C` = 0.9), ref_ratio = ratio(`12C` = 0.1)), "reference ratio .* cannot be for a different isotope")
    expect_error(to_delta(ff(0.9, major = "14N"), ref_ratio = ratio(0.1, major = "12C")), "reference ratio .* cannot have a different major isotope")
    expect_warning(set_attrib(delta(20, ref = "SMOW"), ref_ratio = ratio(0.1, compound = "air")), "changing the reference name")    
    expect_is(d <- set_attrib(delta(20), ref_ratio = ratio(0.1, compound = "SMOW")), "Delta")
    expect_equal(get_value(d, "raw"), 0.02)
    expect_equal(d@compound2, "SMOW")
    expect_equal(d@ref_ratio, 0.1)
    
    # to_delta with numeric value
    expect_equal( (d <- to_delta(ratio(`13C` = 0.1, major = "12C"), 0.1)), delta(`13C` = 0, major = "12C", ref_ratio = 0.1))
    expect_equal(d@isoname, "13C")
    expect_equal(d@major, "12C")
    
    # to_delta with ref standard
    expect_error (to_delta(ratio(`15N`=0.01), ref_ratio = get_standard("13C")), # mismatch
                  "cannot generate .* from two ratio objects that don't have matching attributes .*")
    expect_equal( { # take over standard if undefined
        d <- ratio(0.01) %>% to_delta(ref_ratio = get_standard("13C")); 
        c(d@isoname, d@major)},
        c("13C", "12C"))
    
    
    # alpha to delta FIXME
#     expect_equal(to_delta(alpha(0.99)), delta(-10))
#     expect_equal(to_delta(alpha(0.99), permil = F), delta(-0.01, permil = F))
#     expect_equal(to_delta(alpha(0.99), ref_ratio = 0.1), delta(-10, ref_ratio = 0.1))
#     expect_equal(to_delta(alpha(0.99), ref_ratio = ratio(0.1, compound = "air")), delta(-10, ref_ratio = 0.1, ref = "air"))
#     
    # ratio to delta FIXME (goes via alpha)
    expect_error(to_delta(ratio(c(0.18, 0.16)), ratio(c(0.2, 0.3))), "reference ratio for a delta value object must be exactly one numeric value")
#     expect_equal(to_delta(ratio(0.18), ratio(0.2)), delta(-100, ref_ratio = 0.2))
    x <- runif(20, min = 0.1, max = 0.2) # random ratios
#     expect_equal(to_delta(ratio(x), 0.2), delta((x/0.2 - 1) * 1000, ref_ratio = 0.2))
#     expect_equal(to_delta(ratio(0.18), 0.2, permil = F), delta(-0.1, ref_ratio = 0.2, permil = F))
#     expect_error(to_delta(ratio(`13C` = 0.18), ratio(`18O` = 0.2)), "cannot generate a fractionaton factor from two ratio objects that don't have matching attributes")
#     expect_error(to_delta(ratio(0.18, major = "12C"), ratio(0.2, major = "13C")), "annot generate a fractionaton factor from two ratio objects that don't have matching attributes")
#     expect_equal(get_label(d <- to_delta(ratio(`13C` = 0.18, major = "12C", compound = "CO2"), 
#                                      ratio(`13C`= 0.2, major = "12C", compound = "SMOW"))), paste0("CO2 ", get_iso_letter("delta"), "13C [", get_iso_letter("permil"), "] vs. SMOW"))
#     expect_equal(d@major, "12C")
    
    # systems
    expect_error(to_delta(ratio(0.1, 0.2), ratio(0.1, 0.2)), "the proper way .* not implemented yet")
    
    # abundance to delta
    expect_error(to_delta(abundance(0.2)), "not currently implemented")  # work here
    
    # delta back to ratio
    expect_error(to_ratio(delta(20)), "cannot convert from a ratio to a delta value without the reference ratio set")
    expect_equal(to_ratio(delta(-100, ref_ratio = 0.2)), ratio(0.18))
    x <- runif(20, min = -100, max = 100) # random delta values
    expect_equal(to_ratio(delta(x, ref_ratio = 0.2)), ratio((x/1000 + 1) * 0.2)) # test equation
    expect_equal(to_ratio(delta(`13C` = -100, major = "12C", compound = "CO2", ref_ratio = 0.2)), 
                 ratio(`13C` = 0.18, major = "12C", compound = "CO2")) # test parameters
})
 

test_that("Notation conversions are working currectly", {
    
    # general conversions
    expect_error(switch_notation(5), "don't know how to convert")
    expect_error(switch_notation(ratio(0.5)), "not a recognized notation")
    expect_error(switch_notation(ratio(0.5), 5), "not a recognized notation")
    expect_error(switch_notation(ratio(0.5), "bla"), "not a recognized notation")
    expect_error(switch_notation(ratio(0.5), "percent"), "not permitted to convert .* 'Ratio' to unit 'percent'")
    expect_equal(switch_notation(ratio(0.5), "raw"), ratio(0.5))
    expect_equal(get_value(ab <- switch_notation(abundance(0.1), "percent")), 10)
    expect_equal(get_value(switch_notation(ab, "raw")), 0.1)
    
    # ffactor conversions
    expect_equal(switch_notation(ff(0.99), "permil"), ff(-10, notation = "permil"))
    expect_is(es <- ff(a = ff(0.99), b = ff(20, notation = "permil")), "FractionationFactors")
    expect_equal(switch_notation(es$a, "permil"), ff(a = -10, notation = "permil"))
    expect_equal(es$b, ff(b = +20))
    
    # ff actors and delta values
    expect_equal(to_ff(delta(10)), ff(1.01))
    expect_equal(to_ff(delta(-0.02, notation = "raw")), ff(0.98))
    
    # eltas to fractionation factor (alpha) 
    expect_equal(to_ff(delta(200), delta(-200)), ff(1.2 / 0.8))
    expect_equal(get_value(to_delta(to_ff(delta(200), delta(-200))), "raw"), 1.2 / 0.8 - 1)   
    
    # delta conversions (permil conversion)
    expect_equal(get_iso_opts("default_delta_notation"), "permil") # check the default is set to use permil
    expect_is(dx <- to_delta(delta(0.02, notation="raw")), "Delta")
    expect_equal(get_label(switch_notation(dx, "permil")), paste0(get_iso_letter("delta"), " [", get_iso_letter("permil"), "]"))
    expect_is(d <- switch_notation(delta(20), "raw"), "Delta")
    expect_equal(get_value(d), 0.02)
    expect_equal(get_label(d), get_iso_letter("delta"))
    
    # arithmetic shorthands
    expect_equal( get_value(d <- delta(10) * 1000), 10000)
    expect_is(d@notation, "Notation_ppm")
    expect_equal( get_value(d <- d/1000), 10)
    expect_is(d@notation, "Notation_permil")
    expect_equal( get_value(d <- d/1000), 0.01)
    expect_is(d@notation, "Notation_raw")
    
    expect_equal( get_value(e <- ff(10, notation = "permil") * 1000), 10000)
    expect_is(e@notation, "Notation_ppm")
    expect_equal( get_value(e <- e/1000), 10)
    expect_is(e@notation, "Notation_permil")
    expect_equal( get_value(e <- e/1000), 0.01)
    expect_is(e@notation, "Notation_eps")
    
#    expect_is(dx <- switch_notation(delta(a = 0.02, b = 0.01, notation = "raw"), "permil"), "Deltas") # test isotope system FIXME
#     expect_equal(get_value(dx$a), 20)
#     expect_equal(get_value(dx$b), 10)
#     expect_equal(get_value((d <- switch_notation(dx, "raw"))$a), 0.02)
#     expect_equal(get_value(d$b), 0.01)
})

test_that("Testing that additional data in Isosys data frames doesn't get lost during conversions", {
    expect_is({
        is <- intensity(`32S` = 9502, `33S` = 75, `34S` = 421, `36S` = 2, major = "32S", unit = "#")
        is$extra <- 'test'
        is <- is[c(1,2,5,4,3)]
        rs <- to_ratio(is)
    }, "Ratios")
    expect_equal(names(rs), c("33S", "extra", "36S", "34S"))
    expect_equal(rs$extra, "test")
    expect_is({
        rs$time <- 0.234
        ab <- to_abundance(rs)
    }, "Abundances")
    expect_equal(ab$extra, "test")
    expect_equal(rs$time, 0.234)
})