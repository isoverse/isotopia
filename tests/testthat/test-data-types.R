context("Data Types")

test_that("Testing that data types' (ratio, abundance, delta, etc.) validity controls are working", {
    
    # testing Ratio data type
    expect_error(ratio(NA), "not a valid isotope ratio")
    expect_error(ratio(-0.2), "cannot be negative")
    expect_is(ratio(c(0.1, 0.2, 0.3)), "Ratio")
    expect_error(ratio(0.1, "12C", "12C"), "isotope ratios cannot be defined for the same isotope as minor and major isotope")
    expect_equal(name(ratio(0.2, "12C", "13C")), "R 12C/13C")
    
    # testing Abundance data type
    expect_error(abundance(NA), "not a valid fractional abundance")
    expect_error(abundance(-0.2), "cannot be negative")
    expect_error(abundance(1.1), "cannot be larger than 1")
    expect_is(abundance(c(0.1, 0.2, 0.3)), "Abundance")
    expect_equal(name(abundance(0.1, "12C")), "F 12C")
})


test_that("Testing that isotope system validity controls are working", {
   
    # testing isotope system
    expect_error(isosys(0.1), "not a valid isotope value")
    expect_error(isosys(ratio(0.1), 0.1), "Not all data in the system are provided as isotope data types")
    expect_error(isosys(ratio(c(0.1, 0.2, 0.3)), abundance(c(0.1, 0.2, 0.3))), "Not all isotopes in the system have the same data type")
    expect_error(isosys(ratio(c(0.1, 0.2, 0.3)), ratio(0.2)), "Not the same number of measurements")
    expect_error(isosys(ratio(0.2, "32S"), ratio(0.1, "32S")), "All isotopes in a system must be unique")
    
    # specific test of ratios isotope system
    expect_error(isosys(ratio(0.1, isoname = "15N", major = "14N"), major = "13C"), "Not all values in the isotope system have the specified major ion")
    expect_error(isosys(ratio(0.1, isoname = "14N"), major = "14N"), "isotope ratios cannot be defined for the same isotope as minor and major isotope")
    expect_is(isosys( # proper system initialized
        ratio(0.1, "33S", "32S"), 
        ratio(0.15, "34S", "32S"), major = "32S"), "Ratios")
    expect_equal({ # propagation of major ion
        sys <- isosys(ratio(0.1, "33S"), ratio(0.15, "34S"), major = "32S")
        sys$`33S`@major
    }, "32S")
    
    # specific of ion intensity isotope system
    expect_error(isosys(intensity(1000, "12C")), "major ion must be specified in an isotopic system defined by ion intensities")
    expect_error(isosys(intensity(100, "13C"), major = "12C"), "major ion must be part of the ion intensities isotopic system")
    expect_error(isosys(intensity(1000, "12C", "V"), intensity(100, "13C", "mV"), major = "12C"), "units are specified .* must all be the same")
    expect_is(isosys(intensity(1000, "12C", "mV"), intensity(100, "13C", "mV"), major = "12C"), "Intensities")
    
})

test_that("Testing that object type check functions (is.x()) are working", {
    # is.ratio
    expect_true(is.ratio(ratio(0.1)))
    expect_true(is.ratio(isosys(ratio(0.1))))
    expect_false(is.ratio(abundance(0.1)))
    
    # is.abundance
    expect_true(is.abundance(abundance(0.1)))
    expect_true(is.abundance(isosys(abundance(0.1))))
    expect_false(is.abundance(ratio(0.1)))
    
    # is.intensity
    expect_true(is.intensity(intensity(100)))
    expect_true(is.intensity(isosys(intensity(100, "12C"), major = "12C")))
    expect_false(is.intensity(abundance(0.1)))
    
    # is.isosys
    expect_true(is.isosys(isosys(ratio(0.1))))
    expect_false(is.isosys(ratio(0.1)))
})