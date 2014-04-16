context("Data Types")

test_that("Testing that basic single data types' (ratio, abundance, delta, etc.) validity controls are working", {
    
    # testing basic initialization
    expect_error(iso("InvalidClass"), "not an Isoval class")
    
    # testing Ratio data type
    expect_error(ratio(NA), "not a valid isotope data type")
    expect_error(ratio(-0.2), "cannot be negative")
    expect_is(ratio(c(0.1, 0.2, 0.3)), "Ratio")
    expect_error(ratio(`12C` = 0.1, major = "12C"), "isotope ratios cannot be defined for the same isotope as minor and major isotope")
    expect_equal(name(ratio(`12C` = 0.2, major = "13C")), "R 12C/13C")
    
    # testing Abundance data type
    expect_error(abundance(-0.2), "cannot be negative")
    expect_error(abundance(1.1), "cannot be larger than 1")
    expect_is(abundance(c(0.1, 0.2, 0.3)), "Abundance")
    expect_equal(name(abundance(`12C` = 0.1)), "F 12C")
    
    # testing attribute updates and reinitialization
    expect_error(ratio(abundance(0.1)), "Cannot initialize an isotope value with another isotope value")
    expect_is(ratio(ratio(0.1)), "Ratio")
    
    # update name and major
    expect_equal({
        r <- ratio(0.1)
        r2 <- ratio(`13C` = r, major = "12C")
        r2@isoname
        }, "13C")
    expect_equal(r2@major, "12C")
    
    # keep name and major
    expect_equal({
        r <- ratio(`13C` = 0.1, major = "12C")
        r2 <- ratio(r)
        r2@isoname
        }, "13C")
    expect_equal(r2@major, "12C")
    
    # overwrite name (with warning)
    expect_warning({
        r <- ratio(`13C` = 0.1, major = "12C")
        r2 <- ratio(`14C` = r)
    }, "changing the name")
    expect_equal(r2@isoname, "14C")
    
    # overwrite major (with warning)
    expect_warning({
        r <- ratio(`13C` = 0.1, major = "12C")
        r2 <- ratio(r, major = "11C")
    }, "changing the major isotope")
    expect_equal(r2@major, "11C")
    
    # change unit on intensity (with warning)
    expect_warning({
        i <- intensity(`13C` = 0.1, major = "12C", unit = "mV")
        i2 <- intensity(i, unit = "V")
    }, "changing the unit")
    expect_equal(i2@unit, "V")
})


test_that("Testing that isotope systems' (ratios, abundances, etc.) validity controls are working", {
   
    # testing basic initialization
    expect_error(iso("Isoval", "InvalidClass"), "not an Isosys class")
    
    # testing isotope system
    expect_error(ratio(c(0.1, 0.2, 0.3), 0.2), "Not the same number of measurements")
    expect_error(ratio(`13C` = 0.2, `13C` = 0.1), "All isotopes in a system must be unique")
    expect_true(is.data.frame(ratio(0.1, 0.2)))
    expect_false(is.data.frame(ratio(0.1)))
    expect_true(is.data.frame(ratio(0.1, single_as_df = T)))
    
    # specific test of different isotope systems
    expect_is(ratio(0.2, 0.5), "Ratios")
    expect_is(abundance(0.2, 0.5), "Abundances")
    expect_is(intensity(100, 500), "Intensities")
    expect_error(intensity(100, 500, major = "12C"), "major ion .* must be part of the ion intensities isotopic system")
    expect_is(intensity(`12C` = 100, `13C` = 500, major = "12C"), "Intensities")
    
    # testing attribute updates and reinitialization
    expect_is(ratio(ratio(0.2, 0.5)), "Ratios")
    expect_warning({
        rs <- ratio(`33S` = 0.1, `34S` = 0.2, major = "32S")
        rs2 <- ratio(rs, major = "12C")
    }, "changing the major")
    expect_equal(rs2$`33S`@major, "12C")
    expect_equal(rs2$`34S`@major, "12C")
    
    # testing compound data frame isotope systems to throw the appropriate errors and warnings
    expect_error({
        rs <- ratio(`13C` = c(0.1, 0.2), major = "12C", single_as_df = T)
        rs$`18O` <- ratio(c(0.2, 0.3), major = "16O")
        ratio(rs)
    }, "major ion of all isotope value object in an isotope system must be the same")
    expect_error({
        is <- intensity(c(0.1, 0.2), unit = "mV", single_as_df = T)
        is$`ion.2` <- intensity(c(0.2, 0.3), unit = "V")
        intensity(is)
    }, "units in an isotopic system of ion intensities must all be the same")
    
})

test_that("Testing that object type check functions (is_x()) are working", {
    # is_ratio
    expect_true(is_ratio(ratio(0.1)))
    expect_true(is_ratio(ratio(0.1, 0.2)))
    expect_false(is_ratio(abundance(0.1)))
    
    # is_abundance
    expect_true(is_abundance(abundance(0.1)))
    expect_true(is_abundance(abundance(0.1, 0.2)))
    expect_false(is_abundance(ratio(0.1)))
    
    # is_intensity
    expect_true(is_intensity(intensity(100)))
    expect_true(is_intensity(intensity(100, 400)))
    expect_false(is_intensity(abundance(0.1)))

    # is_isosys vs is_isoval
    expect_true(is_isoval(ratio(0.1)))
    expect_false(is_isoval(ratio(0.1, 0.2)))
    expect_false(is_isosys(ratio(0.1)))
    expect_true(is_isosys(ratio(0.1, 0.2)))
})