context("Data Types")

test_that("Testing that basic single data types' (ratio, abundance, delta, etc.) validity controls are working", {
    
    # testing basic initialization
    expect_error(new("Ratios", data.frame()), "There are no isotope values in this isotope system")
    
    # testing Ratio data type
    expect_error(ratio(NA), "not a valid isotope data type")
    expect_error(ratio(-0.2), "cannot be negative")
    expect_is(ratio(c(0.1, 0.2, 0.3)), "Ratio")
    expect_error(ratio(`12C` = 0.1, major = "12C"), "isotope ratios cannot be defined for the same isotope as minor and major isotope")
    
    # testing weights
    expect_output(ratio(1), "An isotope value .*")
    expect_output(ratio(1, weight = 1), "An isotope value .*")
    expect_output(ratio(1, weight = 2), "A weighted isotope value .*")
    expect_error(ratio(1:5, weight = 2:3), "Not the same number of data values and weights") 
    expect_error(weight(ratio(1:5), 2:3), "Not the same number of data values and weights") 
    expect_equal(ratio(1:5, weight = 1:5)@weight, 1.0:5) # check weight
    expect_identical(weight(ratio(1:5), 0.2), ratio(1:5, weight = 0.2))  # set weight
    expect_equal(as.weight(ratio(1:5, weight = 0.2)), rep(0.2, 5)) # retrieve weight
    expect_equal(as.weight(ratio(1:5)), rep(1, 5)) # retrieve weight
    expect_equal(as.weighted_value(ratio(1:5, weight = 2)), 2*1:5) # retrieve weight

    # testing labels
    expect_equal(name(ratio(`12C` = 0.2, major = "13C")), "R 12C/13C")
    expect_equal(label(ratio(`13C` = 1:5, compound = "CO2", major = "12C")), "CO2 R 13C/12C")
    expect_equal(label(intensity(`13C` = 1:5, compound = "CO2", major = "12C", unit = "mV")), "CO2 13C [mV]")
    
    # testing Abundance data type
    expect_error(abundance(-0.2), "cannot be negative")
    expect_error(abundance(1.1), "cannot be larger than 1")
    expect_is(abundance(c(0.1, 0.2, 0.3)), "Abundance")
    expect_equal(name(abundance(`12C` = 0.1)), "F 12C")
    
    # testing alpha data type
    expect_error(alpha(-0.2), "cannot be negative")
    
    # testing attribute updates and reinitialization
    expect_error(ratio(abundance(0.1)), "Cannot initialize an isotope value with another isotope value")
    expect_is(ratio(ratio(0.1)), "Ratio")

    # update name, major and compound
    expect_equal({
        r <- ratio(0.1)
        r2 <- ratio(`13C` = r, major = "12C", compound = "CO2")
        r2@isoname
        }, "13C")
    expect_equal(r2@major, "12C")
    expect_equal(r2@compound, "CO2")
    
    # keep name, major and compound
    expect_equal({
        r <- ratio(`13C` = 0.1, major = "12C", compound = "CO2")
        r2 <- ratio(r)
        r2@isoname
        }, "13C")
    expect_equal(r2@major, "12C")
    expect_equal(r2@compound, "CO2")
    
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
    
    # overwrite compound (with warning)
    expect_warning({
        r <- ratio(`13C` = 0.1, major = "12C", compound = "CO2")
        r2 <- ratio(r, compound = "DIC")
    }, "changing the compound name")
    expect_equal(r2@compound, "DIC")
    
    # overwrite weight (wight warning)
    expect_warning({
        r <- ratio(`13C` = 0.1*(1:5), weight = 1:5)
        r2 <- weight(r, 3:7)
    }, "changing the weight .* differences: '1, 2, 6, 7'")
    expect_equal(as.weight(r2), 3:7)
    
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
    
    # testing weights in isotope system
    expect_error(ratio(ratio(1:5, weight = 1:5), ratio(1:5, weight = 6:10)), "the weights of all isotope value objects in an isotope system must be the same")
    expect_equal(as.weight(ratio(1, 2, weight = 3)$iso), 3)
    expect_is(rs <- ratio(a = 1:5, b = 6:10, weight = 3:7), "Ratios")
    expect_equal(as.weight(rs$a), 3:7) # single value
    expect_equal(as.weighted_value(rs$a), 1:5*3:7) # convert single value
    expect_equal(as.weight(rs$b), 3:7)
    expect_equal(as.weighted_value(rs$b), 6:10*3:7)
    expect_equal(as.weight(rs), data.frame(a=3:7, b=3:7)) # whole data frame
    expect_equal(as.weighted_value(rs), data.frame(a=1:5*3:7, b=6:10*3:7)) # whole data frame
    
    # specific test of different isotope systems
    expect_is(ratio(0.2, 0.5), "Ratios")
    expect_is(abundance(0.2, 0.5), "Abundances")
    expect_error(abundance(0.5, 0.50001), "the sum of fractional abundances for each data point in an isotope system cannot exceed 1")
    expect_is(intensity(100, 500), "Intensities")
    expect_error(intensity(100, 500, major = "12C"), "major ion .* must be part of the ion intensities isotopic system")
    expect_is(intensity(`12C` = 100, `13C` = 500, major = "12C"), "Intensities")
    
    # testing attribute updates and reinitialization
    expect_is(ratio(ratio(0.2, 0.5)), "Ratios")
    expect_warning({
        rs <- ratio(`33S` = 0.1, `34S` = 0.2, major = "32S")
        rs2 <- ratio(rs, major = "12C", compound = "SO4")
    }, "changing the major")
    expect_equal(rs2$`33S`@major, "12C")
    expect_equal(rs2$`34S`@major, "12C")
    expect_equal(rs2$`33S`@compound, "SO4")
    expect_equal(rs2$`34S`@compound, "SO4")
    
    # testing compound data frame isotope systems to throw the appropriate errors and warnings
    expect_is(rs <- ratio(`33S` = c(0.1, 0.2), `34S` = c(0.2, 0.3), major = "32S", compound = "H2S"), "Ratios")
    expect_error({
        rs2 <- rs
        rs2$c <- abundance(c(0.2, 0.3))
        validObject(rs2)
    }, "Not all isotopes in the system have the expected data type")
    expect_error({
        rs$`18O` <- ratio(c(0.2, 0.3), major = "16O")
        ratio(rs)
    }, "major ion of all isotope value object in an isotope system must be the same")
    expect_error({
        rs$`18O` <- ratio(c(0.2, 0.3), major = "32S", compound = "SO4")
        ratio(rs)
    }, "compound name of all isotope value objects in an isotope system must be the same")
    expect_error({
        is <- intensity(c(0.1, 0.2), unit = "mV", single_as_df = T)
        is$`ion.2` <- intensity(c(0.2, 0.3), unit = "V")
        intensity(is)
    }, "units in an isotopic system of ion intensities must all be the same")
    expect_is({ # should still be a ratio
        rs$test <- 'other column'
        rs
    }, "Ratios")
    expect_true(is.isosys(rs["33S", drop = F]))
    expect_false(is.isosys(rs["test", drop = F])) # reduced to data frame
    expect_is({ # adding column
        rs$`36S` <- ratio(c(0.2, 0.3))
        rs
    }, "Ratios")
    expect_is({ # overwriting column
        rs$`32S` <- ratio(c(0.2, 0.3))
        rs
    }, "Ratios")
})

test_that("Testing that object type check functions (is.x()) are working", {
    # is.ratio
    expect_true(is.ratio(ratio(0.1)))
    expect_true(is.ratio(ratio(0.1, 0.2)))
    expect_false(is.ratio(abundance(0.1)))
    
    # is.abundance
    expect_true(is.abundance(abundance(0.1)))
    expect_true(is.abundance(abundance(0.1, 0.2)))
    expect_false(is.abundance(ratio(0.1)))
    
    # is.intensity
    expect_true(is.intensity(intensity(100)))
    expect_true(is.intensity(intensity(100, 400)))
    expect_false(is.intensity(abundance(0.1)))

    # is.isosys vs is.isoval
    expect_true(is.isoval(ratio(0.1)))
    expect_false(is.isoval(ratio(0.1, 0.2)))
    expect_false(is.isosys(ratio(0.1)))
    expect_true(is.isosys(ratio(0.1, 0.2)))
    
    # is weighted
    expect_false(is.weighted(ratio(0.2)))
    expect_false(is.weighted(ratio(0.2, weight = 1)))
    expect_true(is.weighted(ratio(c(0.1, 0.2), weight = c(1,2))))
})