context("Data Operations")

test_that("Testing proper response to math operators", {
    # operations that are not meaningful ===========
    expect_is(r <- ratio(0.1), "Ratio")
    expect_is(i <- intensity(100), "Intensity")
    expect_is(rs <- ratio(0.1, 0.2), "Ratios")
    expect_is(is <- intensity(100, 1000), "Intensities")
    expect_error(r + 5, "Addition .* not meaningful")
    expect_error(5 + r, "Addition .* not meaningful")
    expect_error(r + i, "Addition .* not meaningful")
    expect_error(r + is, "Addition .* not meaningful")
    expect_error(rs + is, "Addition .* not meaningful")
    expect_error(r - 5, "Subtraction .* not meaningful")
    expect_error(5 - r, "Subtraction .* not meaningful")
    expect_error(r - i, "Subtraction .* not meaningful")
    expect_error(r - is, "Subtraction .* not meaningful")
    expect_error(rs - is, "Subtraction .* not meaningful")
    expect_error(r * 5, "Multiplication .* not meaningful")
    expect_error(5 * r, "Multiplication .* not meaningful")
    expect_error(r * i, "Multiplication .* not meaningful")
    expect_error(r * is, "Multiplication .* not meaningful")
    expect_error(rs * is, "Multiplication .* not meaningful")
    expect_error(r / 5, "Division .* not meaningful")
    expect_error(5 / r, "Division .* not meaningful")
    expect_error(r / i, "Division .* not meaningful")
    expect_error(r / is, "Division .* not meaningful")
    expect_error(rs / is, "Division .* not meaningful")
    
    # operations that are permitted and implemented ===========
    # combining intensities with same definition
    expect_error(intensity(a = 100) + intensity(b = 1000), "trying to combine two intensity objects that don't have matching attributes")
    expect_error(intensity(1:5) + intensity(1000), "trying to combine two intensity objects that don't have matching lengths")
    expect_is(ci <- intensity(100) + intensity(1000), "Intensity")
    expect_is(ci <- intensity(a = 100, major = "b", unit = "mV") + intensity(a = 1000, major = "b", unit = "mV"), "Intensity")
    expect_equal(as.value(ci), 1100.)
    expect_equal(ci@isoname, "a")
    expect_equal(ci@major, "b")
    expect_equal(ci@unit, "mV")
    expect_is(ci <- intensity(1000) - intensity(500), "Intensity")
    expect_equal(as.value(ci), 500.)
    
    # converting intensity to ratio 
    expect_error(intensity(100, unit = "mV") / intensity(1000), "cannot generate an isotope ratio from two intensity objects with differing units")
    expect_error(intensity(100, unit = "mV") / intensity(1000, unit = "V"), "cannot generate an isotope ratio from two intensity objects with differing units")
    expect_error(intensity(1:5) / intensity(2:3), "longer object length is not a multiple of shorter object length")
    expect_is(r <- intensity(`13C` = 1:5) / intensity(`12C` = 5:9), "Ratio")
    expect_equal(as.numeric(r), 1:5/5:9)
    expect_equal(r@isoname, "13C")
    expect_equal(r@major, "12C")
    
    # mixing abundances
    expect_error(abundance(a = 0.2) + abundance(b = 0.3), "trying to mix two isotope objects that don't have matching attributes")
    expect_error(abundance(c(0.1, 0.2)) + abundance(0.3), "trying to mix two isotope objects that don't have matching lengths")
    expect_is(amix <- abundance(0.2) + abundance(0.4), "Abundance")
    expect_equal(as.value(amix), 0.3)
    expect_equal(as.weight(amix), 2)
    expect_equal(as.weighted_value(amix), 0.6)
    expect_equal(amix@compound, "?+?")
    expect_is({
        amix <- abundance(`13C` = 0.2, weight = 2, compound = "a") + 
            abundance(`13C` = 0.5, compound = "b") + 
            abundance(`13C` = 0.3, weight = 3, compound = "c")}, "Abundance")
    expect_equal(label(amix), "a+b+c F 13C") # compound name propagation
    expect_equal(as.value(amix), (0.2*2 + 0.5 + 0.3*3) / (2+1+3)) # formula test
    expect_equal(as.weight(amix), (2+1+3)) # formula test
    expect_equal(as.weighted_value(amix), (0.2*2 + 0.5 + 0.3*3)) # formula test
    
    # mixing ratios
    expect_error(ratio(a = 0.2) + ratio(b = 0.3), "trying to mix two isotope objects that don't have matching attributes")
    expect_error(ratio(c(0.1, 0.2)) + ratio(0.3), "trying to mix two isotope objects that don't have matching lengths")
    expect_is(rmix <- as.ratio(abundance(0.2)) + as.ratio(abundance(0.4)), "Ratio")
    expect_equal(as.value(rmix), as.value(as.ratio(abundance(0.3))))
    expect_equal(as.weight(rmix), 2)
    expect_error(ratio(0.2, 0.3) + ratio(0.3, 0.4), "this really needs to be implemented") # FIXME
    
    # mixing delta values
    expect_true("implement delta value mixing")
})