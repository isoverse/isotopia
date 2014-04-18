context("Data Conversion")

test_that("Testing that isotope data type conversations behave correctly", {
    expect_error(abundance(ratio(.1)), "Cannot initialize an isotope value with another isotope value")
    
    #  initialization tests and keeping attributes
    expect_error(as.ratio("test"), "Don't know how to convert object of class .* to isotope ratio")
    expect_identical(as.ratio(ratio(.1)), ratio(.1))
    expect_identical(as.ratio(ratio(.1, .2)), ratio(.1, .2))
    expect_error(as.abundance("test"), "Don't know how to convert object of class .* to isotope abundance")
    expect_identical(as.abundance(abundance(.1)), abundance(.1))
    expect_identical(as.abundance(abundance(.1, .2)), abundance(.1, .2))
    expect_is(r <- as.ratio(a <- abundance(`13C` = .1, major = "12C", compound = "CO2")), "Ratio")
    expect_equal(r@isoname, a@isoname)
    expect_equal(r@major, a@major)
    expect_equal(r@compound, a@compound)
    expect_error(as.delta("test"), "Don't know how to convert object of class .* to delta value")
    
    # conversions to primtivie
    expect_identical(as.primitive(ratio(0.1*(1:5))), 0.1*(1:5))
    expect_equal(as.primitive(ratio(a = 1:5, b = 6:10)), data.frame(a = 1:5, b = 6:10))
    
    # conversation from abundance to ratio
    expect_equal(as.ratio(abundance(.4)), ratio(.4/.6)) # convertion of single abundance to ratio
    x <- c(0.0001, 0.001, 0.01, 0.1, 0.5) 
    expect_equal(as.ratio(abundance(x)), ratio(x / (1 - x))) # converting multiple values with the formula
    expect_equal(as.ratio(abundance(.1, .3)), ratio(.1/.6, .3/.6)) # convertion of abundance system to ratio system
    y <- sample(x)/5 
    expect_equal(as.ratio(abundance(x, y)), ratio(x / (1 - x - y), y / (1 - x - y))) # converting multiple values in a system
    
    # conversation from ratio to abundance
    expect_equal(as.abundance(ratio(.2)), abundance(.2/1.2)) # convertion of single ratio to abundance
    expect_equal(as.abundance(ratio(x)), abundance(x / (1 + x))) # converting multiple values with the formula
    expect_equal(as.abundance(ratio(.2, .3)), abundance(.2/1.5, .3/1.5)) # convertion of ratio system to abundance system
    expect_equal(as.abundance(ratio(x, y)), abundance(x / (1 + x + y), y / (1 + x + y))) # converting multiple values in a system
    
    # back and forth conversions
    ab <- abundance(x, y)
    expect_equal(as.abundance(as.ratio(ab)), ab)
    expect_true(all(abs(as.primitive(as.abundance(as.ratio(ab))) - as.primitive(ab)) < 10^(-15))) # test that machine error from back and forth conversion is smaller than 10^-15
    
    # conversion from intensity to ratio and abundace
    expect_error(as.ratio(intensity(100)), "Don't know how to convert object of class Intensity to isotope ratio")
    expect_error(as.ratio(intensity(100, 1000)), "none of the isotopes .* could be identified as the major ion")
    expect_error(intensity(intensity(`13C` = 100, major = "13C"), intensity(`12C` = 1000, major = "12C")), "major ion of all isotope value object in an isotope system must be the same")
    expect_is({ # single ratio conversion
        is <- intensity(`12C` = 1000, `13C` = 100, major = "12C", unit = "#", compound = "CO2")
        rs <- as.ratio(is)
    }, "Ratios")
    expect_equal(names(rs)[1], "13C")
    expect_equal(rs$`13C`@major, "12C")
    expect_equal(rs$`13C`@compound, "CO2")
    expect_equal(rs$`13C`, ratio(`13C` = 100/1000, major = "12C", compound = "CO2"))
    expect_is({ # multiple ratio conversions
        is <- intensity(`32S` = 9502, `33S` = 75, `34S` = 421, `36S` = 2, major = "32S", unit = "#")
        rs <- as.ratio(is)
    }, "Ratios")
    expect_equal(rs, ratio(`33S` = 75/9502, `34S` = 421/9502, `36S` = 2/9502, major = "32S")) # value check 
    expect_equal(as.ratio(intensity(x = x, y = y, major = "x")), ratio(y = y/x, major = "x", single_as_df = T)) # formula check
    expect_is(ab <- as.abundance(is), "Abundances")
    expect_equal(ab, abundance(`33S` = 0.0075, `34S` = 0.0421, `36S` = 0.0002, major = "32S")) # value check
    expect_equal(as.abundance(intensity(x = x, y = y, major = "x")), abundance(y = y/(y + x), major = "x", single_as_df = T)) # formula check

})
 

test_that("Testing that additional data in Isosys data frames doesn't get lost during conversions", {
    expect_is({
        is <- intensity(`32S` = 9502, `33S` = 75, `34S` = 421, `36S` = 2, major = "32S", unit = "#")
        is$extra <- 'test'
        is <- is[c(1,2,5,4,3)]
        rs <- as.ratio(is)
    }, "Ratios")
    expect_equal(names(rs), c("33S", "extra", "36S", "34S"))
    expect_equal(rs$extra, "test")
    expect_is({
        rs$time <- 0.234
        ab <- as.abundance(rs)
    }, "Abundances")
    expect_equal(ab$extra, "test")
    expect_equal(rs$time, 0.234)
})