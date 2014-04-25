context("Data Processing")

test_that("Testing that reference standards can be properly registered and retrieved", {
    # register standards
    expect_error(register_standard(0.2), "can only register standards that are ratio isotope objects")
    expect_error(register_standard(ratio(1:2)), "must be a single ratio value")
    expect_error(register_standard(ratio(a = 0.1, major = "b")), "can only register ratios that have minor, major isotope and compound name set")
    expect_error(register_standard(ratio(0.1, major = "b", compound = "test")), "can only register ratios that have minor, major isotope and compound name set")
    expect_error(register_standard(ratio(a = 0.1, compound = "test")), "can only register ratios that have minor, major isotope and compound name set")
    register_standard(ratio(`2H` = 0.00015575, major = "1H", compound = "VSMOW"))
    expect_warning(register_standard(ratio(`2H` = 0.00015576, major = "1H", compound = "VSMOW")), "overwriting an existing standard")
    expect_warning(register_standard(ratio(`2H` = 0.00015575, major = "1H", compound = "VSMOW")), "overwriting an existing standard")
    expect_equal(get_standards(minor = "2H", major = "1H", name = "VSMOW")[[1]], ratio(`2H` = 0.00015575, major = "1H", compound = "VSMOW"))
    expect_message(as.ratio(delta(`2H` = -100, major = "1H", ref = "VSMOW")), "Successfully found a matching standard")
    expect_equal(as.ratio(delta(`2H` = 100, major = "1H", ref = "VSMOW")), ratio(`2H` = 1.1 * 0.00015575, major = "1H"))
})


test_that("Testing that default values can be set properly", {
    # test defaults
    expect_equal(default_minor_isotope(), "")
    expect_equal(default_major_isotope(), "")
    expect_equal(ratio(0.1)@isoname, "")
    expect_equal(ratio(0.2)@major, "")
    expect_equal(default_minor_isotope("13C"), "13C")
    expect_equal(default_major_isotope("12C"), "12C")
    expect_equal(ratio(0.1)@isoname, "13C")
    expect_equal(ratio(0.2)@major, "12C")
    expect_equal(ratio(`18O` = 0.1)@isoname, "18O")
    expect_equal(ratio(0.1, major = "16O")@major, "16O")
    expect_error(ratio(0.1, 0.2), "All isotopes in a system must be unique, found duplicates") # with the default value set, can't initialize a ratios system
    expect_is(rs <- ratio(0.1, `14C` = 0.2), "Ratios") # check specified ratio system
    expect_equal(rs$`13C`, ratio(0.1))
    expect_equal(rs$`14C`, ratio(`14C` = 0.2))
    
    on.exit({ # make sure they are reset
        default_minor_isotope("")
        default_major_isotope("")
    })
})