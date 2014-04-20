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
    expect_equal(as.ratio(delta(`2H` = 100, major = "1H", ref = "VSMOW")), ratio(`2H` = 1.1 * 0.00015575, major = "1H"))
})