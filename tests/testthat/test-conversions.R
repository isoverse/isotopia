context("Data Conversation")

test_that("Testing that isotope data type conversations behave correctly", {
  expect_error(abundance(ratio(.1)), "Cannot initialize an isotope value with another isotope value")
  
  # conversions to ratio
  expect_error(as.ratio("test"), "Don't know how to convert object of class .* to isotope ratio")
  expect_is(as.ratio(ratio(.1)), "Ratio")
  expect_is(as.ratio(ratio(.1, .2)), "Ratios")
  expect_is(r <- as.ratio(a <- abundance(`13C` = .1, major = "12C")), "Ratio")
  expect_equal(r@isoname, a@isoname)
  expect_equal(r@major, a@major)
  
  # conversation to ratio - actual calculations
  expect_equal(as.ratio(abundance(.1)), ratio(.1 / (1 + .1))) # convertion of single abundance to ratio
  
  
  # back and forth conversions
  expect_equal({
      ab <- abundance((1:4)*0.1, (1:4)*0.2)
      as.abundance(as.ratio(ab))
  }, ab)
  
  
  # FIXME - continue here - why does this fail?
  expect_identical({
      ab <- abundance((1:4)*0.1, (1:4)*0.2)
      as.abundance(as.ratio(ab))
  }, ab)
})
