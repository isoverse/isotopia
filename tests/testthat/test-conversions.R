context("Data Conversation")

test_that("Testing that isotope data type conversations behave correctly", {
  expect_error(abundance(ratio(.1)), "Cannot initialize an isotope value with another isotope value")
})
