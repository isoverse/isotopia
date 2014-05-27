context("Miscellaneous tests")

test_that("Testing utility functions", {
    expect_equal('value' %||% 'test', 'value')
    expect_equal(NULL %||% 'test', 'test')
})