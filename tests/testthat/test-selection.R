context("Data Selection")

test_that("Testing that isotope data types can be properly subselected", {
    # subselection of single values (Isoval)
    expect_is(r <- ratio((1:5)*0.1)[c(1,2,5)], "Ratio")
    expect_equal(as.numeric(r), c(0.1, 0.2, 0.5))
    
    # subselection of data.frames (Isosys)
    expect_is(rs <- ratio(`33S` = c(0.1, 0.2), `34S` = c(0.2, 0.3), major = "32S"), "Ratios")
    
    # column selections
    expect_is(rs_sub1 <- rs["33S"], "Ratio") # drop level
    expect_equal(as.numeric(rs_sub1), c(0.1, 0.2))
    expect_identical(rs["33S"], rs[1]) # name and index selection
    expect_identical(rs["33S"], rs$`33S`)
    expect_is(rs_sub2 <- rs["33S", drop = F], "Ratios") # keep as data frame
    expect_false(identical(rs_sub1, rs_sub2))
    expect_equal(names(rs_sub2), "33S")
    expect_equal(names(rs[2:1]), c("34S", "33S")) # revert column order
    
    # row selections
    expect_is(rs_sub3 <- rs[,], "Ratios") 
    expect_equal(nrow(rs_sub3 <- rs[2,]), 1) 
    expect_equal(rs_sub3$`34S`, ratio(`34S`=0.3, major = "32S")) 
    expect_equal(nrow(rs_sub4 <- rs[2:1,]), 2) 
    expect_equal(subset(rs, `33S` == 0.1), rs[1,]) # testing subset
    
    # row and column selection
    expect_equal(as.numeric(rs[2,"34S"]), 0.3) 
    expect_identical(rs[,"34S"], rs[1:2,"34S"]) 
    expect_identical(rs[2,"34S"], rs[2,2]) 
    expect_false(identical(rs[,"34S"], rs[,"34S", drop = FALSE])) # dropping by defaul
    expect_true(identical(rs[,c("33S", "34S")], rs[,c("33S", "34S"), drop = FALSE])) # doesn't matter if more than one column
    
    # adding/overwriting columns to the system 
    expect_is({ # should still be a ratio
        rs$test <- 'other column'
        rs
    }, "Ratios")
    expect_true(is.isosys(rs["33S", drop = F]))
    expect_false(is.isosys(rs["test", drop = F])) # reduced to data frame
    expect_is({
        rs$`36S` <- ratio(c(0.2, 0.3))
        rs
    }, "Ratios")
    expect_is({
        rs$`32S` <- ratio(c(0.2, 0.3))
        rs
    }, "Ratios")
    expect_error({
        rs$`36S` <- abundance(c(0.2, 0.3))
        validObject(rs)
    }, "Not all isotopes in the system have the same data type")
    
    # converting data types
    expect_false(is.iso(as.data.frame(rs)))
    expect_false(is.iso(data.frame(rs)))
})
