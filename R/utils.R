# Null default
# Analog of || from ruby
#
# @keyword internal
# @name nulldefault-infix
# @author Hadley Wickham
"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}

# convenience function for dropping a list
# down one dimension if it's only length 1
# (loosing the naming in the process!)
drop_list <- function(l) {
    if (length(l) == 1L) l[[1]]
    else l
}

#' Run a calculation quietly.
#' 
#' This small utility function is just a convenient wrapper for running
#' isotope calculations silently without outputting any of the warnings or
#' messages (it uses \link{suppressMessages} and \link{suppressWarnings}
#' internally) that might occur. Use with care to suppress warnings, you
#' might end up hiding important information.
#' @export
quietly <- function(expr) {
    suppressMessages(suppressWarnings(expr))
}

# this is not executed automatically in autotesting (since autotesting only 
# sources all code rather than loading a package)
# instead run these lines to starte the autotest
run_autotest <- function() {
    library(testthat)
    auto_test("R", "tests/testthat")
#    auto_test("/Users/sk/Dropbox/Tools/software/r/isotopia/R", "/Users/sk/Dropbox/Tools/software/r/isotopia/tests/testthat")
}


