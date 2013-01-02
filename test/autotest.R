library(testthat)



dyn.load("../pkg/src/plotpositions.so")
auto_test("../pkg/R", "../pkg/inst/tests")

