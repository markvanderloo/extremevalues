
library(testthat)

context('QQ-plot positions')
test_that('plot postion results', {
   expect_equal( plotpositions(1), 0.5 )
   expect_equal( plotpositions(c(1,1), c(0.5,0.5)))
   expect_equal(
      round(plotpositions(c(1,2)),8), 
      round(c(1/3,2/3),8) 
   )
   expect_equal( 
      round(plotpositions(1,1,duplicates='asis'),8),
      round(c(1/3,2/3),8)
   )
})

test_that('plot position errors',{
   expect_error( plotposition(NA) )
   expect_error( plotposition(NaN) )
})

