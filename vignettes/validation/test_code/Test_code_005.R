# Test setup


#' @editor XXX
#' @editDate 2022-11-16
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: integer_check
## data: Eff_integer

test_that("5,1", {
  #TEST CODE HERE
  hello_result <- "Hello, Johnny"

  expect_equal(
    hello_result,
    "Hello, Johnny"
  )
})



#' @editor XXX
#' @editDate 2022-11-16
test_that("5.2",{

  hello_result <- "Hello, Johnny"

  expect_equal(
    hello_result,
    c("Hello, Johnny")
  )
})
