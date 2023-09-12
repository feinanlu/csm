# Test setup


#' @editor XXX
#' @editDate 2022-11-16
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: digit_pref
## data: vs_digit

test_that("3,1", {
  #TEST CODE HERE
  hello_result <- "Hello, Johnny"

  expect_equal(
    hello_result,
    "Hello, Johnny"
  )
})



#' @editor XXX
#' @editDate 2022-11-16
test_that("3.2",{

  hello_result <- "Hello, Johnny"

  expect_equal(
    hello_result,
    c("Hello, Johnny")
  )
})
