# Test setup


#' @editor XXX
#' @editDate 2022-11-16
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: cat_check
## data: DM_cat


test_that("1,1", {
  #TEST CODE HERE
  hello_result_tester <- "Hello, Johnny"

  expect_equal(
    hello_result_tester,
    "Hello, Johnny"
  )
})
##validation log is saved at: XXX/XXX/XX


#' @editor XXX
#' @editDate 2022-11-16
#'
test_that("1.2",{

  hello_result <- "Hello, Johnny"

  expect_equal(
    hello_result,
    c("Hello, Johnny")
  )
})
