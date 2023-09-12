# Test setup


#' @editor Lei Yang
#' @editDate 2023-07-10
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: outlier_check
## data: Eff_outlier_inlier

test_that("8,2", {
  #TEST CODE HERE: code review only, no seperate R code was generated.
  hello_result <- "Validation for mahal was passed via code review!"

  expect_equal(
    hello_result,
    "Validation for mahal was passed via code review!"
  )
})
