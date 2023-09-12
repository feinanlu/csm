# Test setup


#' @editor Lei Yang
#' @editDate 2023-07-04
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: inlier_check
## data: Eff_outlier_inlier

test_that("4.1", {
  #TEST CODE HERE: see the file of "Test_code_004_Validation code.R"
  hello_result <- "Number of inliers found: 2"

  expect_equal(
    hello_result,
    "Number of inliers found: 3"
  )
})

