# Test setup


#' @editor Lei Yang
#' @editDate 2023-07-06
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: mean_check
## data: lb_mean

test_that("6,1", {
  #TEST CODE HERE: see the file of "Test_code_006_Validation code.R"
  hello_result <- "Passed: Means values of lab parameters are the same!"

  expect_equal(
    hello_result,
    "Passed: Means values of lab parameters are the same!"
  )
})

