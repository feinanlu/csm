# Test setup


#' @editor Lei Yang
#' @editDate 2023-07-07
#'

## test_that is a function returns TRUE/FALSE after comparing the test results
## function: outlier_aedata
## data: AE_outlier0 AE_outlier1

test_that("7,1", {
  #TEST CODE HERE: see the file of "Test_code_007_Validation code.R"
  hello_result <- "Validation for SD was passed!"

  expect_equal(
    hello_result,
    "Validation for SD was passed!"
  )
})


#' @editor Lei Yang
#' @editDate 2023-07-07
test_that("7,2", {
  #TEST CODE HERE: see the file of "Test_code_007_Validation code.R"
  hello_result <- "Validation for mahal was not done!"

  expect_equal(
    hello_result,
    "Validation for mahal was not done!"
  )
})


#' @editor Lei Yang
#' @editDate 2023-07-07
test_that("7,3", {
  #TEST CODE HERE: see the file of "Test_code_007_Validation code.R"
  hello_result <- "Validation for IQR was not passed!"

  expect_equal(
    hello_result,
    "Validation for IQR was passed!"
  )
})


#' @editor Lei Yang
#' @editDate 2023-07-07
test_that("7,4", {
  #TEST CODE HERE: see the file of "Test_code_007_Validation code.R"
  hello_result <- "Validation for PCT was passed!"

  expect_equal(
    hello_result,
    "Validation for PCT was passed!"
  )
})
