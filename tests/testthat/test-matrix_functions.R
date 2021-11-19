test_that("Test matrix_power()", {
  
  M <- Matrix::Matrix(2, 2, 2)

  results_single <- matrix_power(M, 2, accumulate = FALSE)
  results_multiple <- matrix_power(M, 2, accumulate = TRUE)

  expect_equal(sum(Matrix::Matrix(8, 2, 2) == results_single), 4)
  expect_equal(sum(Matrix::Matrix(2, 2, 2) == results_multiple[[1]]), 4)
  expect_equal(sum(Matrix::Matrix(8, 2, 2) == results_multiple[[2]]), 4)

})

test_that("Test matrix_summation()", {

  M <- Matrix::Matrix(1, 2, 2)
  results <- matrix_summation(list(M, M))

  expect_equal(sum(Matrix::Matrix(2,2,2) == results), 4)
})


test_that("Test compute_transition_matrix", {

  m <- Matrix::Matrix(
    c(
      c(1, 1, 0),
      c(0, 1, 0),
      c(2, 3, 0)
    ), ncol = 3, byrow = TRUE
  )

  expected <- Matrix::Matrix(
    c(
      c(1/2, 1/2, 0),
      c(0, 1, 0),
      c(2/5, 3/5, 0)
    ), ncol = 3, byrow = TRUE
  )

  expect_equal(sum(compute_transition_matrix(m) == expected), 9)

})