test_that("Test matrix_power()", {
  
  M <- Matrix(2, 2, 2)

  results_single <- matrix_power(M, 2, accumulate = FALSE)
  results_multiple <- matrix_power(M, 2, accumulate = TRUE)

  expect_equal(sum(Matrix(8, 2, 2) == results_single), 4)
  expect_equal(sum(Matrix(2, 2, 2) == results_multiple[[1]]), 4)
  expect_equal(sum(Matrix(8, 2, 2) == results_multiple[[2]]), 4)

})

test_that("Test matrix_summation()", {
  
  M <- Matrix(1, 2, 2)
  results <- matrix_summation(list(M, M))

  expect_equal(sum(Matrix(2,2,2) == results), 4)
})
