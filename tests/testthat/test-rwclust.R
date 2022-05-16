test_that(
  "rwclust runs error-free with valid input", 
{
  
  df <- data.frame(
    from = c(1,2,3),
    to = c(2,3,1),
    weight = c(1,1,1)
  )

  output <- rwclust(df, iter=3, k=1)
  expect_type(output, "list")

})


test_that(
  "Test for edgecase where leaf nodes produce NAs",
  {

    df <- data.frame(
      from = c(1, 1, 2, 3),
      to = c(2, 3, 3, 4),
      weight = c(1, 2, 1, 1)
    )

    output <- rwclust(df, iter=3, k=1)
    expect_false(any(is.na(output$weights)))
  }
)
