context("blasso")

test_that("blasso works", {

  data <- ggplot2::diamonds
  x <- as.matrix(data[,c(1, 5:6, 8:10)])
  y <- data$price

  res_blasso <- blasso(x, y)

  expect_true(class(res_blasso) == "LassoLoop")

  })
