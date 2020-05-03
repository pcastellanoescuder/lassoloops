context("blasso")

test_that("blasso works", {

  data("mtcars")

  x <- mtcars[,1:6]
  y <- mtcars$qsec

  error_x <- x[1:5,]
  error_y <- mtcars[,6:7]

  ##

  blasso_mod <- blasso(x, y, loops = 10, bootstrap = TRUE, ncores = 1)
  blasso_mod_wo_bootstrap <- blasso(x, y, loops = 8, bootstrap = FALSE, ncores = 1)

  ##

  expect_true(class(blasso_mod) == "LassoLoop")
  expect_true(class(blasso_mod_wo_bootstrap) == "LassoLoop")

  expect_true(blasso_mod@bootstraped)
  expect_false(blasso_mod_wo_bootstrap@bootstraped)

  expect_true(blasso_mod@family == "gaussian")
  expect_true(blasso_mod_wo_bootstrap@family == "gaussian")

  expect_false(blasso_mod@family == "cox")
  expect_false(blasso_mod_wo_bootstrap@family == "cox")

  expect_true(blasso_mod@length == 10)
  expect_true(blasso_mod_wo_bootstrap@length == 8)

  ##

  expect_error(blasso(error_x, y, loops = 10, bootstrap = TRUE, ncores = 1))
  expect_error(blasso(x, error_y, loops = 10, bootstrap = TRUE, ncores = 1))

})

