context("binom_blasso")

test_that("binom_blasso works", {

  x <- mtcars[,1:7]
  y <- mtcars$vs

  error_x <- x[1:5,]
  error_y <- mtcars[,8:9]

  ##

  binom_blasso_mod <- binom_blasso(x, y, loops = 10, bootstrap = TRUE, ncores = 1)
  binom_blasso_mod_wo_bootstrap <- binom_blasso(x, y, loops = 8, bootstrap = FALSE, ncores = 1)

  s_binom_blasso_mod <- binom_blasso(x, y, loops = 10, bootstrap = TRUE, ncores = 1, smote = TRUE)
  s_binom_blasso_mod_wo_bootstrap <- binom_blasso(x, y, loops = 8, bootstrap = FALSE, ncores = 1, smote = TRUE)

  ##

  expect_true(class(binom_blasso_mod) == "LassoLoop")
  expect_true(class(binom_blasso_mod_wo_bootstrap) == "LassoLoop")
  expect_true(class(s_binom_blasso_mod) == "LassoLoop")
  expect_true(class(s_binom_blasso_mod_wo_bootstrap) == "LassoLoop")

  expect_true(binom_blasso_mod@bootstraped)
  expect_false(binom_blasso_mod_wo_bootstrap@bootstraped)
  expect_true(s_binom_blasso_mod@bootstraped)
  expect_false(s_binom_blasso_mod_wo_bootstrap@bootstraped)

  expect_true(binom_blasso_mod@family == "binomial")
  expect_true(binom_blasso_mod_wo_bootstrap@family == "binomial")
  expect_true(s_binom_blasso_mod@family == "binomial")
  expect_true(s_binom_blasso_mod_wo_bootstrap@family == "binomial")

  expect_false(binom_blasso_mod@family == "cox")
  expect_false(binom_blasso_mod_wo_bootstrap@family == "cox")
  expect_false(s_binom_blasso_mod@family == "cox")
  expect_false(s_binom_blasso_mod_wo_bootstrap@family == "cox")

  expect_true(binom_blasso_mod@length == 10)
  expect_true(binom_blasso_mod_wo_bootstrap@length == 8)
  expect_true(s_binom_blasso_mod@length == 10)
  expect_true(s_binom_blasso_mod_wo_bootstrap@length == 8)

  ##

  expect_error(binom_blasso(error_x, y, loops = 10, bootstrap = TRUE, ncores = 1))
  expect_error(binom_blasso(x, error_y, loops = 10, bootstrap = TRUE, ncores = 1))
  expect_error(binom_blasso(error_x, y, loops = 10, bootstrap = TRUE, ncores = 1, smote = TRUE))
  expect_error(binom_blasso(x, error_y, loops = 10, bootstrap = TRUE, ncores = 1, smote = TRUE))

})

