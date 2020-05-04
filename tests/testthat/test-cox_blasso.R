context("cox_blasso")

test_that("cox_blasso works", {

  Sys.unsetenv("R_TESTS")

  data("survival_cancer")

  x <- survival_cancer[,4:10]
  x <- data.frame(apply(x, 2, function(y)ifelse(is.na(y), median(y, na.rm = T), y)))
  y <- survival_cancer[,2:3] %>% mutate(status = ifelse(status == 2,1,0))

  error_x <- x[1:10,]
  error_y <- y[,1]

  ##

  cox_mod <- cox_blasso(x, y, loops = 10, bootstrap = TRUE, ncores = 1)
  cox_mod_wo_bootstrap <- cox_blasso(x, y, loops = 8, bootstrap = FALSE, ncores = 1)

  ##

  expect_true(class(cox_mod) == "LassoLoop")
  expect_true(class(cox_mod_wo_bootstrap) == "LassoLoop")

  expect_true(cox_mod@bootstraped)
  expect_false(cox_mod_wo_bootstrap@bootstraped)

  expect_true(cox_mod@family == "cox")
  expect_true(cox_mod_wo_bootstrap@family == "cox")

  expect_false(cox_mod@family == "gaussian")
  expect_false(cox_mod_wo_bootstrap@family == "gaussian")

  expect_true(cox_mod@length == 10)
  expect_true(cox_mod_wo_bootstrap@length == 8)

  ##

  expect_error(cox_blasso(error_x, y, loops = 10, bootstrap = TRUE, ncores = 1))
  expect_error(cox_blasso(x, error_y, loops = 10, bootstrap = TRUE, ncores = 1))

})

