
#' Bootstrap Validation for Quantitative Lasso Regression
#'
#' @description This function performs n `glmnet::cv.glmnet(family = c("gaussian", "poisson"))` models using bootstrap validation and splitting the input data in train and test at each loop.
#'
#' @param x x matrix as in glmnet.
#' @param y Response variable. Should be numeric a vector.
#' @param loops Number of loops (a `glmnet::cv.glmnet` model will be performed in each loop).
#' @param bootstrap Logical indicating if bootstrap will be performed or not.
#' @param alpha The elasticnet mixing parameter, with 0 ≤ alpha ≤ 1. alpha = 1 is the lasso penalty, and alpha = 0 the ridge penalty.
#' @param nfolds Number of folds - default is 10. Although nfolds can be as large as the sample size (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable is nfolds=3.
#' @param offset A vector of length `nobs` that is included in the linear predictor. See `?glmnet::glmnet()`
#' @param family Response type. Quantitative for family = "gaussian" or family = "poisson" (non-negative counts).
#' @param ntest Numeric indicating the percentage of observations that will be used as test set. Default is NULL (no test set).
#' @param seed `set.seed()` that will be used.
#' @param ncores Number of cores. Each loop will run in one core using the `foreach` package.
#'
#' @export
#'
#' @return A LassoLoop object with the results.
#' @references Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. URL http://www.jstatsoft.org/v33/i01/.
#' @author Pol Castellano-Escuder
#'
#' @importFrom tictoc tic toc
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom glmnet cv.glmnet
blasso <- function(x,
                   y,
                   loops = 2,
                   bootstrap = TRUE,
                   alpha = 1,
                   nfolds = 10,
                   offset = NULL,
                   family = "gaussian",
                   ntest = NULL,
                   seed = 987654321,
                   ncores = 2){

  tictoc::tic()

  doParallel::registerDoParallel(cores = ncores)

  set.seed(seed)
  varx <- colnames(x)
  rowx <- nrow(x)
  nvar <- ncol(x)
  n <- length(y)
  res <- vector("list", loops)

  if(rowx != n){
    stop("The number of rows in x is not equal to the length of y!")
  }

  res <- foreach::foreach(i = 1:loops) %dopar% {

    ## BOOTSTRAP
    if(bootstrap){
      idx <- sample(1:n, replace = T)

      new_matrix <- cbind(y, x)
      new_matrix <- new_matrix[idx ,]
    } else {
      new_matrix <- cbind(y, x)
    }

    if(!is.null(ntest)){
      ## TEST
      idx_test <- sample(1:n, 0.2*n, replace = FALSE)

      test <- new_matrix[idx_test ,]
      test_x <- test[,-1]
      test_y <- test[,1]

      ## TRAIN
      train <- new_matrix[-idx_test ,]
      train_x <- train[,-1]
      train_y <- train[,1]

      ## LASSO
      suppressWarnings(
        cv_fit <- glmnet::cv.glmnet(data.matrix(train_x), as.matrix(train_y), alpha = alpha,
                                    family = family, nfolds = nfolds,
                                    offset = offset)
      )

    } else {
      suppressWarnings(
        cv_fit <- glmnet::cv.glmnet(data.matrix(new_matrix[,-1]), as.matrix(new_matrix[,1]), alpha = alpha,
                                    family = family, nfolds = nfolds,
                                    offset = offset)
      )
    }

    tmp_coeffs <- coef(cv_fit, s = "lambda.min")
    final_coef <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

    if(!is.null(ntest)){
      if(!is.null(offset)) {
        lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = data.matrix(test_x), newoffset = offset)
      }
      else {
        lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = data.matrix(test_x))
      }
    }

    if(!is.null(ntest)){
      mse <- mean((test_y - lasso_pred)^2)
    } else {
      mse <- NULL
    }

    res[[i]] <- list(coeffs = final_coef, mse = mse, model = cv_fit)

  }

  res <- new("LassoLoop",
             model = purrr::map(res, 3),
             bootstraped = bootstrap,
             coefficients = purrr::map(res, 1),
             family = family,
             valiadationMetric = "Mean Square Error",
             valiadationValues = purrr::map(res, 2),
             length = length(res))

  tictoc::toc()

  if(validObject(res))
    return(res)

  return(res)
}

