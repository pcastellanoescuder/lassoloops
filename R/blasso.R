
#' Bootstrap Validation for Quantitative Lasso Regression
#'
#' @description
#'
#' @param x x matrix as in glmnet.
#' @param y Response variable. Should be numeric a vector.
#' @param loops Number of loops (a `glmnet::cv.glmnet` model will be performed in each loop).
#' @param bootstrap Logical indicating if bootstrap will be performed or not.
#' @param alpha The elasticnet mixing parameter, with 0 ≤ alpha ≤ 1. alpha = 1 is the lasso penalty, and alpha = 0 the ridge penalty.
#' @param nfolds number of folds - default is 10. Although nfolds can be as large as the sample size (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable is nfolds=3.
#' @param family Response type. Quantitative for family = "gaussian" or family = "poisson" (non-negative counts).
#' @param seed `set.seed()` that will be used.
#' @param ncores Number of cores. Each loop will run in one core using the `foreach` package.
#'
#' @export
#'
#' @return A list with the results.
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
                   family = "gaussian",
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

    if(isTRUE(bootstrap)){

      idx <- sample(1:n, replace = T)

      new_matrix <- cbind(y, x)
      new_matrix <- new_matrix[idx ,]

    } else{

      new_matrix <- cbind(y, x)

    }

    ## TEST

    idx_test <- sample(1:(n/3), replace = FALSE)

    test <- new_matrix[idx_test ,]
    test_x <- test[,-1]
    test_y <- test[,1]

    ## TRAIN

    train <- new_matrix[-idx_test ,]
    train_x <- train[,-1]
    train_y <- train[,1]

    ## LASSO

    cv_fit <- glmnet::cv.glmnet(as.matrix(train_x), train_y, alpha = alpha, family = family, nfolds = nfolds, parallel = TRUE)

    tmp_coeffs <- coef(cv_fit, s = "lambda.min")
    final_coef <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

    lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = as.matrix(test_x))
    mse <- mean((test_y - lasso_pred)^2)
    res[i] <- list(coeffs = final_coef, mse = mse)

  }

  res <- new("LassoLoop",
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

