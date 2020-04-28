
#' Title
#'
#' @description
#'
#' @param x
#' @param y
#' @param loops
#' @param nfolds
#' @param family
#' @param lambda
#' @param seed
#' @param ncores
#'
#' @export
#'
#' @return
#' @references
#' @author
#'
#' @importFrom tictoc tic toc
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom glmnet cv.glmnet
blasso <- function(x,
                   y,
                   loops = 2,
                   nfolds = 10,
                   family = "gaussian",
                   lambda = NULL,
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

    idx <- sample(1:n, replace = T)

    new_matrix <- cbind(y, x)
    new_matrix <- new_matrix[idx ,]

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

    cv_fit <- glmnet::cv.glmnet(as.matrix(train_x), train_y, family = family, nfolds = nfolds, lambda = lambda, parallel = TRUE)

    tmp_coeffs <- coef(cv_fit, s = "lambda.min")
    final_coef <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

    lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = as.matrix(test_x))
    mse <- mean((test_y - lasso_pred)^2)

    res[i] <- list(coeffs = final_coef, mse = mse)
  }

  tictoc::toc()

  return(res)
}

