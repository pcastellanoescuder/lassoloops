
#' Bootstrap Validation for Cox Lasso Regression
#'
#' @description This function performs n `glmnet::cv.glmnet(family = "cox")` models using bootstrap validation and splitting the input data in train and test at each loop.
#'
#' @param x x matrix as in glmnet.
#' @param y Should be a two-column matrix with columns named 'time' and 'status'. The latter is a binary variable, with '1' indicating death, and '0' indicating right censored.
#' @param loops Number of loops (a `glmnet::cv.glmnet` model will be performed in each loop).
#' @param bootstrap Logical indicating if bootstrap will be performed or not.
#' @param alpha The elasticnet mixing parameter, with 0 ≤ alpha ≤ 1. alpha = 1 is the lasso penalty, and alpha = 0 the ridge penalty.
#' @param nfolds number of folds - default is 10. Although nfolds can be as large as the sample size (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable is nfolds=3.
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
#' @importFrom survcomp concordance.index
#' @importFrom purrr map
cox_blasso <- function(x,
                       y,
                       loops = 2,
                       bootstrap = TRUE,
                       alpha = 1,
                       nfolds = 10,
                       seed = 987654321,
                       ncores = 2){

  tictoc::tic()

  doParallel::registerDoParallel(cores = ncores)

  set.seed(seed)
  varx <- colnames(x)
  rowx <- nrow(x)
  nvar <- ncol(x)
  n <- nrow(y)
  res <- vector("list", loops)

  if(ncol(y) != 2){
    stop("y must be a matrix with two columns (time and status)")
  }

  if(rowx != n){
    stop("The number of rows in x is not equal to the number of rows in y!")
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
    test_x <- test[,-c(1:2)]
    test_y <- test[,1:2]

    ## TRAIN

    train <- new_matrix[-idx_test ,]
    train_x <- train[,-c(1:2)]
    train_y <- train[,1:2]

    ## LASSO

    cv_fit <- glmnet::cv.glmnet(as.matrix(train_x), as.matrix(train_y), alpha = alpha, family = "cox", nfolds = nfolds, parallel = TRUE)

    tmp_coeffs <- coef(cv_fit, s = "lambda.min")
    final_coef <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

    lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = as.matrix(test_x), type = "response") # hazards

    cindex <- survcomp::concordance.index(lasso_pred, surv.time = test_y[,1], surv.event = test_y[,2], method = "noether")
    cindex <- list(c = cindex$c.index, se = cindex$se, lower = cindex$lower, upper = cindex$upper, pvalue = cindex$p.value)

    res[i] <- list(coeffs = final_coef, cindex = cindex, model = cv_fit)

  }

  res <- new("LassoLoop",
             model = purrr::map(res, 3),
             bootstraped = bootstrap,
             coefficients = purrr::map(res, 1),
             family = "cox",
             valiadationMetric = "Concordance Index",
             valiadationValues = purrr::map(res, 2),
             length = length(res))

  tictoc::toc()

  if(validObject(res))
    return(res)

}

