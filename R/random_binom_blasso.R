
#' Bootstrap Validation for Binomial Random Lasso Regression
#'
#' @description This function performs n `glmnet::cv.glmnet(family = "binomial")` models using a random subset of variables at each loop. Bootstrap validation will be used at each loop.
#'
#' @param x x matrix as in glmnet.
#' @param y Should be either a numeric factor with two levels.
#' @param loops Number of loops (a `glmnet::cv.glmnet` model will be performed in each loop).
#' @param random_vars Number of variables randomly sampled as candidates at each loop. Default is `sqrt(p)` (where p is number of variables in x).
#' @param bootstrap Logical indicating if bootstrap will be performed or not.
#' @param smote Logical. If it's set to TRUE, the Synthetic Minority Over-sampling Technique will be used to reduce random oversampling. See `performanceEstimation::smote` function.
#' @param perc_over If smote parameter is TRUE. A number that drives the decision of how many extra cases from the minority class are generated (known as over-sampling).
#' @param perc_under If smote parameter is TRUE. A number that drives the decision of how many extra cases from the majority classes are selected for each case generated from the minority class (known as under-sampling).
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
#' @importFrom purrr map
#' @importFrom caret confusionMatrix
#' @importFrom performanceEstimation smote
random_binom_blasso <- function(x,
                                y,
                                loops = 2,
                                random_vars = floor(sqrt(ncol(x))),
                                bootstrap = TRUE,
                                smote = FALSE,
                                perc_over = 2,
                                perc_under = 2,
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
  n <- length(y)
  res <- vector("list", loops)

  if(rowx != n){
    stop("The number of rows in x is not equal to the length of y!")
  }

  ## SMOTE

  if(isTRUE(smote)){

    new_matrix <- cbind(y = as.character(as.factor(y)), x)
    new_matrix <- performanceEstimation::smote(y ~ ., as.data.frame(new_matrix), perc.over = perc_over, perc.under = perc_under)

    x <- new_matrix[,-1]
    y <- new_matrix[,1]
    n <- nrow(new_matrix)

  }

  ## LOOP

  res <- foreach::foreach(i = 1:loops) %dopar% {

    ## RANDOM VARIABLES

    idx_mtry <- sample(1:ncol(x), random_vars, replace = F)

    x <- x[, idx_mtry]

    ## BOOTSTRAP

    if(isTRUE(bootstrap)){

      idx <- sample(1:n, replace = T)

      new_matrix <- cbind(y, x)
      new_matrix <- new_matrix[idx ,]

    } else{

      new_matrix <- cbind(y, x)

    }

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

    cv_fit <- glmnet::cv.glmnet(data.matrix(train_x), as.matrix(train_y), alpha = alpha, family = "binomial", nfolds = nfolds)

    tmp_coeffs <- coef(cv_fit, s = "lambda.min")
    final_coef <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

    lasso_pred <- predict(cv_fit, s = cv_fit$lambda.min, newx = data.matrix(test_x), type = "class")

    cm <- caret::confusionMatrix(as.factor(lasso_pred), as.factor(test_y))
    overall <- cm$overall

    res[[i]] <- list(coeffs = final_coef, accuracy = overall, confusionMatrix = cm$table, model = cv_fit, random_vars = colnames(train_x))

  }

  res <- new("LassoLoop",
             model = purrr::map(res, 4),
             bootstraped = bootstrap,
             coefficients = purrr::map(res, 1),
             family = "binomial",
             valiadationMetric = "Accuracy",
             valiadationValues = purrr::map(res, 2),
             confusionMatrix = purrr::map(res, 3),
             randomVariables = purrr::map(res, 5),
             length = length(res))

  tictoc::toc()

  if(validObject(res))
    return(res)

}

