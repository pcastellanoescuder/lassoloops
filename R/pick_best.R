
#' Select the Best Model
#'
#' @description This function selects the best model among all loops according the best validation metric result.
#'
#' @param object A LassoLoop object.
#'
#' @export
#'
#' @return A list with the best model and its metrics.
#' @author Pol Castellano-Escuder
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map
pick_best <- function(object){

  if(!isTRUE(class(object) == "LassoLoop")){
    stop("Input should be a LassoLoop object")
  }

  ##

  if(object@family == "binomial"){

    accuracy <- purrr::map(object@valiadationValues, 1) %>% which.max()
    Accuracy <- object@valiadationValues[[accuracy]]
    coefficients <- object@coefficients[[accuracy]]
    model <- object@model[[accuracy]]
    return(list(model = model, coefficients = coefficients, accuracy = Accuracy))

  }

  if(object@family == "cox"){

    cindex <- purrr::map(object@valiadationValues, 1) %>% which.max()
    Cindex <- object@valiadationValues[[cindex]]
    coefficients <- object@coefficients[[cindex]]
    model <- object@model[[cindex]]
    return(list(model = model, coefficients = coefficients, cindex = Cindex))

  }

  if(object@family %in% c("binomial", "poisson")){

    mse <- which.min(object@valiadationValues)
    MeanSquareError <- object@valiadationValues[[mse]]
    coefficients <- object@coefficients[[mse]]
    model <- object@model[[mse]]
    return(list(model = model, coefficients = coefficients, mse = MeanSquareError))

  }

}

