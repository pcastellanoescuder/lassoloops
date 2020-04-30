
#' Title
#'
#' @description
#'
#' @param object A LassoLoop object.
#'
#' @export
#'
#' @return
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
    model <- object@coefficients[[accuracy]]
    return(list(model = model, accuracy = Accuracy))

  }

  if(object@family == "cox"){

    cindex <- purrr::map(object@valiadationValues, 1) %>% which.max()
    Cindex <- object@valiadationValues[[cindex]]
    model <- object@coefficients[[cindex]]
    return(list(model = model, cindex = Cindex))

  }

  if(object@family %in% c("binomial", "poisson")){

    mse <- which.min(object@valiadationValues)
    MeanSquareError <- object@valiadationValues[[mse]]
    model <- object@coefficients[[mse]]
    return(list(model = model, mse = MeanSquareError))

  }

}

