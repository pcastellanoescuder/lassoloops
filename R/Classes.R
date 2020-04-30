
setClass("LassoLoop",
         representation(
           bootstraped = "logical",
           coefficients = "list",
           family = "character",
           valiadationMetric = "character",
           valiadationValues = "list",
           confusionMatrix = "list",
           length = "numeric",
           lassoloopsVersion = "character"),
         prototype(
           bootstraped = logical(),
           coefficients = list(),
           family = character(),
           valiadationMetric = character(),
           valiadationValues = list(),
           confusionMatrix = list(),
           length = numeric(),
           lassoloopsVersion = character()))
