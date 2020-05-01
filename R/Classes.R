
setClass("LassoLoop",
         representation(
           model = "list",
           bootstraped = "logical",
           coefficients = "list",
           family = "character",
           valiadationMetric = "character",
           valiadationValues = "list",
           confusionMatrix = "list",
           length = "numeric",
           lassoloopsVersion = "character"),
         prototype(
           model = list(),
           bootstraped = logical(),
           coefficients = list(),
           family = character(),
           valiadationMetric = character(),
           valiadationValues = list(),
           confusionMatrix = list(),
           length = numeric(),
           lassoloopsVersion = character()))
