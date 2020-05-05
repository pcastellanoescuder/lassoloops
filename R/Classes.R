
setClass("LassoLoop",
         representation(
           model = "list",
           bootstraped = "logical",
           coefficients = "list",
           family = "character",
           valiadationMetric = "character",
           valiadationValues = "list",
           confusionMatrix = "list",
           randomVariables = "list",
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
           randomVariables = list(),
           length = numeric(),
           lassoloopsVersion = character()))
