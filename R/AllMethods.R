
##################################################################
## Methods for LassoLoopObject class
setMethod("initialize", "LassoLoop",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            .Object@lassoloopsVersion <-
              as.character(utils::packageVersion("lassoloops"))
            if (validObject(.Object))
              return(.Object)
          })

