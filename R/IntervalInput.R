library(R6)
library(accept)
library(tibble)
source("./R/Util.R")

IntervalInput <- R6Class(
    "IntervalInput",
    public = list(

        # Fields
        lower = NULL,
        upper = NULL,
        value = NULL,

        # Constructor
        initialize = function(
            lower = 0, upper = 1000000
        ){
            self$lower = lower
            self$upper = upper
        },

        addValue = function(value) {
            self$value = self$checkBounds(value)
        },

        checkBounds = function(value){
            if(value <= self$upper && value >= self$lower){
                return(value)
            } else {
                stop(paste0("Value is out of bounds", value, self$lower, self$upper))
            }
        }

    )
)

