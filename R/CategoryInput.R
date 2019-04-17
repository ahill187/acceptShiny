library(R6)
library(accept)
library(tibble)
source("./R/Util.R")

CategoryInput <- R6Class(
    "CategoryInput",
    public = list(

        # Fields
        categories = c(),
        numericCategories = c(),
        value = NULL,

        # Constructor
        initialize = function(
            categories
        ){
            self$categories = categories
            self$makeNumericCategories()
        },

        addValue = function(value) {
            self$value = self$checkBounds(value)
        },

        checkBounds = function(value){
            if(value %in% self$categories){
                return(value)
            } else {
                stop(paste0("Value is not in list of options",value, self$categories))
            }
        },

        makeNumericCategories = function(){
            self$numericCategories = seq(1, length(self$categories))
        }

    )
)

