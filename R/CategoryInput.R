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
        numericValue = NULL,
        value = NULL,

        # Constructor
        initialize = function(
            categories
        ){
            self$categories = categories
            self$makeNumericCategories()
            self$makeBooleanCategories()
        },

        addValue = function(value) {
            self$value = self$checkBounds(value)
            self$numericValue = self$numericCategories[which(self$categories == value)]
        },

        checkBounds = function(value){
            if(value %in% self$categories){
                return(value)
            } else {
                stop(sout("Value", value, "is not in list of options", self$categories))
            }
        },

        makeNumericCategories = function(){
            self$numericCategories = seq(1, length(self$categories))
        },

        makeBooleanCategories = function(){
            self$numericCategories = c(1,0)
        }

    )
)

