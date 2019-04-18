
source("./R/CategoryInput.R")
source("./R/IntervalInput.R")
source("./R/BooleanInput.R")
makeInputs = function(dataTypes, lower, upper, columnNames, categories = NULL){

    inputList = list()
    for(index in 1:length(dataTypes)) {
        dataType = dataTypes[index]
        element = object(dataType)
        if(dataType == "BooleanInput") {
            element$new(BooleanInput$new(c(lower, upper)))
        } else if(dataType == "CategoryInput"){
            element$new(CategoryInput$new(categories[[index]]))
        } else {
            element$new(IntervalInput$new(lower[[index]], upper[[index]]))
        }
        inputList[[columnNames[index]]] = element
        remove(element)
    }

    return(inputList)

}

updateInputs = function(inputList, valueList) {

    for(index in 1:length(inputList)) {
        element = inputList[[index]]
        value = valueList[[index]]
        if("CategoryInput" %in% class(element)) {
            if("BooleanInput" %in% class(element)) {
                value = stringToNumeric(value)
                element$addValue(value)
            } else {
                element$addValue(value)
            }

        } else  {
            value = stringToNumeric(value)
            element$addValue(value)
        }
    }
    return(inputList)
}

stringToNumeric = function(string){
    if(string == "FALSE"){
        return(0)
    } else if(string=="TRUE") {
        return(1)
    } else {
        return(as.numeric(string))
    }
}

combineShinyInputs = function(input, ids) {

    valueList = c()
    for(index in 1:length(ids)) {
        valueList = c(valueList, input[[ids[index]]])
    }
    return(valueList)
}

