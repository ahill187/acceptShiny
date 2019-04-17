
source("./R/CategoryInput.R")
source("./R/IntervalInput.R")
makeInputs = function(dataTypes, lower, upper, columnNames){

    inputList = list()
    for(index in 1:length(dataTypes)) {
        dataType = dataTypes[index]
        element = object(dataType)
        if(dataType == "CategoryInput") {
            element$new(CategoryInput$new(c(lower, upper)))
        } else {
            element$new(IntervalInput$new(lower[index], upper[index]))
        }
        inputList[[columnNames[index]]] = element
        remove(element)
    }

    return(inputList)

}

updateInputs = function(inputList, valueList) {

    for(index in 1:length(inputList)) {
        element = inputList[[index]]
        value = as.numeric(valueList[[index]])
        if("CategoryInput" %in% class(element)) {
            element$addValue(value)
        } else {
            element$addValue(value)
        }
    }
    return(inputList)
}

combineShinyInputs = function(input, ids) {

    valueList = c()
    for(index in 1:length(ids)) {
        valueList = c(valueList, input[[ids[index]]])
    }
    return(valueList)
}

