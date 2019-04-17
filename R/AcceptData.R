library(R6)
library(accept)
library(tibble)
source("./R/Util.R")

AcceptData <- R6Class(
    "RawData",
    public = list(

        # Fields
        id = 1,
        male = object("CategoryInput"),
        age = object("IntervalInput"),
        smoker = object("CategoryInput"),
        oxygen = object("CategoryInput"),
        statin = object("CategoryInput"),
        LAMA = object("CategoryInput"),
        LABA = object("CategoryInput"),
        ICS = object("CategoryInput"),
        azithromycin = object("CategoryInput"),
        FEV1 = object("IntervalInput"),
        BMI = object("IntervalInput"),
        SGRQ = object("IntervalInput"),
        LastYrExacCount = object("IntervalInput"),
        LastYrSevExacCount = object("IntervalInput"),
        prediction = NULL,
        numPatients = 1,

        # Constructor
        initialize = function(
            male,
            age,
            smoker,
            oxygen,
            statin,
            LAMA,
            LABA,
            ICS,
            azithromycin,
            FEV1,
            BMI,
            SGRQ,
            LastYrExacCount,
            LastYrSevExacCount
        ){
            self$male = self$checkType(male, self$male)
            self$age = self$checkType(age, self$age)
            self$smoker = self$checkType(smoker, self$smoker)
            self$oxygen = self$checkType(oxygen, self$oxygen)
            self$statin = self$checkType(statin, self$statin)
            self$LAMA = self$checkType(LAMA, self$LAMA)
            self$LABA = self$checkType(LABA, self$LABA)
            self$ICS = self$checkType(ICS, self$ICS)
            self$azithromycin = self$checkType(azithromycin, self$azithromycin)
            self$FEV1 = self$checkType(FEV1, self$FEV1)
            self$BMI = self$checkType(BMI, self$BMI)
            self$SGRQ = self$checkType(SGRQ, self$SGRQ)
            self$LastYrExacCount = self$checkType(LastYrExacCount, self$LastYrExacCount)
            self$LastYrSevExacCount = self$checkType(LastYrSevExacCount, self$LastYrSevExacCount)
        },

        checkType = function(anObject, field){
            requiredType = field$type
            newType = class(anObject)
            if(requiredType %in% newType){
                return(anObject)
            } else {
                stop(paste0("Type for field must be ", requiredType))
            }
        },

        calculatePrediction = function(){
            patient = tibble(ID = self$id,
                             male = self$male$value,
                             age = self$age$value,
                             smoker = self$smoker$value,
                             oxygen = self$oxygen$value,
                             statin = self$statin$value,
                             LAMA = self$LAMA$value,
                             LABA = self$LABA$value,
                             ICS = self$ICS$value,
                             azithromycin = self$azithromycin$value,
                             FEV1 = self$FEV1$value,
                             BMI = self$BMI$value,
                             SGRQ = self$SGRQ$value,
                             LastYrExacCount = self$LastYrExacCount$value,
                             LastYrSevExacCount = self$LastYrSevExacCount$value)
            self$prediction = predictACCEPT(patient)
            return()
        },

        getPrediction = function(dataType = "tibble") {
            dataTypes = c("tibble", "candleStick")
            pred = "predicted_"
            up = "_upper"
            low = "_lower"
            dataType = match.arg(dataType, dataTypes, several.ok = FALSE)
            if(dataType == "tibble") {
                prediction = self$prediction
            } else if (dataType == "candleStick") {
                prediction = self$prediction
                columns = names(prediction)
                for(patient in self$numPatients){
                    data = data.frame(init = rep(0,4))
                    for(column in columns) {
                        if(strContains(pred, column)){
                            columnName = strsplit(column, pred)[[1]][2]
                            if(strContains(up, column)) {
                                columnName = strsplit(columnName, up)[[1]][1]
                                data[[columnName]][4] = prediction[[column]][patient]
                            } else if(strContains(low, column)){
                                columnName = strsplit(columnName, low)[[1]][1]
                                data[[columnName]][1] = prediction[[column]][patient]
                            } else {
                                data[[columnName]][2] = prediction[[column]][patient]
                                data[[columnName]][3] = prediction[[column]][patient]
                            }
                        }
                    }
                    data = subset(data, select = -init)

                }
            }
            return(data)
        }

    )
)

