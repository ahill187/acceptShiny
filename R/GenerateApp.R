
source("R/TabItemDashGraph.R")
source("R/TabItemDashText.R")

appData = fromJSON("static_data/app.json")
dataSubClasses = appData$tabs$sidebarKeys

tabItemsList = list()
for(i in 1:appData$appLayout$subTabs$number){
    tabType = appData$tabs$tabType[[i]]
    if(tabType == "map"){
        layerChoices = appData$tabs$layerChoices[[i]]$valueName
        names(layerChoices) = appData$tabs$layerChoices[[i]]$label
        tabItemsList[[appData$tabs$inputId[i]]] = TabItemDashMap$new(
            title = appData$tabs$title[[i]],
            inputId = appData$tabs$inputId[[i]],
            mainBoxColor = appData$tabs$mainBoxColor[[i]],
            valueBoxNumber = appData$tabs$valueBoxNumber[[i]],
            valueBoxWidths = appData$tabs$valueBoxWidths[[i]],
            tabNumber = i,
            layerChoices = layerChoices,
            numLayers = appData$tabs$leafletMap$numLayers[i])
    } else if(tabType == "graph") {
        tabItemsList[[appData$tabs$inputId[i]]] = TabItemDashGraph$new(
            title = appData$tabs$title[i],
            inputId = appData$tabs$inputId[[i]],
            mainBoxColor = appData$tabs$mainBoxColor[[i]],
            tabNumber = i,
            dropdownChoices = appData$tabs$dropdownChoices[[i]]$valueName,
            dropdownSelected = appData$tabs$dropdownSelected[[i]],
            dropdown = appData$tabs$dropdown[[i]],
            pngDownloadName = appData$tabs$pngDownloadName[[i]],
            sidebarChoicesNumber = appData$tabs$sidebarChoicesNumber[[i]],
            sidebarShownLabels = appData$tabs$sidebarShownLabels[[i]],
            dataSubClasses = rawData$dataSubClasses,
            columnOptions = sapply(appData$tabs$columnOptions, "[[", i),
            columnTypes = appData$tabs$columnTypes[[i]],
            dataInput = appData$tabs$dataInput[[i]],
            dataTypes = appData$tabs$dataTypes[[i]],
            lower = appData$tabs$lower[[i]],
            upper = appData$tabs$upper[[i]],
            numberOfGraphs = appData$tabs$numberOfGraphs[[i]],
            categories = appData$tabs$categories[[i]]
        )
    } else if(tabType == "text") {
        tabItemsList[[appData$tabs$inputId[i]]] = TabItemDashText$new(
            title = appData$tabs$title[i],
            inputId = appData$tabs$inputId[i],
            tabNumber = i,
            markdownFileName = appData$tabs$markdownFileName[i],
            imageId = appData$tabs$imageId[i],
            imFile = appData$tabs$imFile[i]
        )
    }
}

save(appData, file="data/appData.RData")
save(tabItemsList, file="data/tabItemsList.RData")

