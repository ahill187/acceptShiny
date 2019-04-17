
shinyGoogleChart = function(tabNumber, tabItemsList, input, appData, data, chartType){
    selectedTabItem = tabItemsList[[tabNumber]]
    sidebarShownIds = selectedTabItem$sidebarShownIds
    googleChartId = tabItemsList[[tabNumber]]$googleChartOutputId
    year = input[[sidebarShownIds[5]]]
    xAxis = input[[sidebarShownIds[1]]]
    yAxis = input[[sidebarShownIds[2]]]
    state1 = input[[sidebarShownIds[3]]]
    state2 = input[[sidebarShownIds[4]]]
    color = input[[sidebarShownIds[6]]]
    size = input[[sidebarShownIds[7]]]
    id = appData$tabs$column0[[tabNumber]]

    if (color == "None") {
        color = NULL
    }
    if (size == "None") {
        size = NULL

    }
    if (year == "All Years") {
        year = c(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                 36, 37, 38)
    } else {
        year = as.numeric(year) - 2000
    }


    chart = googleChart(
        title = "",
        elementId = googleChartId,
        chartType = "candleStickChart",
        headerTitles = c(""),
        data = data,
        column0="test0",
        column1="test2",
        column2="test1"
    )
    return(chart)
}



