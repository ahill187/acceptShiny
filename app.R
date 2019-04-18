#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(googleCharts)
library(shiny)
library(shinythemes)
library(shinydashboard2)
library(devtools)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(htmltools)
library(maps) # interactive map
library(mapproj)
library(leaflet)
library(accept)
source("./R/helper_functions.R")
source("./R/AppLayout.R")
source("./R/DashGraph.R")
source("./R/colorSchemes.R")
source("./R/AcceptData.R")
source("./R/appBrickGoogleChart.R")
source("./R/makeInputs.R")
source("./R/uiOptions.R")
load(file = "./data/appData.RData")
load(file = "./data/tabItemsList.RData")

tab_titles = appData$appLayout$mainTabs$titles
numberOfTabs = appData$appLayout$subTabs$number
valueBoxIcons = list(icon("user", lib="font-awesome"), icon("usd", lib="font-awesome"))
i = 1
appLayout <- AppLayout$new(1, "acceptPredict")
initialize = TRUE
noSidebar = TRUE
uiType = "noSidebar"
cat("~~~ Starting UI ~~~", fill = T)

ui = makeUI(uiType)

server <- function(input, output, session) {
    cat("~~~ Starting server ~~~", fill = T)
    options(warn = -1)
    colorScheme = colorSchemes[[appData$appLayout$colorScheme]]

    selectedTab = reactive({
        if(noSidebar) {
            selectedTab = "tab1"
        } else {
            selectedTab = input$selectedTab
        }
    })

    tabNumber = reactive({
        tabItemsList[[selectedTab()]]$tabNumber
    })

    selectedTabItem = reactive({
        if(noSidebar) {
            return(tabItemsList$tab1)
        } else {
            return(tabItemsList[[tabNumber()]])
        }
    })


    observe({
        if (selectedTabItem()$title == "Graph") {
            for (i in 1:selectedTabItem()$sidebarChoicesNumber) {
                sidebarShownInput <- input[[selectedTabItem()$sidebarShownIds[i]]]
                sout(selectedTabItem()$sidebarHiddenIds[i])
                if (!is.null(sidebarShownInput) &&
                    sidebarShownInput == "Select") {
                    shinyjs::show (id = selectedTabItem()$sidebarHiddenIds[i], anim = TRUE)
                }
                else {
                    shinyjs::hide (id = selectedTabItem()$sidebarHiddenIds[i], anim = TRUE)
                }

            }
        }
    })

    lapply(1:numberOfTabs, function(i) {
        outputTypes = tabItemsList[[i]]$outputTypes
        tabItemDash = tabItemsList[[i]]
        sout("Setting up tab components for tab:", i)
        if (is.null(outputTypes)) {

        } else{
            lapply(1:length(outputTypes), function(k) {
                outputType = outputTypes[k]
                if (outputType == "inputSidebar") {

                }
                # OutputType 0: Google Chart
                if (outputType == "googleChartOutput") {
                    googleChartIds = tabItemDash$googleChartOutputIds
                    # columnOptions = reactive({
                    #     return(sapply(appData$tabs$columnOptions, "[[", tabNumber()))
                    # })
                    sidebarShownIds = reactive({
                        tabItemsList[[tabNumber()]]$sidebarShownIds
                    })
                    output[[googleChartIds[1]]] = renderGoogleChart({
                        inputList = makeInputs(appData$tabs$dataTypes[[tabNumber()]],
                                               appData$tabs$lower[[tabNumber()]],
                                               appData$tabs$upper[[tabNumber()]],
                                               appData$tabs$sidebarKeys[[tabNumber()]],
                                               appData$tabs$categories[[tabNumber()]])
                        shinyValueList = combineShinyInputs(isolate(input), sidebarShownIds())
                        valueList = updateInputs(inputList, shinyValueList)
                        data = AcceptData$new(
                            male = valueList[[1]],
                            age = valueList[[2]],
                            smoker = valueList[[3]],
                            oxygen = valueList[[4]],
                            statin = valueList[[5]],
                            LAMA = valueList[[6]],
                            LABA = valueList[[7]],
                            ICS = valueList[[8]],
                            azithromycin = valueList[[9]],
                            FEV1 = valueList[[10]],
                            BMI = valueList[[11]],
                            SGRQ = valueList[[12]],
                            LastYrExacCount = valueList[[13]],
                            LastYrSevExacCount = valueList[[14]]
                        )
                        data$calculatePrediction()
                        predictedData = data$getPrediction("candleStick")[,c(1,3)]
                        chart = shinyGoogleChart(tabNumber(),
                                                 tabItemsList,
                                                 input, appData,predictedData, "candleStickChart",
                                                 googleChartIds[1])
                        chart
                    })

                    output[[googleChartIds[2]]] = renderGoogleChart({
                        inputList = makeInputs(appData$tabs$dataTypes[[tabNumber()]],
                                               appData$tabs$lower[[tabNumber()]],
                                               appData$tabs$upper[[tabNumber()]],
                                               appData$tabs$sidebarKeys[[tabNumber()]],
                                               appData$tabs$categories[[tabNumber()]])
                        shinyValueList = combineShinyInputs(isolate(input), sidebarShownIds())
                        valueList = updateInputs(inputList, shinyValueList)
                        data2 = AcceptData$new(
                            male = valueList[[1]],
                            age = valueList[[2]],
                            smoker = valueList[[3]],
                            oxygen = valueList[[4]],
                            statin = valueList[[5]],
                            LAMA = valueList[[6]],
                            LABA = valueList[[7]],
                            ICS = valueList[[8]],
                            azithromycin = valueList[[9]],
                            FEV1 = valueList[[10]],
                            BMI = valueList[[11]],
                            SGRQ = valueList[[12]],
                            LastYrExacCount = valueList[[13]],
                            LastYrSevExacCount = valueList[[14]]
                        )
                        data2$calculatePrediction()
                        predictedData = data2$getPrediction("candleStick")[,c(2,4)]
                        chart = shinyGoogleChart(tabNumber(),
                                                 tabItemsList,
                                                 input, appData,predictedData, "candleStickChart",
                                                 googleChartIds[2])
                        chart
                    })
                }
                # OutputType 2: Download
                else if (outputType == "downloadOutput") {
                    sout("~~~ Download ~~~")
                    outputId = tabItemDash$downloadOutputId
                    output[[outputId]] <- downloadHandler(
                        filename = function() {
                            paste(tabItemDash$pngDownloadName,
                                  Sys.Date(),
                                  ".png",
                                  sep = "")
                        },
                        content = function(file) {
                            ggsave(
                                file,
                                device = "png",
                                width = 11,
                                height = 8.5
                            )
                        }
                    )
                }
                # OutputType 3: Image Output
                else if (outputType == "imageOutput") {
                    output[[tabItemDash$imageId]] <- renderImage({
                        width  <- session$clientData$output_logos_width
                        height <- session$clientData$output_logos_height
                        # Return a list containing the filename
                        sout("All good")
                        list(
                            src = paste0("./static_data/", tabItemDash$imFile),
                            contentType = 'image/png',
                            width = width,
                            alt = "Logos"
                        )
                    }, deleteFile = FALSE)
                }
                # OutputType 4: Leaflet Output
                else if (outputType == "leafletOutput") {
                    cat("~~~ Leaflet Map ~~~", fill = T)
                    mapOutputId = tabItemDash$mapOutputId
                    leafletMap <- reactive({
                        leafletMap <- leafletMapList[[selectedTab()]]
                        return(leafletMap)
                    })
                    year <- reactive({
                        input[[tabItemsList[[tabNumber()]]$sliderId]] - 2000
                    })

                    output[[mapOutputId]] <- renderLeaflet({
                        leafletMap()$drawMap(year())
                    })
                    cat("~~~ Setting up Info Boxes ~~~", fill = T)

                    mapShapeClick <- paste0(mapOutputId, "_shape_click")
                    changeLayer <-
                        paste0(mapOutputId, "_groups_baselayerchange")
                    value <- reactiveValues(noClickYet = FALSE, layer = 1)
                    valueBoxOutputIds <- tabItemDash$valueBoxOutputIds
                    lapply(1:tabItemDash$valueBoxNumber, function(box) {
                        boxId <- valueBoxOutputIds[box]
                        observeEvent(input[[mapShapeClick]], {
                            print(input[[mapShapeClick]])
                            value$default <- input[[mapShapeClick]]$id
                        })
                        sout("Still working")
                        output[[boxId]] <- renderValueBox({
                            valueName = tabItemDash$valueBoxChoices[box] # column of data to view in box
                            region <- eventReactive(input[[mapShapeClick]],
                                                    ignoreNULL = FALSE, {
                                                        # update the location selectInput on map clicks
                                                        input[[mapShapeClick]]$id
                                                    })
                            layerRegionId <- region()
                            if (is.null(layerRegionId)) {
                                layerRegionId <- "layer_1_region_1"
                            }
                            layerRegionId = strsplit(layerRegionId, "_")
                            layerId = as.numeric(layerRegionId[[1]][2])
                            regionId = as.numeric(layerRegionId[[1]][4])
                            value <-
                                leafletMap()$getLayerValueData(valueName = valueName,
                                                               year = year(),
                                                               layer = layerId)
                            regionName = leafletMap()$regionNames[regionId]
                            subtitles = names(tabItemDash$valueBoxChoices)
                            if (box == tabItemDash$valueBoxNumber) {
                                subtitle = subtitles[box]
                                subtitle = paste(subtitle, regionName)
                            } else{
                                subtitle = subtitles[box]
                            }
                            if (value[regionId] == 0 ||
                                value[regionId] == "No data") {
                                value = "No data"
                            } else {
                                value = paste(leafletMap()$prefix, value)
                            }
                            valueBox(
                                value = value[regionId],
                                subtitle = subtitle,
                                color = colorScheme[box],
                                icon = valueBoxIcons[[layerId]]
                            )
                        })
                    })
                }
            })
        }
    })



}


# Run the application
shinyApp(ui = ui, server = server)
