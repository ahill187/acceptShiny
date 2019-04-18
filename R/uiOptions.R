

library(shiny)
library(shinythemes)
library(shinydashboard2)
source("./R/helper_functions.R")
source("./R/AppLayout.R")
source("./R/DashGraph.R")
source("./R/colorSchemes.R")
source("./R/AcceptData.R")
source("./R/makeInputs.R")
load(file = "./data/appData.RData")
load(file = "./data/tabItemsList.RData")

tab_titles = appData$appLayout$mainTabs$titles
numberOfTabs = appData$appLayout$subTabs$number
i = 1
appLayout <- AppLayout$new(1, "acceptPredict")

makeUI = function(uiType){
    if(uiType == "noSidebar") {
        ui = uiNoSidebar()
    } else {
        ui = uiSidebar()
    }
}



makeUISidebar = function(){

    ui = dashboardPage(
        skin = appLayout$dashboardColour,

        # header
        dashboardHeader(title = appData$title, titleWidth = 320),
        # sidebar
        dashboardSidebar(
            sidebarMenu(
                id = "selectedTab",
                menuItem(
                    tab_titles[1],
                    tabName = "tab1",
                    icon = icon("bar-chart", lib = "font-awesome")
                ),
                menuItem(
                    tab_titles[2],
                    tabName = "tab2",
                    icon = icon("address-book", lib = "font-awesome")
                ),
                menuItem(
                    tab_titles[3],
                    tabName = "tab3",
                    icon = icon("balance-scale", lib = "font-awesome")
                )

            )
        ),
        # body
        dashboardBody(
            list(
                tabItems(
                    asList = T,
                    lapply(1:numberOfTabs, function(i) {
                        tabItemsList[[i]]$tabItem()
                    })),
                tags$style(type="text/css",
                           ".shiny-output-error {visibility: hidden;}",
                           ".shiny-output-error:before {visibility: hidden;}")),
            asList = T)
    )
    return(ui)
}
uiNoSidebar =function(){
    ui =  dashboardPage(
        skin = appLayout$dashboardColour,
        # header
        dashboardHeader(title = appData$title, titleWidth = 320),
        # sidebar
        dashboardSidebar(
            disable = TRUE
        ),
        #body
        dashboardBody(
            list(
                tabItemsList[[1]]$tabItem(),
                tags$style(type="text/css",
                           ".shiny-output-error {visibility: hidden;}",
                           ".shiny-output-error:before {visibility: hidden;}")),
            asList = T)

    )
    return(ui)
}
