#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#import
library(shinydashboard)
library(shiny)
library(ggplot2)
library(geosphere)
library(scales)

#Define Variables and load in data up front if necessary
CovidConfirmedCases <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
AFBaseLocations <- read.csv("https://gitlab.com/messer06/covid/-/raw/master/AFB_Locs.csv?inline=false")
CountyInfo <- read.csv("https://gitlab.com/messer06/covid/-/raw/master/County_Info.csv?inline=false")
HospitalInfo <- read.csv("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
CovidDeaths<-read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
colnames(CovidDeaths)[1]<-"CountyFIPS"
Minus<-ncol(CovidDeaths)
CovidDeaths<-CovidDeaths[,-(Minus)]
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)

# #Create list of hospitals, bases, and counties.
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County


#Build UI
ui <- tagList(
    dashboardPage(
        dashboardHeader(title = "COVID-19 Risk Dashboard",
                        titleWidth = 300,
                        dropdownMenu(
                            headerText = "Want to know more?",
                            icon = icon("info-circle"),
                            tags$li(actionLink("inputInfo", label = "User Inputs", icon = icon("sliders-h")),
                                    class = "dropdown"),
                            tags$li(actionLink("calcInfo", label = "Calculations", icon = icon("calculator")),
                                    class = "dropdown"),
                            tags$li(actionLink("sourceInfo", label = "Sources", icon = icon("user-secret")),
                                    class = "dropdown")
                        )
        ),
        dashboardSidebar(width = 300, 
                         sidebarMenu(
                             selectInput(
                                 "Base",
                                 "Choose your base:", 
                                 list(`Installation` = sort(BaseList) ), 
                                 selectize = FALSE),
                             sliderInput("Radius",
                                         "Choose your local radius (miles):",
                                         min = 10,
                                         max = 100,
                                         value = 25),
                             br(),
                             menuItem(
                                 "Extra Projection Inputs",
                                 tabName = "dashboard",
                                 icon = icon("sliders-h"),
                                 div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                 sliderInput("proj_days",
                                             "How far into the future? (Weeks):",
                                             min = 1,
                                             max = 12,
                                             value = 2),
                                 sliderInput("social_dist",
                                             "% Social distancing in your area:",
                                             min = 0,
                                             max = 100,
                                             value = 60)
                             ),
                             br(),
                             actionButton("refresh", "Refresh", width = "90%"),
                             hr()
                         )
        ),
    
    dashboardBody(
        tags$head(tags$style(HTML(
            '.myClass { 
      font-size: 20px;
      line-height: 50px;
      text-align: left;
      font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
      padding: 0 15px;
      overflow: hidden;
      color: white;
      }
      '))),
        tags$script(HTML('
                     $(document).ready(function() {
                     $("header").find("nav").append(\'<span class="myClass"> Tailored Risk Assesments </span>\');
                     })
                     ')),
        fluidRow(
            valueBox("LOW RISK", subtitle ="Mission Risk",color= "green",icon = icon("smile")),
            valueBox("MEDIUM RISK", subtitle ="Installation Health Risk",color= "yellow",icon = icon("meh")),
            valueBox("HIGH RISK", subtitle ="Local Health Risk",color= "red",icon = icon("frown"))
        ),
        
        tabsetPanel(id = "tabs",
                    ####### START OVERALL RISK TAB #######
                    tabPanel(
                        title = "Summary",
                        htmlOutput("SummaryPlot")
                        
                    ),
                    ####### END OVERALL RISK TAB #######
                    ####### START MISSION RISK TAB #######
                    tabPanel(
                        title = "Mission",
                        value = plotOutput("plot")
                    ),
                    ####### END MISSION RISK TAB #######
                    ####### START INSTALLATION HEALTH RISK TAB #######
                    tabPanel(
                        title = "Installation Health", 
                        fluidRow(
                            # A static valueBox
                            valueBoxOutput("TotalPopulation"),
                            valueBox(2, subtitle ="Total Air Force Deaths", color= "red",icon = icon("skull")),
                            valueBox("85%", subtitle = "Base MTF Burden", color = "teal", icon = icon("hospital"))
                        ),
                        box(status = "primary", width = 13, solidHeader = T, "Current Risk Level: LOW ", align = "center"),
                        fluidRow( 
                            box(title = "Chart 1 Here", "Box content"),
                            box(title = "Chart 2 Here", "Box content")
                        )
                    ),
                    ####### END INSTALLATION HEALTH RISK TAB #######
                    ####### START LOCAL HEALTH RISK TAB #######
                    tabPanel(
                        title = "Local Health",
                        fluidRow(
                            # A static valueBox
                            valueBoxOutput("CovidCases"),
                            valueBoxOutput("LocalCovidDeaths"),
                            valueBoxOutput("HospitalUtilization")
                        ),
                        box(status = "primary", width = 13, solidHeader = T, "Current Risk Level: LOW ", align = "center"),
                        fluidRow( 
                            plotOutput("LocalHealthPlot1"),
                            box(title = "Chart 2 Here", "Box content")
                        )
                    )
                    ####### END LOCAL HEALTH RISK TAB #######
        )
    )
)
)
#Close UI  
###############################
CalculateCounties<-function(ChosenBase, Radius){
    #Finds which counties in given radius. Also Give county statistics
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:3143) {
        CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
    TotalPopulation <-  sum(IncludedCounties$Population)
    TotalPopulation
}

# Finds Covid Cases and statistics on covid per county
CalculateCovid<-function(ChosenBase, Radius){
    #Finds which counties in given radius. Also Give county statistics
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:3143) {
        CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    sum(CovidCounties[,ncol(CovidCounties)])
}

CalculateDeaths<-function(ChosenBase, Radius){
    #Finds which counties in given radius. Also Give county statistics
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:3143) {
        CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    sum(CovidCountiesDeath[,ncol(CovidCountiesDeath)])
}

HospitalIncreases<-function(ChosenBase, Radius){
    #Finds number of hospitals in radius
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:7581) {
        HospitalInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(HospitalInfo$LONGITUDE[i], HospitalInfo$LATITUDE[i]), fun = distHaversine)/1609.34)
    }
    IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= Radius))
    IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
    TotalBeds<-sum(IncludedHospitals$BEDS)
    
    #Finds which counties in given radius. Also Give county statistics
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:3143) {
        CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    n<-ncol(CovidCounties)-6
    TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
    NotHospital<-sum(CovidCounties[,n])
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Upper<-(signif((StillHospital/TotalBeds*.314+.6)*100,3))
    Lower<-(signif((StillHospital/TotalBeds*.207+.6)*100,3))
    paste(Lower, Upper, sep = "-")
}

#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesPerDayChart<-function(ChosenBase, Radius){
    #Finds number of hospitals in radius
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:7581) {
        HospitalInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(HospitalInfo$LONGITUDE[i], HospitalInfo$LATITUDE[i]), fun = distHaversine)/1609.34)
    }
    IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= Radius))
    IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
    TotalBeds<-sum(IncludedHospitals$BEDS)
    
    #Finds which counties in given radius. Also Give county statistics
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:3143) {
        CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    VectDailyCovid<-colSums(CovidCounties[5:length(CovidCounties)])
    DailyNewCases<-VectDailyCovid[2:length(VectDailyCovid)]-VectDailyCovid[1:(length(VectDailyCovid)-1)]
    DailyNewCases
    DailyNewHospitalizations<-ceiling(DailyNewCases*.26)
    DailyNewHospitalizations
    
    
    #Find New Deaths
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    for (i in 1:3143) {
        CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    }
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    VectDailyDeaths<-colSums(CovidCountiesDeath[5:ncol(CovidCountiesDeath)])
    DailyNewDeaths<-VectDailyDeaths[2:length(VectDailyDeaths)]-VectDailyDeaths[1:(length(VectDailyDeaths)-1)]
    
    
    ForecastDate<- seq(as.Date("2020-01-22"), length=(length(DailyNewDeaths)), by="1 day")
    Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCases,DailyNewHospitalizations,DailyNewDeaths)
    colnames(Chart1Data)<-c("ForecastDate","New Cases","New Hospitalizations","New Deaths")
    Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
    
    #Plot the forecasts from above but include the actual values from the test data to compare accuracy.
    ggplot(Chart1DataSub) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
        scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Number of People') +
        ggtitle("COVID-19 Impact", subtitle = "Local Area") +
        theme(text = element_text(size = 15)) +
        labs(color='Legend')
    
    
    
    
}


##########################################
# Define server logic, this is where all plots are generated. 
server <- function(input, output) {
    

    
    #Finds which counties in given radius. Also Give county statistics
    output$TotalPopulation <- renderValueBox({
        valueBox(subtitle = "Total Air Force Cases",
                 comma(CalculateCounties(input$Base,input$Radius)),
                 icon = icon("list-ol")
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$CovidCases <- renderValueBox({
        valueBox(subtitle = "Local Cases",
                 comma(CalculateCovid(input$Base,input$Radius)),
                 icon = icon("list-ol")
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$LocalCovidDeaths <- renderValueBox({
        valueBox(subtitle = "Local Deaths",
                 comma(CalculateDeaths(input$Base, input$Radius)),
                 icon = icon("skull"),
                 color = "red"
        )
    })
    
    #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
    output$HospitalUtilization <- renderValueBox({
        valueBox(subtitle = "Local Hospital Utilization",
                 HospitalIncreases(input$Base,input$Radius),
                 icon = icon("hospital"),
                 color = "teal")
        })
    
    #Create first plot of local health population 
    output$LocalHealthPlot1<-renderPlot({
        CovidCasesPerDayChart(input$Base, input$Radius)
    })
    
    #Create Plot on Summary page
    output$SummaryPlot<-renderGvis({
    DF<-cbind.data.frame(CovidConfirmedCases$State, CovidConfirmedCases[,length(CovidConfirmedCases)])
    colnames(DF)<-c("state","Value")
    ChlorData<-plyr::ddply(DF, "state", numcolwise(sum))
    
    ChlorData<-ChlorData %>% 
        mutate(state_name = state.name[match(state, state.abb)])
    ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
    states <- data.frame(ChlorData$state_name, ChlorData$Value)
    colnames(states)<-c("state_name","COVID-19 Cases")
    
            gvisGeoChart(states, "state_name", "COVID-19 Cases", 
                              options=list(region="US", 
                                           displayMode="regions", 
                                           resolution="provinces",
                                           width=900, height=700))
    
    })
    

    # 
    # 
    # 
    # 
    # 
    #

    
    observeEvent(input$inputInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "USER INPUTS",
                p("Some information"))
        )
    })
    observeEvent(input$calcInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "CALCULATIONS",
                p("Some information"))
        )
    })
    observeEvent(input$sourceInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "SOURCES",
                p("Some information"))
        )
    })  
    
    
}

##########################################
# Run the application 
shinyApp(ui, server)
