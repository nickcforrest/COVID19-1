##########################################
# Define server logic, this is where all plots are generated. 
server <- function(input, output) {
    
    GetCounties<-reactive({
        #BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
        
        CountyInfo$DistanceMiles = cimd[,as.character(input$Base)]
        #for (i in 1:3143) {
        #    CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
        #}
        IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= input$Radius)
        IncludedCounties
    })
    
    GetHospitals<-reactive({
        #Finds number of hospitals in radius
        #BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
        
        HospitalInfo$DistanceMiles = himd[,as.character(input$Base)]
        
        IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= input$Radius))
        IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
        IncludedHospitals
    })
    
    #Finds which counties in given radius. Also Give county statistics
    output$TotalPopulation <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Total Air Force Cases",
                 comma(CalculateCounties(input$Base,input$Radius, MyCounties)),
                 icon = icon("list-ol")
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$CovidCases <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Local Cases",
                 comma(CalculateCovid(input$Base,input$Radius,MyCounties)),
                 icon = icon("list-ol")
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$LocalCovidDeaths <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Local Deaths",
                 comma(CalculateDeaths(input$Base, input$Radius, MyCounties)),
                 icon = icon("skull"),
                 color = "red"
        )
    })
    
    #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
    output$HospitalUtilization <- renderValueBox({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        valueBox(subtitle = "Local Hospital Utilization",
                 HospitalIncreases(input$Base,input$Radius, MyCounties, MyHospitals),
                 icon = icon("hospital"),
                 color = "teal")
    })
    
    #Create first plot of local health population 
    output$LocalHealthPlot1<-renderPlot({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        CovidCasesPerDayChart(input$Base, input$Radius, MyCounties,MyHospitals)
    })
    
    #Create second plot of local health population 
    output$LocalHealthPlot2<-renderPlot({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        CovidCasesCumChart(input$Base, input$Radius, MyCounties,MyHospitals)
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
