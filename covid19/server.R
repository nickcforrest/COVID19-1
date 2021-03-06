##################
##### Server #####
##################

# Layout
##############################################################################################################################################
# The server is used to generate outputs based on the functions in the global. These outputs are then referenced in the UI and diplayed in the app
# First:  Creating reactive functions that change based on radius and Base. The reactive functions are the most important functions in the app.
#         Reactive functions change every time a new base is chosen or a radius is chosen. This updated the app automatically.
# Second: This creates the output variables that can be referenced in the user interface. Each plot, statistic or map needs to have an output.
#         There are 5 sub categories included: Common statistics, line plots, choropleth charts, projections, and data tables.
# Third:  This creates the help settings in the app so that users can see documentation of inputs, sources, and calculations.
##############################################################################################################################################       


# Define server logic, within this all ouputs and reactive variables are generated. 
server <- function(input, output) {
    
    
    # Step One
    ###################################################################################################################################################
    
    
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
    
    # Step Two
    ###################################################################################################################################################
    
    
    # Output common statistics -------------------------------------------------------------------------------------------------------------------------------------------
    
    #Finds which counties in given radius. Also Give county statistics
    output$TotalPopulation <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Total Regional Population",
                 comma(CalculateCounties(input$Base,input$Radius, MyCounties)),
                 icon = icon("list-ol"),
                 color = "light-blue"
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$CovidCases <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Local Cases",
                 comma(CalculateCovid(input$Base,input$Radius,MyCounties)),
                 icon = icon("list-ol"),
                 color = "light-blue"
        )
        
    })
    
    #Outputs change in covid cases per day
    output$CaseChangeLocal <- renderValueBox({
        MyCounties<-GetCounties()
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        
        valueBox(paste("+",toString(changeC)),
                 subtitle = "New Confirmed Cases", 
                 color = "light-blue")
    })
    
    
    # Finds Covid deaths and statistics on covid per county
    output$LocalCovidDeaths <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Local Fatalities",
                 comma(CalculateDeaths(input$Base, input$Radius, MyCounties)),
                 icon = icon("skull"),
                 color = "blue"
        )
    })
    
    #Outputs change in deaths per day   
    output$DeathChangeLocal <- renderValueBox({
        MyCounties<-GetCounties()
        CovidCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        
        valueBox(paste("+",toString(changeC)),
                 subtitle = "New Confirmed Fatalities", 
                 color = "blue")
    })
    
    #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
    output$HospitalUtilization <- renderValueBox({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        valueBox(subtitle = "Local Hospital Utilization *Partially Notional*",
                 HospitalIncreases(input$Base,input$Radius, MyCounties, MyHospitals),
                 icon = icon("hospital"),
                 color = "navy")
    })
    
    
    
    output$HospUtlzChange <- renderValueBox({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        
        #Finds number of hospitals in radius
        TotalBeds<-sum(MyHospitals$BEDS)
        #Finds which counties in given radius. Also Give county statistics
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
        NotHospital<-sum(rev(CovidCounties)[,7])
        StillHospital<-ceiling((TotalHospital-NotHospital))
        Upper<- round(((StillHospital+changeC*.1)/TotalBeds+.5)*100,1)
        #Lower<- round(((StillHospital+changeC*.207)/TotalBeds+.55)*100,1)
        paste(Upper," %", sep = "") 
        
        
        TotalBeds<-sum(MyHospitals$BEDS)
        
        #Finds which counties in given radius. Also Give county statistics
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        n <- ncol(CovidCounties)-6
        x <- length(CovidCounties)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        changey <- sum(rev(CovidCounties)[,2] - rev(CovidCounties)[,3])
        # Today
        TotalHospital<-sum(rev(CovidCounties)[,1])
        NotHospital<-sum(rev(CovidCounties)[,6])
        StillHospital<-ceiling((TotalHospital-NotHospital))
        Upper<-(signif(((StillHospital+changeC*.1)/TotalBeds+.5)*100,3))
        #Lower<-(signif(((StillHospital+changeC*.207)/TotalBeds+.6)*100,3))
        # Yesterday
        TotalHospitaly<-sum(CovidCounties[,ncol(CovidCounties)-1])
        NotHospitaly<-sum(CovidCounties[,n-1])
        StillHospitaly<-ceiling((TotalHospitaly-NotHospitaly))
        Uppery<-(signif(((StillHospitaly+changey*.1)/TotalBeds+.5)*100,3))
        #Lowery<-(signif(((StillHospitaly+changey*.207)/TotalBeds+.6)*100,3))
        chng <- round((Upper-Uppery)/2, 1)
        
        if (chng < 0) {
            sign <- ""
        } else {
            sign <- "+"
        }
        
        valueBox(paste(sign,toString(chng),"%"),
                 subtitle = "Hospital Utilization Change", 
                 color = "navy")
    })
    
    output$CHIMEPeakDate<-renderValueBox({
        MyCounties<-GetCounties()
        Peak<-CalculateCHIMEPeak(MyCounties, input$Base, input$Radius, input$social_dist, input$proj_days)
        Peak<-format(Peak,"%B %d")
        valueBox(subtitle = "CHIME Predicted Peak Hospitalization Date",
                 paste(Peak),
                 icon = icon("hospital"),
                 color = "blue")
    })
    
    output$IHMEPeakDate<-renderValueBox({
        MyHospitals<-GetHospitals()
        Peak<-CalculateIHMEPeak(input$Base, MyHospitals, input$Radius)
        Peak<-format(Peak,"%B %d")
        valueBox(subtitle = "IHME Predicted Peak Hospitalization Date",
                 paste(Peak),
                 icon = icon("hospital"),
                 color = "navy")
    })
    
    # Output line plots for the dashboard ----------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create first plot of local health population 
    output$LocalHealthPlot1<-renderPlotly({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        CovidCasesPerDayChart(input$Base, input$Radius, MyCounties,MyHospitals)
    })
    
    #Create second plot of local health population 
    output$LocalHealthPlot2<-renderPlotly({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        CovidCasesCumChart(input$Base, input$Radius, MyCounties, MyHospitals)
    })
    
    
    
    # Output Choropleth Charts ----------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create Country Plot on Summary page
    output$SummaryPlot<-renderGvis({
        DF<-cbind.data.frame(CovidConfirmedCases$State, rev(CovidConfirmedCases)[,1])
        colnames(DF)<-c("state","Value")
        ChlorData<-plyr::ddply(DF, "state", numcolwise(sum))
        
        ChlorData<-ChlorData %>% 
            mutate(state_name = state.name[match(state, state.abb)])
        ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
        states <- data.frame(ChlorData$state_name, ChlorData$Value)
        colnames(states)<-c("state_name","COVID-19 Cases")
        
        gvisGeoChart(states, "state_name", "COVID-19 Cases", 
                     options=list(region="US",
                                  colors="['#D3D3D3', 'red']",
                                  displayMode="regions", 
                                  resolution="provinces",
                                  width=1200,
                                  height = 600))
    })
    
    
    #Creates the local choropleth charts that change based on which base and radius.
    output$LocalChoroPlot<-renderPlotly({
        MyCounties<-GetCounties()
        PlotLocalChoro(MyCounties, input$Base, input$TypeLocal)
    })
    
    
    
    
    # Output Projections  ---------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create IHME plot by State projected hospitalization 
    output$IHME_State_Hosp<-renderPlotly({
        
        #Creating the stats and dataframes determined by the base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
        IncludedHospitals<-GetHospitals()
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)

        #Get regional and state populations
        MyCounties <- GetCounties()
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(MyCounties$Population)
        StPop <- sum(StPopList$Population)

        # Use Population ratio to scale IHME
        PopRatio <- RegPop/StPop

        # Get total hospital bed number across state
        IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
        TotalBedsState <- sum(IncludedHospitalsST$BEDS)

        # Calculate bed ratio
        BedProp <- TotalBedsCounty/TotalBedsState

        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
        IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
        IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)



        r1 <- ggplot(data=IHME_Region, aes(x=date, y=allbed_mean, ymin=allbed_lower, ymax=allbed_upper)) +
            geom_line(linetype = "dashed", size = 0.75) +
            geom_ribbon(alpha=0.3, fill = "tan3") +
            # geom_hline(yintercept = TotalBedsCounty * 0.5,
            #            linetype = "solid",
            #            color = "red") +
            labs(title = paste("IHME Hospitalization Projections for Selected Region"),
                 x = "Date", y = "Projected Daily Hospitalizations") +
            theme_bw() +
            theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1),
                  axis.line = element_line(color = "black"),
                  legend.position = "top",
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()) +
            scale_x_date(date_breaks = "2 week")

        ggplotly(r1)
    })
    
    
    #Output the SEIAR CHIME projections with a max, min, and expected value
    output$SEIARProjection<-renderPlotly({
        BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
        IncludedCounties<-GetCounties()
        #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
        #We include State, county, population in those counties, cases, fatalities, doubling rate
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
        DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
        CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
        CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
        CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
        colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
        
        #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
        #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
        ActiveCases<-rev(CovidCounties)[1:7]
        ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
        colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
        SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
        colnames(SIRinputs)<-c("cases","pop","doubling")
        
        
        ####################################################################################
        #Mean Estimate
        
        #Next we use the calculated values, along with estimated values from the CDC. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-8
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-input$social_dist
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-input$proj_days
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                                   ventilatortime,daysforecasted,Ro, .5)
        
        MyDates<-seq(Sys.Date()-(length(CovidCounties)-65), length=daysforecasted, by="1 day")
        DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
        TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
        
        
        ####################################################################################
        #Lower Estimate
        
        #Next we use the calculated values, along with estimated values from the CDC. 
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-10
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-input$social_dist
        hospitalizationrate<-5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-input$proj_days
        
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
        
        ####################################################################################
        #Upper Estimate
        #Next we use the calculated values, along with estimated values from the CDC. 
        
        #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
        cases<-SIRinputs$cases
        pop<-SIRinputs$pop
        doubling<-7
        
        #Established Variables at the start for every county or populations
        Ro<-2.5
        incubationtime<-5
        latenttime<-2
        recoverydays<-14
        socialdistancing<-input$social_dist
        hospitalizationrate<-5.5
        icurate<-6
        ventilatorrate<-3
        hospitaltime<-3.5
        icutime<-4
        ventilatortime<-7
        daysforecasted<-input$proj_days
        
        #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
        #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
        SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                                   socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                                   icutime,ventilatortime,daysforecasted,Ro, .5)
        
        DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
        TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
        colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
        colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases","Maximum Total Cases")
        
        DailyProjectionsSub <- melt(data.table(DailyData), id=c("ForecastDate"))
        TotalProjectionsSub <- melt(data.table(TotalData), id=c("ForecastDate"))
        
        DailyData$`Expected Daily Cases` <- round(DailyData$`Expected Daily Cases`,0)
        DailyData$`Minimum Daily Cases` <- round(DailyData$`Minimum Daily Cases`,0)
        DailyData$`Maximum Daily Cases` <- round(DailyData$`Maximum Daily Cases`,0)
        DailyData<-DailyData[-1,]
        
        #Plot for local area cumulative cases
        projections <- ggplot(data = DailyData, 
                              aes(x=ForecastDate,
                                  y=DailyData$`Expected Daily Cases`,
                                  ymin = DailyData$`Minimum Daily Cases`,
                                  ymax = DailyData$`Maximum Daily Cases`)) + 
            #geom_line(aes(x=ForecastDate, y=DailyData$`Expected Daily Cases`, ymin = DailyData$`Minimum Daily Cases` , ymax = DailyData$`Maximum Daily Cases`)) +
            geom_line(linetype = "dashed", size = 0.75) +
            geom_ribbon(alpha=0.3, fill = "cadetblue2") +
            #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
            xlab('Date') +
            ylab('Daily Hospitalizations') +
            ggtitle("Army SEIAR Projected Daily Cases") +
            theme_bw() + 
            theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
                  axis.title = element_text(face = "bold", size = 11, family = "sans"),
                  axis.text.x = element_text(angle = 60, hjust = 1), 
                  axis.line = element_line(color = "black"),
                  legend.position = "top",
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank()) +
            scale_x_date(date_breaks = "2 week")
        labs(color='')
        
        ggplotly(projections)
        
        
    })
    
    #Overlay Projected Plots
    output$OverlayPlots<-renderPlotly({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        PlotOverlay(input$Base, MyCounties, MyHospitals, input$social_dist, input$proj_days)
    })
    
    
    # Output any data tables ------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Render National Data Table on summary page
    output$NationalDataTable1<-DT::renderDataTable({
        NationalDataTable <- DT::datatable(data.frame(NationalDataTable),rownames = FALSE, options = list(dom = 'ft',ordering = F,"pageLength" = 51))
        NationalDataTable
    })
    
    output$CountyDataTable1<-DT::renderDataTable({
        MyCounties<-GetCounties()
        dt<-GetLocalDataTable(MyCounties)
        dt<-DT::datatable(dt, rownames = FALSE, options = list(dom = 't',ordering = F, "pageLength"=100))
        dt
    })
    
    
    
    # Output Report ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    
    
    
    
    
    # Step Three
    ###################################################################################################################################################
    
    #Step three provides input information for annotation of the overall app such as inputs, sources, and calculations. 
    observeEvent(input$inputInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "USER INPUTS",
                InfoLink)
        )
    })
    observeEvent(input$calcInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "CALCULATIONS",
                CalcLink)
        )
    })
    observeEvent(input$sourceInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "SOURCES",
                SourceLink)
        )
    })
    
    
    
    
    
    
}


