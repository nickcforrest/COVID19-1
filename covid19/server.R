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
                 icon = icon("list-ol")
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
        Upper<- round(((StillHospital+changeC*.1)/TotalBeds+.6)*100,1)
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
        Upper<-(signif(((StillHospital+changeC*.1)/TotalBeds+.6)*100,3))
        #Lower<-(signif(((StillHospital+changeC*.207)/TotalBeds+.6)*100,3))
        # Yesterday
        TotalHospitaly<-sum(CovidCounties[,ncol(CovidCounties)-1])
        NotHospitaly<-sum(CovidCounties[,n-1])
        StillHospitaly<-ceiling((TotalHospitaly-NotHospitaly))
        Uppery<-(signif(((StillHospitaly+changey*.1)/TotalBeds+.6)*100,3))
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
    
    
    # Output line plots for the dashboard ----------------------------------------------------------------------------------------------------------------------------------------------------
    
    
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
    output$LocalChoroPlot<-renderPlot({
        MyCounties<-GetCounties()
        PlotLocalChoro(MyCounties, input$Base)
    })
    
    
    
    # Output Projections  ---------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create IHME plot by State projected hospitalization 
    output$IHME_State_Hosp<-renderPlotly({
        #Creating the stats and dataframes determined by teh base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
        IncludedHospitals<-GetHospitals()
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)
        
        # Get total hospital bed number across state
        IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
        TotalBedsState <- sum(IncludedHospitalsST$BEDS)
        
        # Calculate bed ratio
        BedProp <- TotalBedsCounty/TotalBedsState
        
        # Apply ratio's to IHME data
        IHME_Region <- IHME_State
        IHME_Region$allbed_mean = round(IHME_State$allbed_mean*BedProp)
        IHME_Region$allbed_lower = round(IHME_State$allbed_lower*BedProp)
        IHME_Region$allbed_upper = round(IHME_State$allbed_upper*BedProp)
        
        r1 <- ggplot(data=IHME_Region, aes(x=date, y=allbed_mean, ymin=allbed_lower, ymax=allbed_upper)) +
            geom_line(linetype = "dashed", size = 0.75) +
            geom_ribbon(alpha=0.3, fill = "tan3") + 
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
    
    
    ##############################################################
    #START ARMY MODELS
    
    # currentValues <- reactiveValues() #create a list to hold reactive values for the app
    # currentValues$baseNames <- beds
    # currentValues$pops <- pops
    # 
    # # Nothing to plot
    # noPlot <- ggplot(data.frame(), aes(x=0,y=0,label="Run the comparison for a graph!")) +
    #     geom_label() +
    #     theme_minimal() +
    #     theme(axis.line=element_blank(),
    #           axis.text.x=element_blank(),
    #           axis.text.y=element_blank(),
    #           axis.ticks=element_blank(),
    #           axis.title.x=element_blank(),
    #           axis.title.y=element_blank(),
    #           legend.position="none",
    #           panel.background=element_blank(),
    #           panel.border=element_blank(),
    #           panel.grid.major=element_blank(),
    #           panel.grid.minor=element_blank(),
    #           plot.background=element_blank())
    # 
    # model_params <- reactive({
    #     t1 <- c('Population at Risk','Currently Hospitalized COVID-19 Patients','Social distancing (% reduction in social contact)',
    #             'Number of days to project','Hospitalization Portion (% of total infections)','ICU Portion (% of hospitalization)',
    #             'Ventilated Portion (% of ICU)','Hospital Length of Stay','ICU Length of Stay','Ventilation Length of Stay',
    #             'Doubling time before social distancing (days)','Days to recover once infected','The portion of infections found (% of Infections Identified)')
    #     t2 <- c(currentValues$current_pop, input$num_init_cases, input$social_rate, input$n_days, input$hospital_rate, input$icu_rate, input$ventilated_rate,
    #             input$hospital_dur, input$icu_dur, input$ventilated_dur,input$doubling, input$recovery_days, input$detect_prob)
    #     
    #     t <- data.frame(Parameter = t1, 
    #                     Value = t2)
    #     return(t)
    # })
    # 
    # #create the menu baar    
    # output$menuSidebar<-renderMenu({
    #     if(!is.null(currentValues$base_values)){
    #         sidebarMenu(
    #             menuItem("App Overview", tabName = "overview", icon = icon("info"),selected=FALSE),
    #             menuItem("National Impact", tabName = "national", icon = icon("map-marked"),selected=FALSE),
    #             menuItem("Model Controls", tabName = "model_controls", icon = icon("cog"),selected=FALSE),
    #             menuItem("Facility Impact", tabName = "fac_impact", icon = icon("users"),selected=TRUE),
    #             menuItem("Compare", tabName = "compare", icon = icon("balance-scale"),selected=FALSE),
    #             menuItem("MTF Reports", tabName = "report", icon = icon("table"),selected=FALSE)
    #         )
    #     }
    #     else{
    #         sidebarMenu(
    #             menuItem("App Overview", tabName = "overview", icon = icon("info"),selected=FALSE),
    #             menuItem("National Impact", tabName = "national", icon = icon("map-marked"),selected=FALSE),
    #             menuItem("Model Controls", tabName = "model_controls", icon = icon("cog"),selected=FALSE)
    #         )
    #     }
    # })
    # 
    # output$baseNames <-
    #     renderUI({
    #         input$base_load
    #         dataSets <- currentValues$pops$MTF
    #         selectInput("baseNames", 'Select an Air Force Medical Treatment Facility',
    #                     choices = BaseList ,
    #                     selected = "Abraham Lincoln National Airport",
    #                     multiple = FALSE,
    #                     selectize = TRUE)
    #     })
    # 
    # output$baseCompare <-
    #     renderUI({
    #         input$base_load
    #         dataSets <- currentValues$pops$MTF
    #         if(input$pop_type == 1){
    #             dataSets <- dataSets[!(dataSets %in% input$baseNames)]  
    #         } 
    #         selectInput("baseCompare", 'Select an Air Force Medical Treatment Facility',
    #                     choices = BaseList ,
    #                     selected = "Abraham Lincoln National Airport",
    #                     multiple = FALSE,
    #                     selectize = TRUE)
    #     })
    # 
    # observeEvent(input$model_run, {
    #     base_data <- pops[pops$MTF == input$baseNames,]
    #     beds_data <- beds[beds$Parent.MTF == input$baseNames,]
    #     
    #     if(input$pop_type == 1){
    #         currentValues$current_pop <- sum(base_data$Pop.At.Risk)
    #         currentValues$current_beds <- beds_data[1,2]
    #         base_name <- input$baseNames
    #     } else {
    #         currentValues$current_pop <- input$ad_hoc_pop
    #         currentValues$current_beds <-input$ad_hoc_beds
    #         base_name <- input$base_name_ad_hoc
    #     }
    #     
    #     if(input$model_type == 1){
    #         myList <- sir_model_run(input$num_init_cases, currentValues$current_pop, input$detect_prob, 
    #                                 input$doubling, input$recovery_days, input$social_rate, input$hospital_rate,
    #                                 input$icu_rate, input$ventilated_rate, input$hospital_dur, input$icu_dur, input$ventilated_dur, input$n_days)
    #     } else {
    #         myList <- seiar_model_run(input$num_init_cases, currentValues$current_pop, input$incub_period, input$latent_period, 
    #                                   input$doubling, input$recovery_days, input$social_rate, input$hospital_rate,
    #                                   input$icu_rate, input$ventilated_rate, input$hospital_dur, input$icu_dur, input$ventilated_dur, input$n_days, 
    #                                   secondary_cases = 2.5, distribution_e_to_a = 0.5)
    #     }
    #     
    #     
    #     myList$base_name <- base_name
    #     myList$base_data <- base_data
    #     myList$beds <- currentValues$current_beds
    #     
    #     currentValues$base_values <- myList
    # })
    # 
    # observeEvent(input$compare_run, {
    #     base_data <- pops[pops$MTF == input$baseCompare,]
    #     beds_data <- beds[beds$Parent.MTF == input$baseCompare,]
    #     
    #     currentValues$compare_pop <- sum(base_data$Pop.At.Risk)
    #     currentValues$compare_beds <- beds_data[1,2]
    #     base_name <- input$baseCompare
    #     
    #     
    #     if(input$model_type == 1){
    #         myList <- sir_model_run(input$num_init_cases, currentValues$compare_pop, input$detect_prob, 
    #                                 input$doubling, input$recovery_days, input$social_rate, input$hospital_rate,
    #                                 input$icu_rate, input$ventilated_rate, input$hospital_dur, input$icu_dur, input$ventilated_dur, input$n_days)
    #     } else {
    #         myList <- seiar_model_run(input$num_init_cases, currentValues$compare_pop, input$incub_period, input$latent_period, 
    #                                   input$doubling, input$recovery_days, input$social_rate, input$hospital_rate,
    #                                   input$icu_rate, input$ventilated_rate, input$hospital_dur, input$icu_dur, input$ventilated_dur, input$n_days, 
    #                                   secondary_cases = 2.5, distribution_e_to_a = 0.5)
    #     }
    #     
    #     
    #     myList$base_name <- base_name
    #     myList$base_data <- base_data
    #     myList$beds <- currentValues$current_beds
    #     
    #     currentValues$sir_compare <- myList
    #     
    # })
    # 
    # output$total_pop_box <- renderValueBox({
    #     valueBox(
    #         formatC(currentValues$current_pop,format='d', big.mark=','), c('Population at Risk'),
    #         icon=icon("users"), color='green', width=4)
    # })
    # output$r_t <- renderValueBox({
    #     valueBox(
    #         formatC(currentValues$base_values$r_t,format='f', digits = 2 ,big.mark=','), c('Rt'),
    #         icon=icon("project-diagram"), color='green', width=4)
    #     
    # })
    # output$doubling_box <- renderValueBox({
    #     valueBox(
    #         formatC(currentValues$base_values$doubling_time_t,digits = 2,format='f', big.mark=','), c('Doubling Time'),
    #         icon=icon("signal"), color='green', width=4)
    # })
    # output$beds <- renderValueBox({
    #     valueBox(
    #         formatC(as.integer(ceiling(currentValues$base_values$beds)),format='d', big.mark=','), c('Beds Available'),
    #         icon=icon("bed"), color='green', width=4)
    # })
    # 
    # output$base_name <-
    #     renderUI({ 
    #         h3(paste("Impact at",currentValues$base_values$base_name))
    #     })
    # 
    # output$base_name2 <-
    #     renderUI({ 
    #         h3(paste("Daily Admits at",currentValues$base_values$base_name))
    #     })
    # 
    # growth_plot <- reactive({
    #     t <- currentValues$base_values$sir %>% 
    #         select(t,hos_cum,icu_cum,vent_cum) %>% 
    #         gather(-t,key = 'state',value = 'number') %>% 
    #         mutate(state = ifelse(state == "hos_cum","Hospitalized",
    #                               ifelse(state == "icu_cum","ICU", 
    #                                      ifelse(state == "vent_cum", "Ventilated",state)))) 
    #     
    #     ggplot(t, aes(x=t, y=number,colour=state)) +
    #         geom_line(size = 1) + 
    #         geom_hline(yintercept = currentValues$base_values$beds) +
    #         xlab('Days from today') +
    #         ylab('Total Patients') +
    #         ggtitle(paste("Growth in Impact at",currentValues$base_values$base_name), 
    #                 subtitle = paste('Max Beds Required',currentValues$base_values$hos_max)) +  
    #         geom_text(aes(x=15, y = currentValues$base_values$beds,label = "Beds Available at \n"), color="black",data = data.frame()) +
    #         geom_text(aes(x=15, y = currentValues$base_values$beds,label = paste("\n",currentValues$base_values$base_name)), color="black",data = data.frame()) +
    #         theme_bw() +
    #         theme(legend.title = element_blank())
    # })
    # 
    # output$growth_plot <- renderPlotly({
    #     p <- ggplotly(growth_plot())
    #     print(p)
    # })
    # 
    # model_plot <- reactive({
    #     
    #     t <- currentValues$base_values$sir
    #     
    #     if(input$model_type == 1){
    #         t <- t %>% select(t,S,I,R) 
    #     } else {
    #         t <- t %>% select(t,S,E,I,A,R)
    #     }
    #     t <- t %>% 
    #         gather(-t,key = 'state',value = 'number') 
    #     
    #     ggplot(t, aes(x=t, y=number,colour=state)) +
    #         geom_line(size = 1) + 
    #         xlab('Days from today') +
    #         ylab('Total People') +
    #         ggtitle(paste("Model State Output at ",currentValues$base_values$base_name)) + 
    #         theme_bw() +
    #         theme(legend.title = element_blank())
    # })
    # 
    # output$model_plot <- renderPlotly({
    #     p <- ggplotly(model_plot())
    #     print(p)
    # })
    # 
    # growth_compare_plot <- reactive({
    #     t <- currentValues$base_values$sir
    #     t$base_name <- currentValues$base_values$base_name
    #     t1 <- currentValues$sir_compare$sir
    #     t1$base_name <- currentValues$sir_compare$base_name
    #     t <- rbind(t,t1)
    #     
    #     t <- t %>% 
    #         select(t,hos_cum,icu_cum,vent_cum, base_name) %>% 
    #         gather(-t,-base_name,key = 'state',value = 'number') %>% 
    #         mutate(state = ifelse(state == "hos_cum","Hospitalized",
    #                               ifelse(state == "icu_cum","ICU", 
    #                                      ifelse(state == "vent_cum", "Ventilated",state)))) 
    #     
    #     ggplot(t, aes(x=t, y=number,colour=state)) +
    #         geom_line(size = 1) + 
    #         xlab('Days from today') +
    #         ylab('Total Patients') +
    #         ggtitle("Comparison of Growth", 
    #                 subtitle = paste(currentValues$base_values$base_name, 'and',currentValues$sir_compare$base_name)) +
    #         facet_grid(cols = vars(base_name)) +
    #         theme_bw() +
    #         theme(legend.title = element_blank())
    # })
    # 
    # 
    # 
    # daily_plot <- reactive({
    #     t <- currentValues$base_values$sir %>% 
    #         select(t,hos_add,icu_add,vent_add) %>% 
    #         gather(-t,key = 'state',value = 'number') %>% 
    #         mutate(state = ifelse(state == "hos_add","Hospitalized",
    #                               ifelse(state == "icu_add","ICU", 
    #                                      ifelse(state == "vent_add", "Ventilated",state)))) 
    #     
    #     ggplot(t, aes(x=t, y=number,colour=state)) +
    #         geom_line(size = 1) + 
    #         #geom_hline(yintercept = currentValues$base_values$beds) +
    #         xlab('Days from today') +
    #         ylab('Daily Additional Patients') +
    #         ggtitle(paste("Daily Additional Impact at",currentValues$base_values$base_name), 
    #                 subtitle = paste('Maximum Daily Additional Beds',currentValues$base_values$hos_add)) +  
    #         #geom_text(aes(x=15, y = currentValues$base_values$beds,label = "Beds Available at \n"), color="black",data = data.frame()) +
    #         #geom_text(aes(x=15, y = currentValues$base_values$beds,label = paste("\n",currentValues$base_values$base_name)), color="black",data = data.frame()) +
    #         theme_bw() +
    #         theme(legend.title = element_blank())
    # })
    # 
    # output$daily_admit_plot <- renderPlotly({
    #     p <- ggplotly(daily_plot())
    #     print(p)
    # })
    # 
    # daily_compare_plot <- reactive({
    #     t <- currentValues$base_values$sir
    #     t$base_name <- currentValues$base_values$base_name
    #     t1 <- currentValues$sir_compare$sir
    #     t1$base_name <- currentValues$sir_compare$base_name
    #     t <- rbind(t,t1)
    #     
    #     t <- t %>% 
    #         select(t,hos_add,icu_add,vent_add, base_name) %>% 
    #         gather(-t,-base_name,key = 'state',value = 'number') %>% 
    #         mutate(state = ifelse(state == "hos_cum","Hospitalized",
    #                               ifelse(state == "icu_cum","ICU", 
    #                                      ifelse(state == "vent_cum", "Ventilated",state)))) 
    #     
    #     ggplot(t, aes(x=t, y=number,colour=state)) +
    #         geom_line(size = 1) + 
    #         xlab('Days from today') +
    #         ylab('Total Patients') +
    #         ggtitle("Comparison of Daily Admits", 
    #                 subtitle = paste(currentValues$base_values$base_name, 'and',currentValues$sir_compare$base_name)) +
    #         facet_grid(cols = vars(base_name)) +
    #         theme_bw() +
    #         theme(legend.title = element_blank())
    # })
    # 
    # output$compare_plot <- renderPlot({
    #     if(!is.null(currentValues$sir_compare)){
    #         print(growth_compare_plot())
    #     } else {
    #         print(noPlot)
    #     }
    # })
    # 
    # output$compare_daily_plot <- renderPlot({
    #     if(!is.null(currentValues$sir_compare)){
    #         print(daily_compare_plot())
    #     } else {
    #         print(noPlot)
    #     }
    # })
    # 
    # stats_table <- reactive({
    #     i <- currentValues$base_values$beds - currentValues$base_values$sir$hos_cum
    #     d <- 0
    #     for(j in 1:length(i)){
    #         if(i[j]<0){
    #             d <- j
    #             break
    #         }
    #     }
    #     
    #     t <- data.frame(Rt = currentValues$base_values$r_t,
    #                     `Doubling Time` = round(currentValues$base_values$doubling_time_t,2),
    #                     `Beds Available` = as.integer(ceiling(currentValues$base_values$beds)),
    #                     `Time Bed Capacity Reached` = as.integer(d),
    #                     `Max Required Beds` = as.integer(ceiling(currentValues$base_values$hos_max)),
    #                     `Max ICU` = as.integer(ceiling(currentValues$base_values$icu_max)),
    #                     `Max Ventilated` = as.integer(ceiling(currentValues$base_values$vent_max)),
    #                     `Time of Max` = as.integer(ceiling(currentValues$base_values$hos_t_max))
    #     )
    #     return(t)
    # })
    # 
    # output$stats_table <- renderTable({
    #     stats_table() %>% select(-Rt,-Doubling.Time,-Beds.Available)
    # },  
    # bordered = TRUE,  
    # align = 'c')
    # 
    # daily_stats_table <- reactive({
    #     i <- currentValues$base_values$beds - currentValues$base_values$sir$hos_cum
    #     d <- 0
    #     for(j in 1:length(i)){
    #         if(i[j]<0){
    #             d <- j
    #             break
    #         }
    #     }
    #     t <- data.frame(`Max Daily Additional Beds` = as.integer(ceiling(currentValues$base_values$hos_add)),
    #                     `Max Daily Additional ICU Patients` = as.integer(ceiling(currentValues$base_values$icu_add)),
    #                     `Max Daily Additional Ventilated` = as.integer(ceiling(currentValues$base_values$vent_add)),
    #                     `Time of Max` = as.integer(ceiling(currentValues$base_values$hos_t_max))
    #     ) 
    #     return(t)
    # })
    # 
    # output$daily_stats_table <- renderTable({
    #     daily_stats_table()
    # })
    # 
    # sir_data_table <- reactive({
    #     #post current results
    #     currentValues$base_values$sir <- currentValues$base_values$sir %>% 
    #         mutate_if(is.numeric, round, 1)
    #     sir_data <- currentValues$base_values$sir[,1:(ncol(currentValues$base_values$sir)-1)] 
    #     if(input$model_type == 1){
    #         colnames(sir_data)  <- c('day','S','I','R',
    #                                  'hospitial admit', 'hospital total',
    #                                  'ICU admit', 'ICU total',
    #                                  'vent admit', 'vent total')
    #     } else{
    #         colnames(sir_data)  <- c('day','S','E','I','A','R',
    #                                  'hospitial admit', 'hospital total',
    #                                  'ICU admit', 'ICU total',
    #                                  'vent admit', 'vent total')
    #     }
    #     
    #     return(sir_data)
    # })
    # 
    # #render previous saved analysis data table
    # output$sir_data_table<-DT::renderDataTable({
    #     sir_data_table()
    #     
    # }, server = TRUE, options = list(pageLength = 200, autoWidth = TRUE, scrollX = FALSE))
    # 
    # output$download_sir_data <- downloadHandler(
    #     filename = function() {
    #         paste('sir_data', currentValues$base_values$base_name,'_',Sys.Date(), '.csv', sep='')
    #     },
    #     content = function(file) {
    #         sir_data <- currentValues$base_values$sir[,1:(ncol(currentValues$base_values$sir)-1)] 
    #         if(input$model_type == 1){
    #             colnames(sir_data)  <- c('day','S','I','R',
    #                                      'hospitial admit', 'hospital total',
    #                                      'ICU admit', 'ICU total',
    #                                      'vent admit', 'vent total')
    #         } else{
    #             colnames(sir_data)  <- c('day','S','E','I','A','R',
    #                                      'hospitial admit', 'hospital total',
    #                                      'ICU admit', 'ICU total',
    #                                      'vent admit', 'vent total')
    #         }
    #         write.csv(sir_data, file, row.names = FALSE)
    #     }
    # )
    # 
    # output$download_growth_plot <- downloadHandler(
    #     filename = function() {
    #         paste0(gsub("\ ", "_", currentValues$base_values$base_name),
    #                '_growth_plot_(', paste(Sys.Date(), sep="_"), ')', '.png') },
    #     content = function(file) { ggsave(file, growth_plot()) }
    # )
    # 
    # output$download_daily_plot <- downloadHandler(
    #     filename = function() {
    #         paste0(gsub("\ ", "_", currentValues$base_values$base_name),
    #                '_growth_plot_(', paste(Sys.Date(), sep="_"), ')', '.png') },
    #     content = function(file) { ggsave(file, daily_plot()) }
    # )
    # 
    # output$download_model_plot <- downloadHandler(
    #     filename = function() {
    #         paste0(gsub("\ ", "_", currentValues$base_values$base_name),
    #                '_model_plot_(', paste(Sys.Date(), sep="_"), ')', '.png') },
    #     content = function(file) { ggsave(file, model_plot()) }
    # )
    # # Render and download a report
    # output$report <- downloadHandler(
    #     filename = function() {
    #         paste0(gsub("\ ", "_", currentValues$base_values$base_name),
    #                '_report_(', paste(Sys.Date(), sep="_"), ')', '.doc') },
    #     content = function(file) {
    #         daily_stats <- daily_stats_table() %>% gather(key = "Metric", value = "Value")
    #         daily_stats$Value <- round(daily_stats$Value, 2)
    #         stats <- stats_table() %>% gather(key = "Metric", value = "Value")
    #         stats$Value <- round(stats$Value, 2)
    #         if(input$model_type == 1){
    #             sir_data_table <- sir_data_table() %>%  select(-c('S','I','R'))
    #         } else{
    #             sir_data_table <- sir_data_table() %>%  select(-c('S','E','I','A','R'))
    #         }
    #         
    #         tempReport <- file.path(tempdir(), "report.Rmd")
    #         file.copy("report.Rmd", tempReport, overwrite = TRUE)
    #         params <- list(baseName = currentValues$base_values$base_name,
    #                        growth_plot = growth_plot(),
    #                        daily_plot = daily_plot(),
    #                        model_plot = model_plot(),
    #                        model_params = model_params(),
    #                        stats_table = stats,
    #                        daily_stats_table = daily_stats,
    #                        sir_data_table = sir_data_table
    #         )
    #         rmarkdown::render(tempReport, output_file = file,
    #                           params = params,
    #                           envir = new.env(parent = globalenv())
    #         )
    #     })
    # 
    # output$covid_frame <- renderUI({
    #     
    #     my_test <- tags$iframe(src='map.html', height = 700, width = '100%',frameborder = "no")
    #     print(my_test)
    #     my_test
    # })   
    
    
    
    
}



