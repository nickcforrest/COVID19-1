##################
##### Global #####
##################

# Layout
##############################################################################################################################################
# The global introduces all libraries, functions, datasets, and formatting that is necessary to pass through the server.
# First:  The libraries are loaded which have built in functions are used throughout the app.
# Second: We load data from https://github.com/treypujats/COVID19/tree/master/covid19/data ,  usafacts.org , and covid19.healthdata.org
#         The github data has static information on on all air force bases, US counties, and US hospitals. 
#         The usafacts data has dynamic information that is updated daily reporting the number of cases and number of deaths daily.
#         The IHME provides projection data of the pandemic
#         After loading the data, we format headers, establish data tables for printing, and do any static changes to the dataset for the app.
# Third:  Functions are used to execute the tasks in the server. Functions in the global are not dynamic, but they take in dynamic inputs
#         Global functions are used to calculate statistics, make data tables, plot graphs, and create visuals.
##############################################################################################################################################       



# Step One
###################################################################################################################################################
#Loads in all necessary packages for the shiny app

library(plyr)
library(dplyr)
library(ggplot2)
#library(tidyverse)
library(tidyr)
library(shinydashboard)
library(shiny)
library(geosphere)
library(scales)
library(googleVis)
#library(reshape2)
library(usmap)
library(data.table)



# Step Two
###################################################################################################################################################
#Define Variables and load in data up front if necessary.
#This data updates daily with CovidConfirmedCases and CovidDeaths. The CDC updates these numbers every day.
#The static data (countyinfo, hospitalinfo, AFBaseLocations) is used to for lat and long coordinates to measure distance.
#Hospital Data allows us to determine the bed capacity of all hospitals in the nation
#AFBaseLocations provide names and coordinates of base.
#CountyInfo is used to measure population of a county and coordinates.

CovidConfirmedCases <- data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
AFBaseLocations <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/raw/master/covid19/data/baseinfo.rda"))
CountyInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/raw/master/covid19/data/countyinfo.rda"))
HospitalInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/hospitalinfo.rda?raw=true"))
CovidDeaths<-as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
himd = as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/himd.rda?raw=true"))
cimd = as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/cimd.rda?raw=true"))

#Updating data frames to ensure they are filled and match the data we reference later in the scripts
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
colnames(CovidDeaths)[1]<-"CountyFIPS"
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)
CovidConfirmedCases[is.na(CovidConfirmedCases)]<-0
CovidDeaths[is.na(CovidDeaths)]<-0
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"


#Read in IHME data for projecting data in the future
temp <- tempfile()
download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", temp, mode="wb")
filename = paste(format(as.Date(Sys.Date()-1), "%Y"), "_",
                 format(as.Date(Sys.Date()-1), "%m"), "_",
                 format(as.Date(Sys.Date()-1), "%d"), ".2",
                 "/Hospitalization_all_locs.csv", 
                 sep = "")

unzip(temp, files = filename)
IHME_Model <- read.csv(filename)
unlink(temp)
IHME_Model$date <- as.Date(IHME_Model$date, format = "%Y-%m-%d")
StateList <- data.frame(state.name, state.abb)
IHME_Model <- merge(IHME_Model, StateList, by.x = "location", by.y = names(StateList)[1])
names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"


#Create list of hospitals, bases, and counties.
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County



######################Data Specific to plotting counties and states as choropleth
#Input the Included Counties as factors
PlottingCountyData<- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
                              header = TRUE, stringsAsFactors = FALSE)
PlottingCountyData$county <- tolower(gsub("([A-Za-z]+).*", "\\1", PlottingCountyData$County.Name))
PlottingCountyData$county <- gsub("^(.*) parish, ..$","\\1", PlottingCountyData$county)
#Creating state name in addition to state abb
PlottingCountyData<-PlottingCountyData %>% 
    mutate(state_name = tolower(state.name[match(State, state.abb)]))
#Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
county_df <- map_data("county")
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL
#Calling in state data so we can map it correctly
state_df <- map_data("state", projection = "albers", parameters = c(39, 45))
colnames(county_df)[6]<-"State"


#Create National Data table on summary page
NationalDataTable<-CovidConfirmedCases
NationalDataTable$State<-as.factor(NationalDataTable$State)
NationalDataTable<-NationalDataTable[,-c(1,2,4)]
NationalDataTable<-aggregate(.~State, NationalDataTable, sum)
RateofCovidChange<-rev(NationalDataTable)[c(1:7)]
RateofCovidChange<-ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable<-CovidDeaths
NationalDeathTable$State<-as.factor(NationalDeathTable$State)
NationalDeathTable<-NationalDeathTable[,-c(1,2,4)]
NationalDeathTable<-aggregate(.~State, NationalDeathTable, sum)
RateofDeathChange<-rev(NationalDeathTable)[c(1:7)]
RateofDeathChange<-ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)

NationalDataTable<-data.frame(NationalDataTable$State, NationalDataTable[,length(NationalDataTable)],RateofCovidChange, NationalDeathTable[,length(NationalDeathTable)], RateofDeathChange)
colnames(NationalDataTable)<-c("State","Total Cases","Average New Cases Per Day", "Total Deaths","Average New Deaths Per Day")
NationalDataTable$`Cases Per 1000 People`<-c(731449,4822023,2949131,6553255,38041430,5187582,3590347,633427,917092,19317568,9919945,1392313,3074186,1595728,12875255,6537334,2885905,4380415,4601893,6646144,5884563,1329192,9883360,5379139,6021988,2984926,1005141,9752073,699628,1855525,1320718,8864590,2085538,2758931,19570261,11544225,3814820,3899353,12763536,1050292,4723723,833354,6456243,26059203,2855287,8185867,626011,6897012,5726398,1855413,576412)
NationalDataTable$`Cases Per 1000 People`<-round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 1000 People`/1000))



#Step Three
###################################################################################################################################################

# Establish Local Counties ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#These Functions establishes which counties are going to be included in the analysis determined by the base and radius.
CalculateCounties<-function(ChosenBase, Radius, IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    TotalPopulation <-  sum(IncludedCounties$Population)
    TotalPopulation
}


# Create Numerical Statistics for the dashboard -------------------------------------------------------------------------------------------------------------------------------------

# Finds Covid Cases and statistics on covid per county
CalculateCovid<-function(ChosenBase, Radius, IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    sum(rev(CovidCounties)[,1])
}

CalculateDeaths<-function(ChosenBase, Radius, IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    sum(CovidCountiesDeath[,ncol(CovidCountiesDeath)])
}

HospitalIncreases<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
    TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
    NotHospital<-sum(rev(CovidCounties)[,7])
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Upper<- round(((StillHospital+changeC*.1)/TotalBeds+.6)*100,1)
    #Lower<- round(((StillHospital+changeC*.207)/TotalBeds+.55)*100,1)
    paste(Upper," %", sep = "") 
}



# Create Charts for plotting lines showing trends among the virus  ------------------------------------------------------------------------------------------------------------------


#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesPerDayChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    n<-as.numeric(length(CovidCounties))
    VectDailyCovid<-colSums(CovidCounties[,29:n])
    DailyNewCases<-VectDailyCovid[2:length(VectDailyCovid)]-VectDailyCovid[1:(length(VectDailyCovid)-1)]
    DailyNewCases
    DailyNewHospitalizations<-ceiling(DailyNewCases*.26)
    DailyNewHospitalizations
    #Find New Deaths
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    VectDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
    DailyNewDeaths<-VectDailyDeaths[2:length(VectDailyDeaths)]-VectDailyDeaths[1:(length(VectDailyDeaths)-1)]
    #Clean up the dataset to prepare for plotting
    ForecastDate<- seq(as.Date("2020-02-15"), length=(length(DailyNewDeaths)), by="1 day")
    Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCases,DailyNewHospitalizations,DailyNewDeaths)
    colnames(Chart1Data)<-c("ForecastDate","New Cases","New Hospitalizations","New Fatalities")
    Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
    #plot for local area daily cases
    ggplot(Chart1DataSub) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
        scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Number of People') +
        theme(text = element_text(size = 15)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(color='')+
        theme_bw()+
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
        ) +
        theme(axis.line = element_line(color = "black"))+
        theme(legend.position = "top")
}



#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CumDailyCovid<-colSums(CovidCounties[,29:length(CovidCounties)])
    CumHospitalizations<-ceiling(CumDailyCovid*.26)
    #Find New Deaths
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CumDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
    #Clean up the dataset to get ready to plot it
    ForecastDate<- seq(as.Date("2020-02-15"), length=(length(CumDailyDeaths)), by="1 day")
    Chart2Data<-cbind.data.frame(ForecastDate,CumDailyCovid,CumHospitalizations,CumDailyDeaths)
    colnames(Chart2Data)<-c("ForecastDate","Total Cases","Total Hospitalizations","Total Fatalities")
    Chart2DataSub <- melt(data.table(Chart2Data), id=c("ForecastDate"))
    #plot for local area cumulative cases
    ggplot(Chart2DataSub,height = 250) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
        scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Number of People') +
        theme(text = element_text(size = 15)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(color='')+
        theme_bw()+
        theme(
            plot.background = element_blank()
            ,panel.grid.major = element_blank()
            ,panel.grid.minor = element_blank()
            ,panel.border = element_blank()
        ) +
        theme(axis.line = element_line(color = "black"))+
        theme(legend.position = "top")
}



# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------

#Create Data Table for local statistics
GetLocalDataTable<-function(IncludedCounties){
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,2], rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities" )
    CountyDataTable
}



# Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------

#Create plot of Covid Cases by County
PlotLocalChoro<-function(IncludedCounties, ChosenBase){
    BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Creating the choropleth dataset so we have all info in one data set and can plot it together
    choropleth <- merge(county_df, PlottingCountyData, by = c("county", "State"))
    choropleth <- choropleth[order(choropleth$order), ]
    choropleth$state_name<-NULL
    choropleth<-data.frame(choropleth$county, choropleth$State, choropleth$group, choropleth$lat, choropleth$long, rev(choropleth)[,1])
    colnames(choropleth)<-c("County","State","group","lat","long","Cases")
    choropleth<-subset(choropleth, State %in% IncludedCounties$State)
    
    #Plot the data
    PlotCovidLocal<-ggplot(choropleth, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = log(Cases))) +
        coord_fixed() +
        theme_minimal() +
        ggtitle("COVID-19 Cases by County") +
        geom_label(label= BaseStats$Base,data = BaseStats, aes(x=Long, y=Lat, group = 1),
                   color = 'black', size = 4, vjust = -1)+
        geom_point(data = BaseStats, aes(x=Long, y=Lat, group = 1),
                   color = 'red', size = 5)+
        theme(axis.line = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), axis.title = element_blank()) +
        scale_fill_viridis("log(Cases)")
    
    plot(PlotCovidLocal)
}

 # Identify Info Pages
 #Inputs
InfoLink <- includeMarkdown("https://github.com/treypujats/COVID19/blob/master/covid19/InputsInfo.html")
CalcLink <- includeMarkdown("https://github.com/treypujats/COVID19/blob/master/covid19/InputsInfo.html")
SourceLink <- includeMarkdown("https://github.com/treypujats/COVID19/blob/master/covid19/InputsInfo.html")

