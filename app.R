#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(geosphere)

#Define Variables and load in data up front if necessary
CovidConfirmedCases <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
AFBaseLocations <- read.csv("https://gitlab.com/messer06/covid/-/raw/master/AFB_Locs.csv?inline=false")
CountyInfo <- read.csv("https://gitlab.com/messer06/covid/-/raw/master/County_Info.csv?inline=false")
HospitalInfo<-read.csv("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
DistanceData<-read.csv("https://github.com/treypujats/COVID19/raw/master/Distance_Matrix.csv")

#Create list of hospitals, bases, and counties.
BaseList<-AFBaseLocations$Base
HospitalList<-HospitalInfo$NAME
CountyList<-CountyInfo$County

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 Risk Projections"),

    # Sidebar with a slider input for installation commander prioritization 
    sidebarLayout(
        sidebarPanel(
            selectInput("Base", "Choose a Base:",
                        list(`Installation` = list("Wright-Patterson", "Nellis", "Pentagon"))
                     ),         
            h2("Preferences"),
            sliderInput("MissionPref",
                        "Mission Prioritization:",
                        min = 0,
                        max = 1,
                        value = .3),
            sliderInput("InstallationPref",
                        "Installation Health Prioritization:",
                        min = 0,
                        max = 1,
                        value = .3),
            sliderInput("LocalPref",
                        "Local Area Health Prioritization:",
                        min = 0,
                        max = 1,
                        value = .4),
            h2("CHIME INPUTS?")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            
            # Show a plot of the generated distribution
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Overall", plotOutput("plot")),
                            tabPanel("Mission", plotOutput("plot")),
                            tabPanel("Installation", plotOutput("plot")),
                            tabPanel("Local", plotOutput("plot"))
                            
                )
            )
        )
        ),
    
    # Sidebar with a slider input for installation commander prioritization 
    sidebarLayout(
        sidebarPanel(h2("Key Statistics"),
                 
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(h2("In Depth Statistics")
            

        )
    )
)   
#Close UI    

###############################
#Call Python Script
#source_python('CountyID.py')


##########################################
# Define server logic, this is where all plots are generated. 
server <- function(input, output) {
    
    output$plot <- reactivePlot(function() {
        # check for the input variable
       
            Data <- data.frame(x = x, var = MissionPref*x)
    
         
        p <- ggplot(Data, aes(var, x)) + 
            xlab(input$variable)
        print(p)
    })
}
    

##########################################

# Run the application 
shinyApp(ui = ui, server = server)
