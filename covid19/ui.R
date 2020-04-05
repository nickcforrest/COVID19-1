##########################
##### User Interface #####
##########################

# Layout
##############################################################################################################################################
# The User Interface generates the visual of the application. It establishes location and layout of all outputs and inputs from server and user
# First:  The dashboard header shows the main title and introduction to the application
# Second: The sidebar shows all inputs that the user can change
# Third:  The body provides all visual outputs, statistics, and charts. It is updated every time a user changes the inputs
##############################################################################################################################################     




# Begin User Interface ------------------------------------------------------------------------------------------------------------------------------------------------------------



#Build UI
#Establishes the layout of the overall dashboard and how items are displayed
ui <- tagList(
    dashboardPage(skin = "black",
                  
                  # Step One - Header
                  ###################################################################################################################################################
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
                  
                  # Step Two - Sidebar
                  ###################################################################################################################################################
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
                                           "Extra Inputs",
                                           tabName = "dashboard",
                                           icon = icon("sliders-h"),
                                           div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                           sliderInput("proj_days",
                                                       "Projection days:",
                                                       min = 14,
                                                       max = 120,
                                                       value = 60),
                                           sliderInput("social_dist",
                                                       "% Social distancing in your area:",
                                                       min = 0,
                                                       max = 100,
                                                       value = 50)
                                       ),
                                       br(),
                                       #actionButton("refresh", "Refresh", width = "90%"),
                                       hr(),
                                       fluidRow(
                                           valueBox("LOW RISK", subtitle ="Mission Risk **notional ex.**",color= "green",width = 12)
                                       ),
                                       fluidRow(
                                           valueBox("MEDIUM RISK", subtitle ="Installation Health Risk **notional ex.**",color= "yellow", width = 12)
                                       ),
                                       fluidRow(
                                           valueBox("HIGH RISK", subtitle ="Local Health Risk **notional ex.**",color= "red",width = 12)
                                       )
                                   )
                                   
                  ),
                  
                  # Step Three - Body
                  ###################################################################################################################################################
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
                                   $("header").find("nav").append(\'<span class="myClass"> WASH YOUR HANDS </span>\');
                                   })
                                   ')),
                      
                      tabsetPanel(id = "tabs",
                                  
                                  # Summary Tab -------------------------------------------------------------
                                  tabPanel(
                                      title = "National Summary",
                                      
                                      box(title = "National Impact Map",solidHeader = T, align = "center", htmlOutput("SummaryPlot"),width = 13),
                                      
                                      box(title = "National Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("NationalDataTable1"), style = "height:240px;overflow-y: scroll;overflow-x:scroll"),width = 13)
                                      
                                  ),
                                  ####### END SUMMARY TAB #######
                                  
                                  
                                  # Current Local Health ----------------------------------------------------
                                  tabPanel(
                                      title = "Current Local Health",
                                      fluidRow(
                                          # A static valueBox
                                          valueBoxOutput("CovidCases"),
                                          valueBoxOutput("LocalCovidDeaths"),
                                          valueBoxOutput("HospitalUtilization")
                                      ),
                                      fluidRow(
                                          valueBoxOutput("CaseChangeLocal", width = 4),
                                          valueBoxOutput("DeathChangeLocal", width = 4),
                                          valueBoxOutput("HospUtlzChange", width = 4)
                                      ),
                                      fluidRow( 
                                          box(title = "Daily Reports",plotlyOutput("LocalHealthPlot1",height = 300)),
                                          box(title = "Total Reports",plotlyOutput("LocalHealthPlot2",height = 300))
                                      ),
                                      fluidRow(
                                          box(title = "Local Impact Map", plotOutput("LocalChoroPlot", height = 250),height = 300),
                                          box(title = "Local County Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("CountyDataTable1"), style = "height:240px;overflow-y: scroll"), height = 300)
                                      )
                                  ),
                                  ####### END CURRENT LOCAL HEALTH TAB #######
                                  
                                  ####### BEGIN LOCAL PROJECTION TAB #########
                                  # Local Health Projections ------------------------------------------------
                                  tabPanel(
                                      title = "Local Health Projections",
                                      fluidRow(
                                          valueBoxOutput("TotalPopulation")
                                      ),
                                      fluidRow(
                                          box(plotlyOutput("IHME_State_Hosp",height = 400)),
                                          box(plotlyOutput("SEIARProjection"),height = 400))
                                  ),
                                  ####### END PROJECTION TAB #######
                                  
                                  ####### BEGIN MISSION RISK TAB #########
                                  # Mission Risk ------------------------------------------------------------
                                  tabPanel(
                                      title = "Mission Risk",
                                      "Hello World")
                                      
                                  
                                  ####### END Mission Risk #######
                                  
                                  # ####### START ACME TABS #######
                                  # tabPanel("App Overview",
                                  #          tabsetPanel(
                                  #              tabPanel('Overview',
                                  #                       includeMarkdown("overview.md")
                                  #              ),
                                  #              tabPanel('SIR Model',
                                  #                       includeMarkdown("overview_sir.md")
                                  #              ),
                                  #              tabPanel('SEIAR Model',
                                  #                       includeMarkdown("overview_seiar.md")
                                  #              )
                                  #          ) #Close inner tabsetPanel
                                  # ),
                                  # tabPanel('National Impact',
                                  #          htmlOutput("covid_frame"),
                                  #          HTML("<center><h4><a href='https://github.com/iankloo/cov_map' target='_blank'>Data and Map Source</a></h4></center>")
                                  # ),
                                  # tabPanel('Model Controls',
                                  #          box(width = 6,
                                  #              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  #                  radioButtons("model_type", label = h3("Model Type"),
                                  #                               choices = list("SIR" = 1, "SEIAR" = 2),
                                  #                               selected = 1)),
                                  #              div(style="display: inline-block;vertical-align:top; width: 150px;",
                                  #                  radioButtons("pop_type", label = h3("Population"),
                                  #                               choices = list("Army MTF" = 1, "Ad hoc" = 2),
                                  #                               selected = 1))
                                  # 
                                  #          ),
                                  #          tabBox(width=12,
                                  #                 tabPanel("Variables",
                                  #                          fluidRow(
                                  #                              box(width=3,
                                  #                                  conditionalPanel(
                                  #                                      condition = "input.pop_type == 1",
                                  #                                      uiOutput('baseNames')
                                  #                                  ),
                                  #                                  conditionalPanel(
                                  #                                      condition = "input.pop_type == 2",
                                  #                                      textInput("base_name_ad_hoc",
                                  #                                                h3("Name of MTF"),
                                  #                                                value = 'Ad hoc MTF'),
                                  #                                      numericInput("ad_hoc_pop",
                                  #                                                   h3("Starting Population"),
                                  #                                                   value = 1000),
                                  #                                      numericInput("ad_hoc_beds",
                                  #                                                   h3("Available Beds"),
                                  #                                                   value = 10)
                                  #                                  ),
                                  #                                  numericInput("num_init_cases",
                                  #                                               h3("Currently Hospitalized COVID-19 Patients"),
                                  #                                               value = 1),
                                  #                                  numericInput("social_rate",  h3("Social distancing (% reduction in social contact)"),
                                  #                                               value = 30),
                                  #                                  numericInput("n_days",  h3("Number of days to project"),
                                  #                                               value = 180),
                                  #                                  br(),br(),
                                  #                                  #just adding some spacing
                                  #                                  actionButton('model_run', "Run Model",
                                  #                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:200%")
                                  # 
                                  #                              ),
                                  #                              box(width = 3,
                                  #                                  h2("Rates of Impact"),
                                  #                                  numericInput("hospital_rate",  h3("Hospitalization Proportion (% of total infections)"),
                                  #                                               value = 3),
                                  #                                  numericInput("icu_rate",  h3("ICU Proportion (% of Hospitialization)"),
                                  #                                               value = 16),
                                  #                                  numericInput("ventilated_rate",  h3("Ventilated Proportion (% of ICU)"),
                                  #                                               value = 44)
                                  #                              ),
                                  #                              box(width = 3,
                                  #                                  h2("Duration"),
                                  #                                  numericInput("hospital_dur",  h3("Hospital Length of Stay"),
                                  #                                               value = 7),
                                  #                                  numericInput("icu_dur",  h3("ICU Length of Stay"),
                                  #                                               value = 9),
                                  #                                  numericInput("ventilated_dur",  h3("Ventilation Length of Stay"),
                                  #                                               value = 10)
                                  #                              ),
                                  #                              box(width = 3,
                                  #                                  h2("Advanced Variables"),
                                  #                                  numericInput("doubling", h3("Doubling time before social distancing (days)"),
                                  #                                               value = 6),
                                  #                                  numericInput("recovery_days", h3("Days to recover once infected"),
                                  #                                               value = 14),
                                  #                                  conditionalPanel(
                                  #                                      condition = "input.model_type == 1",
                                  #                                      numericInput("detect_prob",h3("The Proportion of infections found (% of Infections Identified)"),
                                  #                                                   value= 50)
                                  #                                  ),
                                  #                                  conditionalPanel(
                                  #                                      condition = "input.model_type == 2",
                                  #                                      numericInput("latent_period",h3("Latent Period (days)"),
                                  #                                                   value= 2),
                                  #                                      numericInput("incub_period", h3("Time to symptoms (days)"),
                                  #                                                   value=5.5)
                                  #                                  )
                                  # 
                                  # 
                                  #                              )
                                  #                          ) #end fluid row
                                  #                 ) #end tab panel
                                  #          ) # end tab box
                                  # )
                                  # 
                                  # ,
                                  # tabPanel("Facility Impact",
                                  #          uiOutput('base_name'),
                                  #          tabBox(width=12,
                                  #                 tabPanel("Census Analysis",
                                  #                          fluidRow(
                                  # 
                                  #                          ),
                                  #                          fluidRow(
                                  #                              box(width = 3,
                                  #                                  valueBoxOutput('total_pop_box', width = 12),
                                  #                                  valueBoxOutput('r_t', width = 12),
                                  #                                  valueBoxOutput('doubling_box', width = 12),
                                  #                                  valueBoxOutput('beds', width = 12)
                                  #                              ),
                                  #                              box(width = 9,
                                  #                                  align="right",
                                  # 
                                  #                                  tableOutput("stats_table"),
                                  # 
                                  #                                  # radioButtons("growth_plot_select",
                                  #                                  #              label = '',
                                  #                                  #              choices=c("All","Hospitial","ICU","Vent"),
                                  #                                  #              selected = "All",
                                  #                                  #              inline = TRUE),
                                  #                                  plotlyOutput("growth_plot"),
                                  #                                  br(),
                                  #                                  downloadButton('download_growth_plot', 'Download Plot')
                                  #                              )
                                  #                          )
                                  #                 ),
                                  #                 tabPanel("Admissions Analysis",
                                  #                          fluidRow(
                                  #                              box(width=12,
                                  #                                  uiOutput('base_name2'),
                                  #                                  tableOutput("daily_stats_table"),
                                  #                                  plotlyOutput("daily_admit_plot"),
                                  #                                  downloadButton('download_daily_plot', 'Download Plot')
                                  # 
                                  #                              )
                                  #                          )
                                  #                 ),
                                  #                 tabPanel("Model Analysis",
                                  #                          fluidRow(
                                  #                              box(width = 12,
                                  #                                  plotlyOutput("model_plot"),
                                  #                                  downloadButton('download_model_plot', 'Download Plot')
                                  #                              )
                                  #                          )),
                                  #                 tabPanel("Data",
                                  #                          fluidRow(box(width=12,DT::dataTableOutput('sir_data_table'),downloadButton('download_sir_data')))
                                  #                 )
                                  # 
                                  #          )
                                  # 
                                  # ),
                                  # tabPanel("Compare",
                                  #          box(width = 12,
                                  #              h3('Select an MTF to run a comparison.'),
                                  #              uiOutput('baseCompare'),
                                  #              actionButton('compare_run', "Run Comparison",
                                  #                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:200%")
                                  #          ),
                                  #          tabBox(width=12,
                                  #                 tabPanel("Census",
                                  #                          plotOutput('compare_plot')
                                  #                 ),
                                  #                 tabPanel("Admit",
                                  #                          plotOutput('compare_daily_plot')
                                  #                 )
                                  #          )
                                  # ),
                                  # tabPanel("Report",
                                  #          tabBox(width=12,
                                  #                 tabPanel("Report",
                                  #                          fluidRow(
                                  #                              box(width=6,
                                  #                                  h4("Download MTF Report Here"),
                                  #                                  downloadButton("report", "Generate report")
                                  #                              )
                                  #                          )
                                  #                 )
                                  #          )
                                  # )
                                  # ####### END ACME TABS #######                              
                      )
                  )
    ),
    
    #The footer is just showing the information on the main contributors to the app
    tags$footer("created by Nick Forrest, Trey Pujats, Garrett Alarcon, James Deitschel", align = "center", style = "
              position:absolute;
              bottom:50;
              width:100%;
              height:25px;   /* Height of the footer */
              color: grey;
              padding: 0px;
              background-color: transparent;
              z-index: 1000;")
)
#Close UI 
