#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


# Define UI for application that draws a histogram
navbarPage(
  title = HTML("<img src='img/beijing_icon.jpeg' width='50' height='50'>"),
  
  tags$style(HTML("
    .leaflet-control.legend {
      font-size: 9px; 
    }
  ")),
  
  
  header = div(
    style = "text-align: center;", # Center the content horizontally
    h1("Beijing Air Quality Analysis"),
    hr()  # Horizontal line for separation
  ),
  setBackgroundColor("white"),

  
  tabPanel(
    h4("Time Series Analysis"),
    tabsetPanel(
      tabPanel('Aggregate Analysis',
               selectizeInput(inputId = "aggregate_particulate_selection",
                              label = "Particulate Selection",
                              choices = colnames(all_stations)[c(10:19,21,24)]
               ),
               plotOutput('aggregate_particulate_analysis'),
               fluidRow(
                 column(12, verbatimTextOutput("aggregate_dt_name"),
                        DT::dataTableOutput('aggregate_dt')
                 ),
               )
      ),
      
      tabPanel('Monthly and Yearly',
        selectizeInput(inputId = "monthly_particulate_selection",
          label = "Particulate Selection",
          choices = colnames(all_stations)[c(10:19,21,24)]
        ),
        
        selectizeInput(inputId = "year_selection_monthly_analysis",
          label = "Year to Analyze",
          choices = unique(all_stations[4])
        ),
        selectizeInput(inputId = "month_selection",
          label = "Month of Year to Analyze",
          choices = c('January','February','March','April','May','June','July','August','September','October','November','December'),
          selected = 'March'
        ),
        
        plotOutput('yearly_particulate_analysis'),
        plotOutput('month_analysis'),
        fluidRow(
          column(6, verbatimTextOutput("yearly_dt_name"),
            DT::dataTableOutput('yearly_dt')
          ),
          column(6, verbatimTextOutput("monthly_dt_name"),
            DT::dataTableOutput('monthly_dt')
          )
        )
      ),
      
      tabPanel('Daily',
               selectizeInput(inputId = "daily_particulate_selection",
                              label = "Particulate Selection",
                              choices = colnames(all_stations)[c(10:19,21,24)]
               ),
               dateInput('daily_date_input',
                         label = 'Date to Analyze: yyyy-mm-dd',
                         value = max(all_stations$date),
                         min =  min(all_stations$date), max = max(all_stations$date)
               ),
               plotOutput('daily_particulate_analysis'),
               
               
               fluidRow(
                 column(12, verbatimTextOutput("daily_dt_name"),
                        DT::dataTableOutput('mean_daily_particulate')
                 )
               )
      ),
      
    )
  ),
  
  tabPanel(
    h4("Heat Map"),
    fluidRow(
      fluidRow(
               div(
                 style = "text-align: center;",
                 column(12,
                        h3('Map of Beijing and Data Collection Stations'),
                 ),
               ),
               column(6,
                      div(style = "text-align: center;",
                          verbatimTextOutput("mean_heatmap_title")
                      ),
                      leafletOutput('beijing_map')
               ),
               column(6,
                      div(style = "text-align: center;",
                          verbatimTextOutput("total_particulates_heatmap_title")
                      ),
                      leafletOutput('total_particulates_map')
               )
      ),
      fluidRow(
               column(6,
                      div(
                        selectizeInput(inputId = "heatmap_particulate_selection",
                                       label = "Particulate Selection",
                                       choices = colnames(all_stations)[c(10:15)]
                        ),
                        style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",   
                      )
               ),
               column(6,
                      div(
                        dateInput('heatmap_date_input',
                                  label = 'Date to Analyze: yyyy-mm-dd',
                                  value = max(all_stations$date),
                                  min =  min(all_stations$date), max = max(all_stations$date)
                        ),
                        style = "display: flex; flex-direction: column; align-items: center; justify-content: center;"
                      )
               ),
      ),
      fluidRow(
               column(12,"datatable for the heatmap",
                      #create the datatable
                      DT::dataTableOutput('heatmap_dt')
               )
      )
    )
  ),
  
  tabPanel(
    h4("Station Comparison"),
    fluidRow(
             fluidRow(
                      column(5,offset = 1,
                             selectizeInput(inputId = "station_name1_line_graph",
                                            label = "First Station Name",
                                            choices = unique(all_stations$station)
                             ),
                             
                             selectizeInput(inputId = "station_name2_line_graph",
                                            label = "Second Station Name",
                                            choices = unique(all_stations$station),
                                            selected = unique(all_stations$station)[2]
                             ),
                             selectizeInput(inputId = "line_graph_particulate_selection",
                                            label = "Particulate Selection",
                                            choices = colnames(all_stations)[c(10:19,21,24)]
                             ),
                      ),
                      column(5, offset = 1,
                             dateRangeInput('dateRangeLineGraph',
                                            label = 'Date range input: yyyy-mm-dd',
                                            start = min(all_stations$date), end = max(all_stations$date),
                                            min =  min(all_stations$date), max = max(all_stations$date)
                             ),
                             
                      )
             ),
    ),
    tabsetPanel(
      tabPanel('Mean',
               plotOutput('meanParticulatesLineGraph'),
               fluidRow(
                 column(6, verbatimTextOutput("station_1_name_mean"),
                        DT::dataTableOutput('station_1_dt_mean')
                 ),
                 column(6, verbatimTextOutput("station_2_name_mean"),
                        DT::dataTableOutput('station_2_dt_mean')
                 )
               )
      ),
      tabPanel('Maximum',
               plotOutput('maxParticulatesLineGraph'),
               fluidRow(
                 column(6, verbatimTextOutput("station_1_name_max"),
                        DT::dataTableOutput('station_1_dt_max')
                 ),
                 column(6, verbatimTextOutput("station_2_name_max"),
                        DT::dataTableOutput('station_2_dt_max')
                 )
               )
      ),
      tabPanel('Minimum',
               plotOutput('minParticulatesLineGraph'),
               fluidRow(
                 column(6, verbatimTextOutput("station_1_name_min"),
                        DT::dataTableOutput('station_1_dt_min')
                 ),
                 column(6, verbatimTextOutput("station_2_name_min"),
                        DT::dataTableOutput('station_2_dt_min')
                 )
               )
      )
    )
  ),
  
  
  
  
)

  
  

