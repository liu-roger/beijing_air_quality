#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(leaflet)
library(maps)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DT)
library(con2aqi)
#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/

all_stations = read_csv('all_stations.csv')

# Define UI for application that draws a histogram
navbarPage(
  h4("Beijing Air Quality"),
  # img(src='img/beijing_icon.jpeg', width='50%'),
  tabPanel(
    h4("Heat Map"),
    fluidRow(style = "border: 4px double red;",
      column(12,
        h4('Beijing Air Quality Analysis'),
        #img(src='img/beijing_icon.jpeg', width='50%', align='middle'),
        
        fluidRow(style = "border: 4px double red;",
          column(6,
            # 'Particulate Selection',
            dateRangeInput('date_range_heatmap_datatable',
              label = 'Date range input for the Data Table: yyyy-mm-dd',
              start = Sys.Date() - 2, end = Sys.Date() + 2
            )
          ),
          column(6,
            # 'Date Range Input',
            dateInput('date_heatmap',
              label = 'Date input for the heatmap: yyyy-mm-dd',
              value = Sys.Date()
            ),
            # dateRangeInput('date_range_heatmap_datatable',
            #   label = 'Date range input for the Data Table: yyyy-mm-dd',
            #   start = Sys.Date() - 2, end = Sys.Date() + 2
            # )
          ),
        )
      ),
      fluidRow(style = "border: 4px double red;",
        column(12,
          h4('Map of Beijing and Data Collection Stations'),
          leafletOutput('beijing_map')
        )
      ),
      fluidRow(style = "border: 4px double red;",
        column(12,"datatable for the heatmap",
          #create the datatable
          DT::dataTableOutput('heatmap_dt')
        )
      )
    )
  ),
  
  tabPanel(
    h4("Line Graphs"),
    fluidRow(style = "border: 4px double red;",
      column(12,h4("Beijing Air Quality Analysis"),
        # img(src='img/beijing_icon.jpeg', width='50%'),
        fluidRow(style = "border: 4px double red;",
          column(6,
            selectizeInput(inputId = "station_name1_line_graph",
              label = "First Station Name",
              choices = unique(all_stations$station)
            ),
            
            selectizeInput(inputId = "station_name2_line_graph",
              label = "Second Station Name",
              choices = unique(all_stations$station)
            ),
            selectizeInput(inputId = "line_graph_particulate_selection",
              label = "Particulate Selection",
              choices = colnames(all_stations)[c(10:19,21)]
            ),
          ),
          column(6, "this is where the time slider is displayed",
            dateRangeInput('dateRangeLineGraph',
            label = 'Date range input: yyyy-mm-dd',
            start = Sys.Date() - 2, end = Sys.Date() + 2
            ),
            selectizeInput(inputId = "time_aggregation",
              label = "Time Frame",
              choices = colnames(all_stations)[c(4,5)]
            ),
            
          )
        ),
        fluidRow(style = "border: 4px double red;",
          column(12,"this is where the line graph is displayed",
            plotOutput('particulatesLineGraph'),
          
            fluidRow(
              column(5, verbatimTextOutput("station_1_name"),
                DT::dataTableOutput('station_1_dt')
              ),
              column(5, verbatimTextOutput("station_2_name"),
                DT::dataTableOutput('station_2_dt')
              ),
              column(2,
                checkboxGroupInput("show_vars", "Particulates to Show:",
                  names(all_stations[c(2,10:15)]), selected = names(all_stations[c(2,10:15)])
                )
              )
            )
          )
        )
      )
    )
  )
)
  
  

