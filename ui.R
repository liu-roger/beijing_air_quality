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
library(shinyWidgets)
#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


# Define UI for application that draws a histogram
navbarPage(
  title = HTML("<img src='img/beijing_icon.jpeg' width='50' height='50'>"),
  header = div(
    style = "text-align: center;", # Center the content horizontally
    h1("Beijing Air Quality Analysis"),
    hr()  # Horizontal line for separation
  ),
  setBackgroundColor("white"),
  tabPanel(
    h4("Heat Map"),
    fluidRow(
      fluidRow(style = "border: 1px solid #E54B4B;",
        div(
          style = "text-align: center;",
          column(12,
            h4('Map of Beijing and Data Collection Stations'),
          ),
        ),
        column(12,
          leafletOutput('beijing_map')
        )
      ),
      fluidRow(style = "border: 1px solid #E54B4B;",
        column(6,
          div(
            dateRangeInput('date_range_heatmap_datatable',
              label = 'Date range input for the Data Table: yyyy-mm-dd',
              start = Sys.Date() - 2, end = Sys.Date() + 2
            ),
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",   
          )
        ),
        column(6,
          div(
            dateInput('date_heatmap',
              label = 'Date input for the heatmap: yyyy-mm-dd',
              value = Sys.Date()
            ),
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center;"
          )
        ),
      ),
      fluidRow(style = "border: 1px solid #E54B4B;",
        column(12,"datatable for the heatmap",
          #create the datatable
          DT::dataTableOutput('heatmap_dt')
        )
      )
    )
  ),
  
  tabPanel(
    h4("Line Graphs"),
    fluidRow(style = "border: 1px solid #E54B4B;",
      fluidRow(style = "border: 1px solid #E54B4B;",
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
            choices = colnames(all_stations)[c(10:19,21)]
          ),
        ),
        column(5, offset = 1,
          dateRangeInput('dateRangeLineGraph',
            label = 'Date range input: yyyy-mm-dd',
            start = min(all_stations$date), end = max(all_stations$date),
            min =  min(all_stations$date), max = max(all_stations$date)
          ),
          selectizeInput(inputId = "stat_aggregation",
            label = "Statistic to be seen:",
            choices = c('Mean','Maximum', 'Minimum')
          ),
            
        )
      ),
      fluidRow(style = "border: 1px solid #E54B4B;",
        column(12,("Line Graph for Comparison"),
          plotOutput('particulatesLineGraph'),
        
        fluidRow(
          column(6, verbatimTextOutput("station_1_name"),
            DT::dataTableOutput('station_1_dt')
          ),
          column(6, verbatimTextOutput("station_2_name"),
            DT::dataTableOutput('station_2_dt')
          )
        )
        )
      )
    )
  )
)

  
  

