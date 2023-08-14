#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(maps)
library(dplyr)
library(tidyverse)

#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


#loading datasets and joining them
beijing = read_csv("datasets/data.csv")
aotizhongxin = read_csv("datasets/station_data/PRSA_Data_Aotizhongxin_20130301-20170228.csv")
changping = read_csv("datasets/station_data/PRSA_Data_Changping_20130301-20170228.csv")
dingling = read_csv("datasets/station_data/PRSA_Data_Dingling_20130301-20170228.csv")
dongsi = read_csv("datasets/station_data/PRSA_Data_Dongsi_20130301-20170228.csv")
guanyuan = read_csv("datasets/station_data/PRSA_Data_Guanyuan_20130301-20170228.csv")
gucheng = read_csv("datasets/station_data/PRSA_Data_Gucheng_20130301-20170228.csv")
huairou = read_csv("datasets/station_data/PRSA_Data_Huairou_20130301-20170228.csv")
nongzhanguan = read_csv("datasets/station_data/PRSA_Data_Nongzhanguan_20130301-20170228.csv")
shunyi = read_csv("datasets/station_data/PRSA_Data_Shunyi_20130301-20170228.csv")
tiantan = read_csv("datasets/station_data/PRSA_Data_Tiantan_20130301-20170228.csv")
wanliu = read_csv("datasets/station_data/PRSA_Data_Wanliu_20130301-20170228.csv")
wanshouxigong = read_csv("datasets/station_data/PRSA_Data_Wanshouxigong_20130301-20170228.csv")
# view(beijing)

all_stations = rbind(
  aotizhongxin,
  changping,
  dingling,
  dongsi,
  guanyuan,
  gucheng,
  huairou,
  nongzhanguan,
  shunyi,
  tiantan,
  wanliu,
  wanshouxigong
  )
c()
all_stations = all_stations %>%
  unite("date",year:day,sep = '-')
view(all_stations)
length(all_stations$hour)
all_stations$minute = vector('numeric',length(all_stations$hour))

# all_stations$minute = 
# Define UI for application that draws a histogram
navbarPage(
  h4("Beijing Air Quality"),
  # img(src='img/beijing_icon.jpeg', width='50%'),
  tabPanel(
    h4("Heat Map"),
    fluidRow(style = "border: 4px double red;",
      column(12,
        'centered beijing icon',
        img(src='img/beijing_icon.jpeg', width='50%'),
        
        fluidRow(style = "border: 4px double red;",
          column(6,
            'Particulate Selection',
            selectizeInput(inputId = "particulate_selection_heatmap",
              label = "Particulate Selection",
              choices = colnames(all_stations)[c(4:13)]
            )
          ),
          column(6,
            'Date Range Input',
            dateInput('date',
              label = 'Date input: yyyy-mm-dd',
              value = Sys.Date()
            )
          ),
        )
      ),
      fluidRow(style = "border: 4px double red;",
        column(12,
          'HEAT MAP',
          dateInput('date',
            label = 'Date input: yyyy-mm-dd',
            value = Sys.Date()
          )
        )
      ),
      fluidRow(style = "border: 4px double red;",
        column(12,
          'DATASET DATATABLE',
          dateInput('date',
            label = 'Date input: yyyy-mm-dd',
            value = Sys.Date()
          )
        )
      )
    )
  ),
  
  tabPanel(
    h4("Line Graphs"),
    fluidRow(style = "border: 4px double red;",
      column(12,"centered beijing icon",
        img(src='img/beijing_icon.jpeg', width='50%'),
        fluidRow(style = "border: 4px double red;",
          column(6,
            selectizeInput(inputId = "station_name1_line_graph",
              label = "First Station Name",
              choices = unique(all_stations$station)
            ),
            selectizeInput(inputId = "station_name2_line_graph",
              label = "Second Station Name",
              choices = unique(all_stations$station)
            )
          ),
          column(6, "this is where the time slider is displayed",
            dateRangeInput('dateRangeLineGraph',
            label = 'Date range input: yyyy-mm-dd',
            start = Sys.Date() - 2, end = Sys.Date() + 2
            ),
          )
        ),
        fluidRow(style = "border: 4px double red;",
          column(12,"this is where the line graph is displayed",
            checkboxInput('jitter', 'Jitter'),
            checkboxInput('smooth', 'Smooth'),
          
            fluidRow(
              column(6, 'this is where the table1 is displayed',
                checkboxInput('jitter', 'Jitter'),
                checkboxInput('smooth', 'Smooth')
              ),
              column(6, 'this is where the table2 is displayed',
                checkboxInput('jitter', 'Jitter'),
                checkboxInput('smooth', 'Smooth')
              )
            )
          )
        )
      )
    )
  )
)
  
  

