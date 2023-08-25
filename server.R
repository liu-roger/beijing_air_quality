#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(ggplot2)
library(DT)

#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


#loading datasets and joining them
all_stations = read_csv('all_stations.csv')


# Define server logic required to draw a histogram
function(input, output, session) {
  output$heatmap_dt = DT::renderDataTable({
    all_stations[c(1,2,10:19,20,21)]
  })
  output$station_1_dt = DT::renderDataTable({
    DT::datatable(all_stations[all_stations$station == input$station_name1_line_graph, input$show_vars, drop = FALSE])
  })
  output$station_2_dt = DT::renderDataTable({
    DT::datatable(all_stations[all_stations$station == input$station_name2_line_graph, input$show_vars, drop = FALSE])
  })
  
  output$beijing_map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 116.383331,lat = 39.916668 ,zoom = 9) %>%
      addMarkers(lng = 116.383331,lat = 39.91666, popup = "Beijing City, China") %>%
      addMarkers(lng = 116.3937,lat = 39.9858, popup = "Aoti ZhongXin") %>%
      addMarkers(lng = 116.23471,lat = 40.21612 , popup = "Changping") %>%
      addMarkers(lng = 116.26667,lat = 40.26667, popup = "Dingling") %>%
      addMarkers(lng = 116.4341,lat = 39.9320, popup = "Dongsi") %>%
      addMarkers(lng = 116.3609186,lat = 39.9353679, popup = "Guanyuan") %>%
      addMarkers(lng = 116.179722,lat = 39.913611, popup = "Gucheng") %>%
      addMarkers(lng = 116.6878,lat = 40.3971, popup = "Huairou") %>%
      addMarkers(lng = 116.4594991,lat = 39.9425493, popup = "Nongzhanguan") %>%
      addMarkers(lng = 116.8665,lat = 40.0577, popup = "Shunyi") %>%
      addMarkers(lng = 116.4066,lat = 39.8822, popup = "Tiantan") %>%
      addMarkers(lng = 116.2576,lat = 39.9977, popup = "Wanliu") %>%
      addMarkers(lng = 116.4066,lat = 39.8822, popup = "Wanshouxigong") 
  }
  )
  output$particulatesLineGraph = renderPlot(
    all_stations %>%
      filter(station == input$station_name1_line_graph) %>%
      group_by(date) %>%
      summarise(mean_particle = mean(input$line_graph_particulate_selection)) %>%
      ggplot(aes(x=date, y=mean_particle)) + geom_line()
  )
  output$station_1_name  <- renderText({
    paste("Station Name:", as.character(input$station_name1_line_graph))
  })
  
  output$station_2_name  <- renderText({
    paste("Station Name:", as.character(input$station_name2_line_graph))
  })
    
}
