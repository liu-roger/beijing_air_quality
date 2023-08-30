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
library(patchwork)

#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


# Define server logic required to draw a histogram
function(input, output, session) {
  
  all_stations_reactive = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$line_graph_particulate_selection) 
    start_date = as.Date(input$dateRangeLineGraph[1])
    end_date = as.Date(input$dateRangeLineGraph[2])
    
    result <- all_stations %>%
      filter((station == input$station_name1_line_graph | station == input$station_name2_line_graph) & (date >= start_date & date <= end_date)) %>%
      group_by(station, date) %>%
      summarise(mean_particle = round(mean(!!selected_col, na.rm = TRUE),digits = 3)) # Use !! to unquote the variable name
    
    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$heatmap_dt = DT::renderDataTable({
    all_stations[c(1,2,10:19,20,21)]
  })
  output$station_1_dt = DT::renderDataTable({
    DT::datatable(all_stations_reactive()[all_stations_reactive()$station==input$station_name1_line_graph,c(2,3)])
    
  })
  output$station_2_dt = DT::renderDataTable({
    DT::datatable(all_stations_reactive()[all_stations_reactive()$station==input$station_name2_line_graph,c(2,3)])
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
  # all_stations_reactive = reactive({
  #   all_stations %>%
  #     select(station,input$line_graph_particulate_selection)
  #     filter(station == input$station_name1_line_graph) %>%
  #     group_by(date) %>%
  #     summarise(mean_particle = mean(input$line_graph_particulate_selection))
  # })
  # 
  
  
  
  output$particulatesLineGraph = renderPlot(
    all_stations_reactive() %>%
      ggplot(aes(x=date, y= mean_particle)) + 
        geom_line(aes(color=station)) +
        labs(x = "Date", y = input$line_graph_particulate_selection) +
        facet_grid(rows = vars(station))
  )
  
  output$station_1_name  <- renderText({
    paste("Station Name:", as.character(input$station_name1_line_graph), '- Average',as.character(input$line_graph_particulate_selection))
  })
  
  output$station_2_name  <- renderText({
    paste("Station Name:", as.character(input$station_name2_line_graph), '- Average',as.character(input$line_graph_particulate_selection))
    
  })
    
}
