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
library(openair)
library(plotly)
# ______________________________________________________________________________________________________________________

#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


# Define server logic required to draw a histogram
function(input, output, session) {
  
  all_stations_reactive_mean = reactive({
    
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
  
  output$meanParticulatesLineGraph = renderPlot(
    all_stations_reactive_mean() %>%
      ggplot(aes(x=date, y= mean_particle)) + 
      geom_line(aes(color=station)) +
      labs(title = 'Mean Metric by Day', x = "Date", y = input$line_graph_particulate_selection) +
      facet_grid(rows = vars(station))
  )
# ______________________________________________________________________________________________________________________

  all_stations_reactive_max = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$line_graph_particulate_selection) 
    start_date = as.Date(input$dateRangeLineGraph[1])
    end_date = as.Date(input$dateRangeLineGraph[2])
    
    result <- all_stations %>%
      filter((station == input$station_name1_line_graph | station == input$station_name2_line_graph) & (date >= start_date & date <= end_date)) %>%
      group_by(station, date) %>%
      summarise(max_particle = round(max(!!selected_col, na.rm = TRUE),digits = 3)) # Use !! to unquote the variable name
    
    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$maxParticulatesLineGraph = renderPlot(
    all_stations_reactive_max() %>%
      ggplot(aes(x=date, y= max_particle)) + 
      geom_line(aes(color=station)) +
      labs(title = 'Max Metric by Day', x = "Date", y = input$line_graph_particulate_selection) +
      facet_grid(rows = vars(station))
  )
  
# ______________________________________________________________________________________________________________________
  
  all_stations_reactive_min = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$line_graph_particulate_selection) 
    start_date = as.Date(input$dateRangeLineGraph[1])
    end_date = as.Date(input$dateRangeLineGraph[2])
    
    result <- all_stations %>%
      filter((station == input$station_name1_line_graph | station == input$station_name2_line_graph) & (date >= start_date & date <= end_date)) %>%
      group_by(station, date) %>%
      summarise(min_particle = round(min(!!selected_col, na.rm = TRUE),digits = 3)) # Use !! to unquote the variable name
    
    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$minParticulatesLineGraph = renderPlot(
    all_stations_reactive_min() %>%
      ggplot(aes(x=date, y= min_particle)) + 
      geom_line(aes(color=station)) +
      labs(title = 'Min Metric by Day', x = "Date", y = input$line_graph_particulate_selection) +
      facet_grid(rows = vars(station))
  )
  
# ______________________________________________________________________________________________________________________
  
  all_stations_reactive_daily_mean = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$daily_particulate_selection) 
    selected_date = as.Date(input$daily_date_input)
    
    result <- all_stations %>%
      filter((date == selected_date)) %>%
      group_by(station, time) %>%
      summarise(mean_hourly = round(mean(!!selected_col, na.rm = TRUE),digits = 3)) # Use !! to unquote the variable name
    
    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$daily_particulate_analysis = renderPlot(
    all_stations_reactive_daily_mean() %>%
      ggplot(aes(time, mean_hourly)) + geom_line(aes(color=station)) + geom_point(aes(color=station))
    # facet_grid(rows = vars(station))
  )
  
# ______________________________________________________________________________________________________________________
  
  all_stations_reactive_yearly = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$monthly_particulate_selection) 
    
    result <- all_stations %>%
      filter(year == input$year_selection_monthly_analysis) %>%
      group_by(station, month, year) %>%
      summarise(mean_yearly = round(mean(!!selected_col, na.rm = TRUE),digits = 3)) 

    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$yearly_particulate_analysis = renderPlot(
    all_stations_reactive_yearly() %>%
      ggplot(aes(month, mean_yearly)) + geom_line(aes(color=station)) +geom_point(aes(color=station)) 
    # facet_grid(rows = vars(year))
  )
  
# ______________________________________________________________________________________________________________________
  
  all_stations_reactive_monthly = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$monthly_particulate_selection) 
    # selected_month = as.numeric(input$month_selection)
    selected_month = switch(input$month_selection,
                            'January' = 1,
                            'February' = 2,
                            'March' = 3,
                            'April' = 4,
                            'May' = 5,
                            'June' = 6,
                            'July' = 7,
                            'August' = 8,
                            'September' = 9,
                            'October' = 10,
                            'November' = 11,
                            'December' = 12)
    
    result <- all_stations %>%
      filter((year == input$year_selection_monthly_analysis) & (month == selected_month)) %>%
      group_by(station, day) %>%
      summarise(mean_monthly = round(mean(!!selected_col, na.rm = TRUE),digits = 3)) 
    
    # Debug: Show the first few rows of the result
    print(head(result))
    return(result)
  })
  
  output$month_analysis = renderPlot(
    all_stations_reactive_monthly() %>%
      ggplot(aes(day, mean_monthly)) + geom_point(aes(color=station)) #+ geom_point(aes(color=station))
    # facet_grid(rows = vars(year))
  )
  
# ______________________________________________________________________________________________________________________
  
  all_stations_reactive_aggregate = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$yearly_particulate_selection) 
    
    result <- all_stations %>%
      group_by(year, station) %>%
      summarise(mean_aggregate = round(mean(!!selected_col, na.rm = TRUE),digits = 3)) 
    
    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$aggregate_particulate_analysis = renderPlot(
    all_stations_reactive_aggregate() %>%
      ggplot(aes(year, mean_aggregate)) + geom_line(aes(color=station)) + geom_point(aes(color=station))
  )
# ______________________________________________________________________________________________________________________
  
  
  output$heatmap_dt = DT::renderDataTable({
    all_stations[c(1,2,10:19,20,21)]
  })
  
  output$station_1_dt_mean = DT::renderDataTable({
    DT::datatable(all_stations_reactive_mean()[all_stations_reactive_mean()$station==input$station_name1_line_graph,c(2,3)])
  })
  output$station_2_dt_mean = DT::renderDataTable({
    DT::datatable(all_stations_reactive_mean()[all_stations_reactive_mean()$station==input$station_name2_line_graph,c(2,3)])
  })
  
  
  
  output$station_1_dt_max = DT::renderDataTable({
    DT::datatable(all_stations_reactive_max()[all_stations_reactive_max()$station==input$station_name1_line_graph,c(2,3)])
  })
  output$station_2_dt_max = DT::renderDataTable({
    DT::datatable(all_stations_reactive_max()[all_stations_reactive_max()$station==input$station_name2_line_graph,c(2,3)])
  })
  
  
  
  output$station_1_dt_min = DT::renderDataTable({
    DT::datatable(all_stations_reactive_min()[all_stations_reactive_min()$station==input$station_name1_line_graph,c(2,3)])
  })
  output$station_2_dt_min = DT::renderDataTable({
    DT::datatable(all_stations_reactive_min()[all_stations_reactive_min()$station==input$station_name2_line_graph,c(2,3)])
  })
  
  
  output$mean_daily_particulate = DT::renderDataTable({
    DT::datatable(all_stations_reactive_daily_mean())
  })
  
  
  
  
  output$beijing_map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 116.383331,lat = 39.916668 ,zoom = 9) %>%
      addCircleMarkers(lng = 116.383331,lat = 39.91666, popup = "Beijing City, China") %>%
      addCircleMarkers(lng = 116.3937,lat = 39.9858, popup = "Aoti ZhongXin") %>%
      addCircleMarkers(lng = 116.23471,lat = 40.21612 , popup = "Changping") %>%
      addCircleMarkers(lng = 116.26667,lat = 40.26667, popup = "Dingling") %>%
      addCircleMarkers(lng = 116.4341,lat = 39.9320, popup = "Dongsi") %>%
      addCircleMarkers(lng = 116.3609186,lat = 39.9353679, popup = "Guanyuan") %>%
      addCircleMarkers(lng = 116.179722,lat = 39.913611, popup = "Gucheng") %>%
      addCircleMarkers(lng = 116.6878,lat = 40.3971, popup = "Huairou") %>%
      addCircleMarkers(lng = 116.4594991,lat = 39.9425493, popup = "Nongzhanguan") %>%
      addCircleMarkers(lng = 116.8665,lat = 40.0577, popup = "Shunyi") %>%
      addCircleMarkers(lng = 116.4066,lat = 39.8822, popup = "Tiantan") %>%
      addCircleMarkers(lng = 116.2576,lat = 39.9977, popup = "Wanliu") %>%
      addCircleMarkers(lng = 116.352,lat = 39.878, popup = "Wanshouxigong") 
  }
  )
  
  
  output$station_1_name_mean  <- renderText({
    paste("Station Name:", as.character(input$station_name1_line_graph), 'Daily Average',as.character(input$line_graph_particulate_selection))
  })
  output$station_2_name_mean  <- renderText({
    paste("Station Name:", as.character(input$station_name2_line_graph), 'Daily Average',as.character(input$line_graph_particulate_selection))
  })
  
  
  output$station_1_name_max  <- renderText({
    paste("Station Name:", as.character(input$station_name1_line_graph), 'Daily Max',as.character(input$line_graph_particulate_selection))
  })
  output$station_2_name_max  <- renderText({
    paste("Station Name:", as.character(input$station_name2_line_graph), 'Daily Max',as.character(input$line_graph_particulate_selection))
    
  })
  
  
  output$station_1_name_min  <- renderText({
    paste("Station Name:", as.character(input$station_name1_line_graph), 'Daily Min',as.character(input$line_graph_particulate_selection))
  })
  output$station_2_name_min  <- renderText({
    paste("Station Name:", as.character(input$station_name2_line_graph), 'Daily Min',as.character(input$line_graph_particulate_selection))
    
  })
    
}
