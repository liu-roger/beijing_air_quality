#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# ______________________________________________________________________________________________________________________

#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/


# Define server logic required to draw a histogram
function(input, output, session) {
  
  heatmap_reactive_df = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$heatmap_particulate_selection) 
    selected_date = as.Date(input$heatmap_date_input)
    selected_col_percentage = as.name(paste0('percent_', tolower(input$heatmap_particulate_selection)))
    
    print(selected_col_percentage)
    print(class(selected_col))
  
    result <- all_stations %>%
      filter(date == selected_date) %>%
      select(station,date, selected_col,total_particulates, selected_col_percentage) %>%
      group_by(station) %>%
      summarise(mean_particle = round(mean(!!selected_col, na.rm = TRUE),digits = 3),
                mean_total_particulate = round(mean(total_particulates, na.rm = TRUE),digits = 3),
                mean_particulate_percentage = round(mean(!!selected_col_percentage, na.rm = TRUE),digits = 3)
                )
    
    # Debug: Show the first few rows of the result
    print(head(result))
    return(result)
  })
  
  output$heatmap_dt = DT::renderDataTable({
    datatable(heatmap_reactive_df(),
              options = list(
                lengthMenu = c(12,24)
              )
     )
  })
  
  output$beijing_map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 116.383331,lat = 40.1 ,zoom = 9) %>%
      addCircleMarkers(lng = 116.383331,lat = 39.91666, popup = paste('Beijing City, China')
                       ) %>%
      addCircleMarkers(lng = 116.3937,lat = 39.9858, popup = paste("Aotizhongxin", '<br>',
                                                                   'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Aotizhongxin', ]$mean_particle, '<br>',
                                                                   'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Aotizhongxin', ]$mean_total_particulate, '<br>',
                                                                   'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Aotizhongxin', ]$mean_particulate_percentage, '<br>'
                                                                   )
                       ) %>%
      addCircleMarkers(lng = 116.23471,lat = 40.21612 , popup = paste("Changping", '<br>',
                                                                      'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Changping', ]$mean_particle, '<br>',
                                                                      'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Changping', ]$mean_total_particulate, '<br>',
                                                                      'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Changping', ]$mean_particulate_percentage, '<br>'
                                                                      )
                       ) %>%
      addCircleMarkers(lng = 116.26667,lat = 40.26667, popup = paste("Dingling", '<br>',
                                                                     'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Dingling', ]$mean_particle, '<br>',
                                                                     'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Dingling', ]$mean_total_particulate, '<br>',
                                                                     'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Dingling', ]$mean_particulate_percentage, '<br>'
                                                                     )
                       ) %>%
      addCircleMarkers(lng = 116.4341,lat = 39.9320, popup = paste("Dongsi", '<br>',
                                                                   'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Dongsi', ]$mean_particle, '<br>',
                                                                   'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Dongsi', ]$mean_total_particulate, '<br>',
                                                                   'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Dongsi', ]$mean_particulate_percentage, '<br>'
                                                                   )
                       ) %>%
      addCircleMarkers(lng = 116.3609186,lat = 39.9353679, popup = paste("Guanyuan", '<br>',
                                                                         'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Guanyuan', ]$mean_particle, '<br>',
                                                                         'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Guanyuan', ]$mean_total_particulate, '<br>',
                                                                         'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Guanyuan', ]$mean_particulate_percentage, '<br>'
                                                                         )
                       ) %>%
      addCircleMarkers(lng = 116.179722,lat = 39.913611, popup = paste("Gucheng", '<br>',
                                                                       'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Gucheng', ]$mean_particle, '<br>',
                                                                       'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Gucheng', ]$mean_total_particulate, '<br>',
                                                                       'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Gucheng', ]$mean_particulate_percentage, '<br>'
                                                                      )
                       ) %>%
      addCircleMarkers(lng = 116.6878,lat = 40.3971, popup = paste("Huairou", '<br>',
                                                                   'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Huairou', ]$mean_particle, '<br>',
                                                                   'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Huairou', ]$mean_total_particulate, '<br>',
                                                                   'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Huairou', ]$mean_particulate_percentage, '<br>'
                                                                   )
                       ) %>%
      addCircleMarkers(lng = 116.4594991,lat = 39.9425493, popup = paste("Nongzhanguan", '<br>',
                                                                         'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Nongzhanguan', ]$mean_particle, '<br>',
                                                                         'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Nongzhanguan', ]$mean_total_particulate, '<br>',
                                                                         'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Nongzhanguan', ]$mean_particulate_percentage, '<br>'
                                                                        )
                       ) %>%
      addCircleMarkers(lng = 116.8665,lat = 40.0577, popup = paste("Shunyi", '<br>',
                                                                   'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Shunyi', ]$mean_particle, '<br>',
                                                                   'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Shunyi', ]$mean_total_particulate, '<br>',
                                                                   'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Shunyi', ]$mean_particulate_percentage, '<br>'
                                                                   )
                       ) %>%
      addCircleMarkers(lng = 116.4066,lat = 39.8822, popup = paste("Tiantan", '<br>',
                                                                   'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Tiantan', ]$mean_particle, '<br>',
                                                                   'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Tiantan', ]$mean_total_particulate, '<br>',
                                                                   'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Tiantan', ]$mean_particulate_percentage, '<br>'
                                                                   )
                       ) %>%
      addCircleMarkers(lng = 116.2576,lat = 39.9977, popup = paste("Wanliu", '<br>',
                                                                   'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Wanliu', ]$mean_particle, '<br>',
                                                                   'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Wanliu', ]$mean_total_particulate, '<br>',
                                                                   'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Wanliu', ]$mean_particulate_percentage, '<br>'
                                                                   )
                       ) %>%
      addCircleMarkers(lng = 116.352,lat = 39.878, popup = paste("Wanshouxigong", '<br>',
                                                                 'Avg',input$heatmap_particulate_selection, 'conc. :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Wanshouxigong', ]$mean_particle, '<br>',
                                                                 'Total Particulates :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Wanshouxigong', ]$mean_total_particulate, '<br>',
                                                                 'Avg Particulate Percent :', heatmap_reactive_df()[heatmap_reactive_df()$station == 'Wanshouxigong', ]$mean_particulate_percentage, '<br>'
                                                                 )
                       ) 
  }
  )
  
# ______________________________________________________________________________________________________________________
  
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
  
  output$station_1_dt_mean = DT::renderDataTable({
    DT::datatable(all_stations_reactive_mean()[all_stations_reactive_mean()$station==input$station_name1_line_graph,c(2,3)],
                  options = list(
                    lengthMenu = c(31,62)
                  ))
  })
  output$station_2_dt_mean = DT::renderDataTable({
    DT::datatable(all_stations_reactive_mean()[all_stations_reactive_mean()$station==input$station_name2_line_graph,c(2,3)],
                  options = list(
                    lengthMenu = c(31,62)
                  ))
  })
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
    print(head(result))
    return(result)
  })
  
  output$maxParticulatesLineGraph = renderPlot(
    all_stations_reactive_max() %>%
      ggplot(aes(x=date, y= max_particle)) + 
      geom_line(aes(color=station)) +
      labs(title = 'Max Metric by Day', x = "Date", y = input$line_graph_particulate_selection) +
      facet_grid(rows = vars(station))
  )
  
  output$station_1_dt_max = DT::renderDataTable({
    DT::datatable(all_stations_reactive_max()[all_stations_reactive_max()$station==input$station_name1_line_graph,c(2,3)],
                  options = list(
                    lengthMenu = c(31,62)
                    ))
  })
  output$station_2_dt_max = DT::renderDataTable({
    DT::datatable(all_stations_reactive_max()[all_stations_reactive_max()$station==input$station_name2_line_graph,c(2,3)],
                  options = list(
                    lengthMenu = c(31,62)
                    ))
  })
  
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
  
  output$station_1_dt_min = DT::renderDataTable({
    DT::datatable(all_stations_reactive_min()[all_stations_reactive_min()$station==input$station_name1_line_graph,c(2,3)],
                  options = list(
                    lengthMenu = c(31,62)
                    ))
  })
  output$station_2_dt_min = DT::renderDataTable({
    DT::datatable(all_stations_reactive_min()[all_stations_reactive_min()$station==input$station_name2_line_graph,c(2,3)],
                  options = list(
                    lengthMenu = c(31,62)
                    ))
  })
  
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
      ggplot(aes(time, mean_hourly)) + geom_line(aes(color=station)) + geom_point(aes(color=station)) +
      labs(title = 'Hourly Analysis', x = "Hour of the Day", y = input$daily_particulate_selection) 
      
    # facet_grid(rows = vars(station))
  )
  
  output$mean_daily_particulate = DT::renderDataTable({
    DT::datatable(all_stations_reactive_daily_mean(),
                  options = list(
                    lengthMenu = c(24,48,72)
                    ))
  })
  
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
      ggplot(aes(month, mean_yearly)) + geom_line(aes(color=station)) +geom_point(aes(color=station)) +
      labs(title = 'Analysis by Month', x = "Month", y = input$monthly_particulate_selection) +
      scale_x_continuous(breaks = c(seq(1, 12, by = 1))) # Include the initial value
    
    # facet_grid(rows = vars(year))
  )
  
  output$yearly_dt = DT::renderDataTable({
    DT::datatable(all_stations_reactive_yearly(),
                  options = list(
                    lengthMenu = c(12,24)
                    ))
  })
  
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
    # print(head(result))
    return(result)
  })
  
  output$month_analysis = renderPlot(
    all_stations_reactive_monthly() %>%
      ggplot(aes(day, mean_monthly)) + geom_point(aes(color=station)) + 
      labs(title = 'Analysis by Day', x = "Day of the Month", y = input$monthly_particulate_selection) +
      scale_x_continuous(breaks = c(seq(1, 31, by = 1))) # Include the initial value
    
  )
  
  output$monthly_dt = DT::renderDataTable({
    DT::datatable(all_stations_reactive_monthly(),
                  options = list(
                    lengthMenu = c(31,62)
                    ))
  })
  
# ______________________________________________________________________________________________________________________
  
  all_stations_reactive_aggregate = reactive({
    
    # Convert the input to a column name
    selected_col <- as.name(input$aggregate_particulate_selection) 
    
    result <- all_stations %>%
      group_by(year, station) %>%
      summarise(mean_aggregate = round(mean(!!selected_col, na.rm = TRUE),digits = 3)) 
    
    # Debug: Show the first few rows of the result
    # print(head(result))
    return(result)
  })
  
  output$aggregate_particulate_analysis = renderPlot(
    all_stations_reactive_aggregate() %>%
      ggplot(aes(year, mean_aggregate)) + geom_line(aes(color=station)) + geom_point(aes(color=station)) +
      labs(title = 'Analysis by Year', x = "Day of the Month", y = input$aggregate_particulate_selection) 
    
  )
  
  output$aggregate_dt = DT::renderDataTable({
    DT::datatable(all_stations_reactive_aggregate(),
                  options = list(
                    lengthMenu = c(12,24)
                    ))
  })
# ______________________________________________________________________________________________________________________
  
  
  
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
  
  output$daily_dt_name  <- renderText({
    paste('Hourly Analysis of',as.character(input$daily_particulate_selection),'on',as.character(input$daily_date_input), 'for all stations')
  })
  
  output$yearly_dt_name  <- renderText({
    paste('Monthly Analysis of',as.character(input$monthly_particulate_selection),'in',as.character(input$year_selection_monthly_analysis), 'for all stations')
  })
  
  output$monthly_dt_name  <- renderText({
    paste('Daily Analysis of', as.character(input$monthly_particulate_selection),'of',as.character(input$month_selection),'of',as.character(input$year_selection_monthly_analysis),'for all stations')
  })
  
  output$aggregate_dt_name  <- renderText({
    paste('Yearly Analysis of',as.character(input$aggregate_particulate_selection), 'for all stations from 2013 - 2017' )
  })
    
}
