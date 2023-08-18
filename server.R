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
all_stations = all_stations %>%
  unite("date",year:day,remove=FALSE, sep = '-')
# view(all_stations)
# length(all_stations$hour)
all_stations$minute = rep('00',times = length(all_stations$hour))
all_stations = all_stations[,c("station","date","year","month",'day', "hour","minute","PM2.5","PM10","SO2","NO2","CO","O3","TEMP","PRES","DEWP","RAIN","wd","WSPM")]
all_stations = all_stations %>%
  unite("time",c(hour,minute),remove=FALSE, sep = ':')

all_stations = all_stations %>%
  unite("dateTime",c(date,time),remove = FALSE, sep = ' ')

#all_stations$dateTime = strptime(all_stations$dateTime,format("%Y-%m-%d %H:%M"))
#all_stations$date = strptime(all_stations$date,format("%Y-%m-%d"))
# view(all_stations)

beijing_map = leaflet() %>%
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



# Define server logic required to draw a histogram
function(input, output, session) {
  output$heatmap_dt = DT::renderDataTable({
    all_stations[c(1,2,10:19,20,21)]
  })
  output$station_1_dt = DT::renderDataTable({
    # all_stations = all_stations[c(1)]
    DT::datatable(all_stations[, input$line_graph_particulate_selection, drop = FALSE])
    #all_stations[2,input$line_graph_particulate_selection]
  })
  output$station_2_dt = DT::renderDataTable({
    DT::datatable(all_stations[, input$line_graph_particulate_selection, drop = FALSE])
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
  # output$particulatesLineGraph = renderPlot(
  #   all_stations %>%
  #     filter(station = input$station_name1_line_graph) %>%
  #     ggplot(aes(x=date, y=input$line_graph_particulate_selection))
  # )
    
}
