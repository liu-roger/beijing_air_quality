library(shiny)
library(leaflet)
library(maps)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DT)
library(readr)

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

# all_stations$dateTime = strptime(all_stations$dateTime,format("%Y-%m-%d %H:%M"))
all_stations$date = as.Date(all_stations$date,format("%Y-%m-%d"))
# view(all_stations)

all_stations = all_stations %>%
  fill(PM2.5,PM10,SO2,NO2,CO,O3,TEMP,PRES,DEWP,RAIN,wd,WSPM)
sapply(all_stations, function(x) sum(is.na(x)))



is.data.frame(all_stations)
write_csv(all_stations, "~/Documents/r_project/beijing_air_quality/all_stations.csv")
