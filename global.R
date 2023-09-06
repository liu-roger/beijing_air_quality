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
all_stations = read_csv('./datasets/all_stations.csv')
all_stations_with_aqi = read_csv('./datasets/all_stations_with_aqi.csv')
