library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(RSQLite)
library(rsconnect)


#data source
# https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data
# https://www.apple.com/startpage/
all_stations = read_csv('./datasets/all_stations.csv')