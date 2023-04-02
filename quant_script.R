# Data preparation =====
# Pre-Settings =====
rm(list=ls())
setwd("~/Documents/Study/Data analysis/Quant Challange")

# Libraries =====
library(readxl)
library(tidyverse)
library(geosphere)
library(lubridate)

# Minnesota County yearly agricultural production -----
yearly_data <- read.csv("84/agri/minnesota_county_yearly_agricultural_production.csv")

corn_data <- yearly_data %>% filter(Commodity == "CORN") %>% filter(Crop == "CORN, GRAIN")
oats_data <- yearly_data %>% filter(Commodity == "OATS")
soyb_data <- yearly_data %>% filter(Commodity == "SOYBEANS")

# Minnesota County and Station location -----
stations <- read.csv("84/weather/Minnesota Station location list.csv")
counties <- read.csv("84/agri/minnesota_county_location.csv")

counties <- counties %>% dplyr::select(-capital_name)
counties$county_name <- str_replace(counties$county_name, " County", "")
counties$county_name <- toupper(counties$county_name)

get_distance <- function(county_lat, county_lon, station_lat, station_lon) {
  dist <- distGeo(c(county_lon, county_lat), c(station_lon, station_lat))
  return(dist)
}

closest_stations <- vector("list", nrow(counties))  # create a list to store the closest weather station for each county
for (i in 1:nrow(counties)) {
  county_lat <- counties[i, "county_latitude"]
  county_lon <- counties[i, "county_longitude"]
  distances <- apply(stations[, c("Latitude", "Longitude")], 1, function(x) get_distance(county_lat, county_lon, x[1], x[2]))  # calculate the distances between the county location and all weather stations
  closest_station <- which.min(distances)  # find the index of the closest weather station
  closest_stations[[i]] <- stations[closest_station, "Code"]  # store the name of the closest weather station
}

counties$closest_station <- unlist(closest_stations)
counties <- counties[,-2:-3]

rm(closest_station, closest_stations, stations, county_lat, county_lon, distances, i, get_distance)

# County weather -----
weather <- list()
for (file in counties$closest_station) {
  weather[[file]] <- read.csv(paste0("84/weather/minnesota_daily/", file, ".csv"), header = FALSE, col.names = c("date", "tavg", "tmin", "tmax", "prcp"))
}

rm(file)

# aggregate data # INNENTŐL NEM JÓ, valami nem stimmel
aggregated <- list()
for (i in 1:length(weather)) {
  weather[[i]]$date <- year(as.Date(weather[[i]]$date))
  aggregated[[i]] <- aggregate(weather[[1]][, 2:5], by = list(weather[[1]][, 1]), FUN = mean, na.rm = TRUE)
}
#  aggregated[[i]] <- aggregate(.~ year, data = weather[[i]], FUN = mean)

weather[[2]]$date <- year(as.Date(weather[[2]]$date))




