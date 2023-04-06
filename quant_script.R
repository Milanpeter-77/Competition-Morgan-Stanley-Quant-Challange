# ================================ #
# Quant Challenge - Qualifier task #
# ================================ #

# I.Pre-Settings =====
rm(list=ls()) #clearing environment
setwd("~/Documents/Study/Data analysis/Quant Challange") #setting working directory
theme_colors <- c("#2f5496","#0e98a7", "#5fcdd9", "#E3CD30")

# II.Libraries =====
library(readxl)
library(tidyverse)
library(geosphere)
library(maps)
library(mapdata)
library(lubridate)
library(caret)

# III.Reading Minnesota County yearly agricultural production =====
yearly_data <- read.csv("84/agri/minnesota_county_yearly_agricultural_production.csv")
colnames(yearly_data) <- c("Year", "County", "Commodity", "Crop", "Harvested", "Production", "Yield")
yearly_data$Yield <- as.numeric(yearly_data$Yield) #will produce NAs

# IV.Minnesota County and Station location =====
stations <- read.csv("84/weather/Minnesota Station location list.csv")
counties <- read.csv("84/agri/minnesota_county_location.csv")

counties <- counties %>% dplyr::select(-capital_name)
counties$county_name <- str_replace(counties$county_name, " County", "")
counties$county_name <- toupper(counties$county_name)

get_distance <- function(county_lat, county_lon, station_lat, station_lon) {
  dist <- distGeo(c(county_lon, county_lat), c(station_lon, station_lat))
  return(dist)
}

closest_stations <- vector("list", nrow(counties)) #create a list to store the closest weather station for each county
for (i in 1:nrow(counties)) {
  county_lat <- counties[i, "county_latitude"]
  county_lon <- counties[i, "county_longitude"]
  distances <- apply(stations[, c("Latitude", "Longitude")], 1, function(x) get_distance(county_lat, county_lon, x[1], x[2])) # calculate the distances between the county location and all weather stations
  closest_station <- which.min(distances) #find the index of the closest weather station
  closest_stations[[i]] <- stations[closest_station, "Code"] #store the name of the closest weather station
}

counties$closest_station <- unlist(closest_stations)

#plotting the connection between stations and counties
county_map <- map_data(map = "county", region = "minnesota")

county_station <- data.frame(county_name = counties$county_name,
                             Code = counties$closest_station)

county_station_coords <- merge(counties, county_station, by = "county_name")
county_station_coords <- merge(county_station_coords, stations, by = "Code")

#plot the world map and add the coordinates as points (9.5x9.5 inch)
county_plot <- ggplot() +
  geom_polygon(data = county_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey80") + 
  geom_point(data = counties, aes(x = county_longitude, y = county_latitude, color = "Counties"), size = 3)

map_plot <- county_plot +
  geom_point(data = stations, aes(x = Longitude, y = Latitude, color = "Weather stations"), size = 3) +
  geom_segment(data = county_station_coords,
               aes(x = county_longitude, y = county_latitude, xend = Longitude, yend = Latitude, color = "Closest station"),
               size = 1.1, lineend = "round") +
  scale_color_manual(name = "Locations", values = c("Counties" = theme_colors[2], "Weather stations" = theme_colors[1], "Closest station" = theme_colors[3])) +
  theme(panel.background = element_rect(fill = "white"))

map_plot

#binding station and location data
yearly_data$Station <- counties$closest_station[match(yearly_data$County, counties$county_name)]
yearly_data$Latitude <- counties$county_latitude[match(yearly_data$County, counties$county_name)]
yearly_data$Longitude <- counties$county_longitude[match(yearly_data$County, counties$county_name)]

rm(closest_station, closest_stations, stations, county_lat, county_lon, distances, i, get_distance, county_map, county_plot, county_station, county_station_coords, map_plot) #cleaning temporary values

# V.County weather =====
weather <- list()
for (file in counties$closest_station) {
  weather[[file]] <- read.csv(paste0("84/weather/minnesota_daily/", file, ".csv"), header = FALSE, col.names = c("date", "tavg", "tmin", "tmax", "prcp"))
  weather[[file]]$date <- as.Date(weather[[file]]$date)
}

#aggregate data
aggregated <- list()
for (file in counties$closest_station) {
  aggregated[[file]][1:2] <- aggregate(tavg ~ format(date, "%Y"), weather[[file]], mean)
  aggregated[[file]][3:4] <- aggregate(tmin ~ format(date, "%Y"), weather[[file]], mean)
  aggregated[[file]][5:6] <- aggregate(tmax ~ format(date, "%Y"), weather[[file]], mean)
  aggregated[[file]][7:8] <- aggregate(prcp ~ format(date, "%Y"), weather[[file]], sum)
  aggregated[[file]][9:10] <- aggregate(prcp ~ format(date, "%Y"), weather[[file]], length)
  names(aggregated[[file]]) <- c("tavg_date", "tavg", "tmin_date", "tmin", "tmax_date", "tmax", "prcp_date", "prcp", "prcp_obs_date", "prcp_obs")
}

# VI.Crops filtering =====
sample_size <- 180 # if prcp_obs > NA in a year
#Corn
corn_data <- yearly_data %>% dplyr::filter(Commodity == "CORN" & Crop == "CORN, GRAIN" & !is.na(Station) & !is.na(Yield))
for (i in 1:nrow(corn_data)) {
  corn_data$tavg[i] <- aggregated[[corn_data$Station[i]]]$tavg[match(corn_data$Year[i], aggregated[[corn_data$Station[i]]]$tavg_date)]
  corn_data$tmin[i] <- aggregated[[corn_data$Station[i]]]$tmin[match(corn_data$Year[i], aggregated[[corn_data$Station[i]]]$tmin_date)]
  corn_data$tmax[i] <- aggregated[[corn_data$Station[i]]]$tmax[match(corn_data$Year[i], aggregated[[corn_data$Station[i]]]$tmax_date)]
  corn_data$prcp[i] <- NA
  if (!is.na(aggregated[[corn_data$Station[i]]]$prcp_obs[match(corn_data$Year[i], aggregated[[corn_data$Station[i]]]$prcp_obs_date)]) &
      aggregated[[corn_data$Station[i]]]$prcp_obs[match(corn_data$Year[i], aggregated[[corn_data$Station[i]]]$prcp_obs_date)] > sample_size) {
    corn_data$prcp[i] <- aggregated[[corn_data$Station[i]]]$prcp[match(corn_data$Year[i], aggregated[[corn_data$Station[i]]]$prcp_date)]
  }
}

#Oats
oats_data <- yearly_data %>% dplyr::filter(Commodity == "OATS" & !is.na(Station) & !is.na(Yield))
for (i in 1:nrow(oats_data)) {
  oats_data$tavg[i] <- aggregated[[oats_data$Station[i]]]$tavg[match(oats_data$Year[i], aggregated[[oats_data$Station[i]]]$tavg_date)]
  oats_data$tmin[i] <- aggregated[[oats_data$Station[i]]]$tmin[match(oats_data$Year[i], aggregated[[oats_data$Station[i]]]$tmin_date)]
  oats_data$tmax[i] <- aggregated[[oats_data$Station[i]]]$tmax[match(oats_data$Year[i], aggregated[[oats_data$Station[i]]]$tmax_date)]
  oats_data$prcp[i] <- NA
  if (!is.na(aggregated[[oats_data$Station[i]]]$prcp_obs[match(oats_data$Year[i], aggregated[[oats_data$Station[i]]]$prcp_obs_date)]) &
      aggregated[[oats_data$Station[i]]]$prcp_obs[match(oats_data$Year[i], aggregated[[oats_data$Station[i]]]$prcp_obs_date)] > sample_size) {
    oats_data$prcp[i] <- aggregated[[oats_data$Station[i]]]$prcp[match(oats_data$Year[i], aggregated[[oats_data$Station[i]]]$prcp_date)]
  }
}

#Soybean
soyb_data <- yearly_data %>% dplyr::filter(Commodity == "SOYBEANS" & !is.na(Station) & !is.na(Yield))
for (i in 1:nrow(soyb_data)) {
  soyb_data$tavg[i] <- aggregated[[soyb_data$Station[i]]]$tavg[match(soyb_data$Year[i], aggregated[[soyb_data$Station[i]]]$tavg_date)]
  soyb_data$tmin[i] <- aggregated[[soyb_data$Station[i]]]$tmin[match(soyb_data$Year[i], aggregated[[soyb_data$Station[i]]]$tmin_date)]
  soyb_data$tmax[i] <- aggregated[[soyb_data$Station[i]]]$tmax[match(soyb_data$Year[i], aggregated[[soyb_data$Station[i]]]$tmax_date)]
  soyb_data$prcp[i] <- NA
  if (!is.na(aggregated[[soyb_data$Station[i]]]$prcp_obs[match(soyb_data$Year[i], aggregated[[soyb_data$Station[i]]]$prcp_obs_date)]) &
      aggregated[[soyb_data$Station[i]]]$prcp_obs[match(soyb_data$Year[i], aggregated[[soyb_data$Station[i]]]$prcp_obs_date)] > sample_size) {
    soyb_data$prcp[i] <- aggregated[[soyb_data$Station[i]]]$prcp[match(soyb_data$Year[i], aggregated[[soyb_data$Station[i]]]$prcp_date)]
  }
}

rm(file, i, sample_size) # cleaning temporary values
rm(yearly_data, weather, aggregated, counties) # cleaning previous data files

# VII.Models =====
## CORN -----
corn_data <- na.omit(corn_data)
corn_data$lnYield <- log(corn_data$Yield)

M1 <- lm(Yield ~ tavg + tmin + tmax + prcp, data = corn_data)
summary(M1)
M2 <- lm(Yield ~ Year + tavg + tmin + tmax + prcp, data = corn_data)
summary(M2)
M3 <- lm(Yield ~ Year + tavg + tmin + tmax + I(prcp^2), data = corn_data)
summary(M3)
M4 <- lm(Yield ~ Year + tavg + tmin + tmax + ifelse(log(prcp) >= 0, log(prcp), 0), data = corn_data)
summary(M4)
M5 <- lm(Yield ~ Year + tavg + tmin + tmax + prcp + Latitude + Longitude, data = corn_data)
summary(M5)
M6 <- lm(lnYield ~ Year + tavg + tmin + tmax + prcp, data = corn_data)
summary(M6)
M7 <- lm(lnYield ~ Year + tavg + tmin + tmax + log(prcp), data = corn_data)
summary(M7)

# CROSS-VALIDATION
# Creating test set
# Creating work and test sets
smp_size <- floor(0.2*nrow(corn_data))

set.seed(1)
test_ids <- sample(seq_len(nrow(corn_data)), size = smp_size)
corn_data$test <- 0
corn_data$test[test_ids] <- 1

corn_test <- corn_data %>% filter(corn_data$test == 1)
corn_work <- corn_data %>% filter(corn_data$test == 0)

# CV
# fold num
k <- 5

set.seed(1)
cv1 <- train(as.formula(M1), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv2 <- train(as.formula(M2), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv3 <- train(as.formula(M3), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv4 <- train(as.formula(M4), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv5 <- train(as.formula(M5), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv6 <- train(as.formula(M6), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv7 <- train(as.formula(M7), corn_work, method = "lm", trControl = trainControl(method = "cv", number = k))

# calculate average RMSE
cv <- c("cv1","cv2","cv3", "cv4", "cv5", "cv6", "cv7")
rmse_cv <- c()

for(i in 1:length(cv)) {
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2 +
                        get(cv[i])$resample[[1]][5]^2)/5)
}

# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4]),
                     rbind(cv5$resample[1], rmse_cv[5]),
                     rbind(cv6$resample[1], rmse_cv[6]),
                     rbind(cv7$resample[1], rmse_cv[7]))

colnames(cv_mat) <- c("Resample", "Model1","Model2","Model3", "Model4", "Model5", "Model6", "Model7")
cv_mat

# test set predictions
#create formula list
models <- list(M1, M2, M3, M4, M5, M6, M7)
rmse_test <- c()
pred <- data.frame(matrix(0, nrow = nrow(corn_test), ncol = length(models)))

for (i in 1:length(models)) {
  pred[,i] <- predict(models[[i]], newdata = corn_test)
  
  if (models[[i]]$call$formula[2] == "lnYield()") {
    lev <- exp(pred[,i]) * exp((cv_mat[5, 1 + i]^2)/2)
    rmse_test[i] <- RMSE(lev, corn_test$Yield)
  } else {
    rmse_test[i] <- RMSE(pred[,i], corn_test$Yield)
  }
}
model_names <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

test_mat <- cbind(model_names, rmse_test)
test_mat

# yhat plot (6x6 inch) for CORN
pred_data <- data.frame(corn_data$Yield, predict(M4, corn_data))
colnames(pred_data) <- c("yield", "prediction")
ggplot(data = pred_data) +
  geom_point(aes(x = yield, y = prediction), color = theme_colors[1], size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = max(yield), yend = max(yield)), size = 1.1, color = theme_colors[4], linetype = 2) +
  labs(x = "Actual Corn Yield",
       y = "Predicted Corn Yield by Model 4",
       title = "Predicted Corn Yield vs. Actual Corn Yield") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 8),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "grey70"),
        panel.background = element_rect(fill = "white", color = "white"))

rm(corn_test, corn_work, cv_mat, cv1, cv2, cv3, cv4, cv5, cv6, cv7, models, pred, pred_data, test_mat, cv, i, k, lev, model_names, rmse_cv, rmse_test, smp_size, test_ids, theme_colors)

## OATS -----
oats_data <- na.omit(oats_data)
oats_data$lnYield <- log(oats_data$Yield)

O_M1 <- lm(Yield ~ tavg + tmin + tmax + prcp, data = oats_data)
summary(O_M1)
O_M2 <- lm(Yield ~ Year + tavg + tmin + tmax + prcp, data = oats_data)
summary(O_M2)
O_M3 <- lm(Yield ~ Year + tavg + tmin + tmax + I(prcp^2), data = oats_data)
summary(O_M3)
O_M4 <- lm(Yield ~ Year + tavg + tmin + tmax + ifelse(log(prcp) >= 0, log(prcp), 0), data = oats_data)
summary(O_M4)
O_M5 <- lm(Yield ~ Year + tavg + tmin + tmax + prcp + Latitude + Longitude, data = oats_data)
summary(O_M5)
O_M6 <- lm(lnYield ~ Year + tavg + tmin + tmax + prcp, data = oats_data)
summary(O_M6)
O_M7 <- lm(lnYield ~ Year + tavg + tmin + tmax + log(prcp), data = oats_data)
summary(O_M7)

# CROSS-VALIDATION
# Creating test set
# Creating work and test sets
smp_size <- floor(0.2*nrow(oats_data))

set.seed(1)
test_ids <- sample(seq_len(nrow(oats_data)), size = smp_size)
oats_data$test <- 0
oats_data$test[test_ids] <- 1

oats_test <- oats_data %>% filter(oats_data$test == 1)
oats_work <- oats_data %>% filter(oats_data$test == 0)

# CV
# fold num
k <- 5

set.seed(1)
cv1 <- train(as.formula(O_M1), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv2 <- train(as.formula(O_M2), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv3 <- train(as.formula(O_M3), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv4 <- train(as.formula(O_M4), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv5 <- train(as.formula(O_M5), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv6 <- train(as.formula(O_M6), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv7 <- train(as.formula(O_M7), oats_work, method = "lm", trControl = trainControl(method = "cv", number = k))

# calculate average RMSE
cv <- c("cv1","cv2","cv3", "cv4", "cv5", "cv6", "cv7")
rmse_cv <- c()

for(i in 1:length(cv)) {
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2 +
                        get(cv[i])$resample[[1]][5]^2)/5)
}

# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4]),
                     rbind(cv5$resample[1], rmse_cv[5]),
                     rbind(cv6$resample[1], rmse_cv[6]),
                     rbind(cv7$resample[1], rmse_cv[7]))

colnames(cv_mat) <- c("Resample", "Model1","Model2","Model3", "Model4", "Model5", "Model6", "Model7")
cv_mat

# test set predictions
#create formula list
models <- list(O_M1, O_M2, O_M3, O_M4, O_M5, O_M6, O_M7)
rmse_test <- c()
pred <- data.frame(matrix(0, nrow = nrow(oats_test), ncol = length(models)))

for (i in 1:length(models)) {
  pred[,i] <- predict(models[[i]], newdata = oats_test)
  
  if (models[[i]]$call$formula[2] == "lnYield()") {
    lev <- exp(pred[,i]) * exp((cv_mat[5, 1 + i]^2)/2)
    rmse_test[i] <- RMSE(lev, oats_test$Yield)
  } else {
    rmse_test[i] <- RMSE(pred[,i], oats_test$Yield)
  }
}
model_names <- c("O_M1", "O_M2", "O_M3", "O_M4", "O_5", "O_M6", "O_M7")

test_mat <- cbind(model_names, rmse_test)
test_mat

rm(oats_test, oats_work, cv_mat, cv1, cv2, cv3, cv4, cv5, cv6, cv7, models, pred, pred_data, test_mat, cv, i, k, lev, model_names, rmse_cv, rmse_test, smp_size, test_ids)

## SOYBEANS -----
soyb_data <- na.omit(soyb_data)
soyb_data$lnYield <- log(soyb_data$Yield)

S_M1 <- lm(Yield ~ tavg + tmin + tmax + prcp, data = soyb_data)
summary(S_M1)
S_M2 <- lm(Yield ~ Year + tavg + tmin + tmax + prcp, data = soyb_data)
summary(S_M2)
S_M3 <- lm(Yield ~ Year + tavg + tmin + tmax + I(prcp^2), data = soyb_data)
summary(S_M3)
S_M4 <- lm(Yield ~ Year + tavg + tmin + tmax + ifelse(log(prcp) >= 0, log(prcp), 0), data = soyb_data)
summary(S_M4)
S_M5 <- lm(Yield ~ Year + tavg + tmin + tmax + prcp + Latitude + Longitude, data = soyb_data)
summary(S_M5)
S_M6 <- lm(lnYield ~ Year + tavg + tmin + tmax + prcp, data = soyb_data)
summary(S_M6)
S_M7 <- lm(lnYield ~ Year + tavg + tmin + tmax + log(prcp), data = soyb_data)
summary(S_M7)

# CROSS-VALIDATION
# Creating test set
# Creating work and test sets
smp_size <- floor(0.2*nrow(soyb_data))

set.seed(1)
test_ids <- sample(seq_len(nrow(soyb_data)), size = smp_size)
soyb_data$test <- 0
soyb_data$test[test_ids] <- 1

soyb_test <- soyb_data %>% filter(soyb_data$test == 1)
soyb_work <- soyb_data %>% filter(soyb_data$test == 0)

# CV
# fold num
k <- 5

set.seed(1)
cv1 <- train(as.formula(S_M1), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv2 <- train(as.formula(S_M2), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv3 <- train(as.formula(S_M3), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv4 <- train(as.formula(S_M4), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv5 <- train(as.formula(S_M5), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv6 <- train(as.formula(S_M6), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1)
cv7 <- train(as.formula(S_M7), soyb_work, method = "lm", trControl = trainControl(method = "cv", number = k))

# calculate average RMSE
cv <- c("cv1","cv2","cv3", "cv4", "cv5", "cv6", "cv7")
rmse_cv <- c()

for(i in 1:length(cv)) {
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2 +
                        get(cv[i])$resample[[1]][5]^2)/5)
}

# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4]),
                     rbind(cv5$resample[1], rmse_cv[5]),
                     rbind(cv6$resample[1], rmse_cv[6]),
                     rbind(cv7$resample[1], rmse_cv[7]))

colnames(cv_mat) <- c("Resample", "Model1","Model2","Model3", "Model4", "Model5", "Model6", "Model7")
cv_mat

# test set predictions
#create formula list
models <- list(S_M1, S_M2, S_M3, S_M4, S_M5, S_M6, S_M7)
rmse_test <- c()
pred <- data.frame(matrix(0, nrow = nrow(soyb_test), ncol = length(models)))

for (i in 1:length(models)) {
  pred[,i] <- predict(models[[i]], newdata = soyb_test)
  
  if (models[[i]]$call$formula[2] == "lnYield()") {
    lev <- exp(pred[,i]) * exp((cv_mat[5, 1 + i]^2)/2)
    rmse_test[i] <- RMSE(lev, soyb_test$Yield)
  } else {
    rmse_test[i] <- RMSE(pred[,i], soyb_test$Yield)
  }
}
model_names <- c("S_M1", "S_M2", "S_M3", "S_M4", "S_M5", "S_M6", "S_M7")

test_mat <- cbind(model_names, rmse_test)
test_mat

rm(soyb_test, soyb_work, cv_mat, cv1, cv2, cv3, cv4, cv5, cv6, cv7, models, pred, pred_data, test_mat, cv, i, k, lev, model_names, rmse_cv, rmse_test, smp_size, test_ids)

# VIII.Predicions =====
prediction_files <- list.files("84/weather/prediction_targets_daily")
prediction_files <- str_replace(prediction_files, ".csv", "")
prediction_data <- list()
for (file in prediction_files) {
  if (file.info(paste0("84/weather/prediction_targets_daily/", file, ".csv"))$size > 0) {
    prediction_data[[file]] <- read.csv(paste0("84/weather/prediction_targets_daily/", file, ".csv"), header = FALSE, col.names = c("date", "tavg", "tmin", "tmax", "prcp"))
    prediction_data[[file]]$date <- as.Date(prediction_data[[file]]$date)
  }
}
prediction_files <- names(prediction_data)

# aggregate
prediction_data_aggregated <- list()
for (file in prediction_files) {
  if (typeof(prediction_data[[file]]$tavg) != "logical") {
    prediction_data_aggregated[[file]][1:2] <- aggregate(tavg ~ format(date, "%Y"), prediction_data[[file]], mean)
  } else {
    prediction_data_aggregated[[file]][1:2] <- NA
  }
  
  if (typeof(prediction_data[[file]]$tmin) != "logical") {
    prediction_data_aggregated[[file]][3:4] <- aggregate(tmin ~ format(date, "%Y"), prediction_data[[file]], mean)
  } else {
    prediction_data_aggregated[[file]][3:4] <- NA
  }
  
  if (typeof(prediction_data[[file]]$tmax) != "logical") {
    prediction_data_aggregated[[file]][5:6] <- aggregate(tmax ~ format(date, "%Y"), prediction_data[[file]], mean)
  } else {
    prediction_data_aggregated[[file]][5:6] <- NA
  }
  
  if (typeof(prediction_data[[file]]$prcp) != "logical") {
    prediction_data_aggregated[[file]][7:8] <- aggregate(prcp ~ format(date, "%Y"), prediction_data[[file]], sum)
    prediction_data_aggregated[[file]][9:10] <- aggregate(prcp ~ format(date, "%Y"), prediction_data[[file]], length)
  } else {
    prediction_data_aggregated[[file]][7:8] <- NA
    prediction_data_aggregated[[file]][9:10] <- NA
  }
  
  names(prediction_data_aggregated[[file]]) <- c("tavg_date", "tavg", "tmin_date", "tmin", "tmax_date", "tmax", "prcp_date", "prcp", "prcp_obs_date", "prcp_obs")
} 


# data frames
prediction_dataframes <- list()
for (file in prediction_files) {
  prediction_dataframes[[file]]$Year <- seq(1950, 2025, 1)
  prediction_dataframes[[file]]$tavg <- prediction_data_aggregated[[file]]$tavg[match(prediction_dataframes[[file]]$Year, prediction_data_aggregated[[file]]$tavg_date)]
  prediction_dataframes[[file]]$tmin <- prediction_data_aggregated[[file]]$tmin[match(prediction_dataframes[[file]]$Year, prediction_data_aggregated[[file]]$tmin_date)]
  prediction_dataframes[[file]]$tmax <- prediction_data_aggregated[[file]]$tmax[match(prediction_dataframes[[file]]$Year, prediction_data_aggregated[[file]]$tmax_date)]
  prediction_dataframes[[file]]$prcp <- prediction_data_aggregated[[file]]$prcp[match(prediction_dataframes[[file]]$Year, prediction_data_aggregated[[file]]$prcp_date)]
  prediction_dataframes[[file]]$prcp_obs <- prediction_data_aggregated[[file]]$prcp_obs[match(prediction_dataframes[[file]]$Year, prediction_data_aggregated[[file]]$prcp_obs_date)]
  prediction_dataframes[[file]] <- as.data.frame(prediction_dataframes[[file]])
  prediction_dataframes[[file]] <- prediction_dataframes[[file]][rowSums(is.na(prediction_dataframes[[file]][, 2:5])) < 4, ]
}

prediction_dataframes <- lapply(prediction_dataframes, function(df) {df[is.na(df)] <- 0; df})

rm(prediction_data, prediction_data_aggregated)

# prediction
for (file in prediction_files) {
  prediction_dataframes[[file]]$file <- file
  prediction_dataframes[[file]]$crop_corn <- "CORN, GRAIN"
  prediction_dataframes[[file]]$pred_corn <- predict.lm(M4, newdata = prediction_dataframes[[file]]) # M4 <- model
  prediction_dataframes[[file]]$crop_oats <- "OATS"
  prediction_dataframes[[file]]$pred_oats <- predict.lm(O_M4, newdata = prediction_dataframes[[file]]) # O_M4 <- model
  prediction_dataframes[[file]]$crop_soyb <- "SOYBEANS"
  prediction_dataframes[[file]]$pred_soyb <- predict.lm(M6, newdata = prediction_dataframes[[file]]) # S_M <- model
}

# export
export_df <- data.frame(matrix(0, 0, 4))
for (file in prediction_files) {
  colnames(export_df) <- c("file", "Year", "crop_corn", "pred_corn")
  export_df <- rbind(export_df, prediction_dataframes[[file]][c("file", "Year", "crop_corn", "pred_corn")])
  colnames(export_df) <- c("file", "Year", "crop_oats", "pred_oats")
  export_df <- rbind(export_df, prediction_dataframes[[file]][c("file", "Year", "crop_oats", "pred_oats")])
  colnames(export_df) <- c("file", "Year", "crop_soyb", "pred_soyb")
  export_df <- rbind(export_df, prediction_dataframes[[file]][c("file", "Year", "crop_soyb", "pred_soyb")])
}

colnames(export_df) <- c("Target location", "Year", "Crop", "Predicted yield (BU/acre)")

write.csv(export_df, "quant_predictions.csv", row.names = FALSE)

