#read_DJI_log.R
#Reads DJI log file.
#
# Tomo Eguchi
# 2024-03-04
# 

library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)

dirname <- "data/Gray Whale Photogrammetry/Logs/20240108_F01_Leia/"
dirname <- "data/Gray Whale Photogrammetry/Logs/20240109_F02_Han Solo/"
load.log <- function(dirname){
  filename <- dir(path = dirname, pattern = ".csv")
  if (length(filename) == 1){
  
  dat.0 <- read.csv(file = paste0(dirname, filename)) %>%
    select(CUSTOM.date..local., CUSTOM.updateTime..local.,
           OSD.flyTime..s., OSD.latitude, OSD.longitude,
           OSD.height..ft., OSD.altitude..ft., 
           OSD.mileage..ft.,  OSD.hSpeed..MPH., 
           OSD.pitch, OSD.roll, OSD.yaw, 
           OSD.directionOfTravel, OSD.gpsNum, 
           OSD.gpsLevel, OSD.droneType,
           GIMBAL.pitch, GIMBAL.roll, GIMBAL.yaw,
           BATTERY.chargeLevel, BATTERY.voltage..V.) %>%
    transmute(Date = as.Date(CUSTOM.date..local., format = "%m/%d/%Y"),
              Time = CUSTOM.updateTime..local.,
              Flight.time = OSD.flyTime..s.,
              Latitude = OSD.latitude,
              Longitude = OSD.longitude,
              Height_ft = OSD.height..ft.,
              Altitude_ft = OSD.altitude..ft.,
              Distance_ft = OSD.mileage..ft.,
              Horiz_Speed = OSD.hSpeed..MPH.,
              Pitch = OSD.pitch,
              Roll = OSD.roll,
              Yaw = OSD.yaw,
              Direction = OSD.directionOfTravel,
              Num_GPS = OSD.gpsNum,
              Level_GPS = OSD.gpsLevel,
              Drone = OSD.droneType,
              Gimbal_Pitch = GIMBAL.pitch,
              Gimbal_Roll = GIMBAL.roll,
              Gimbal_Yaw = GIMBAL.yaw,
              Battery_Level = BATTERY.chargeLevel,
              Battery_voltage = BATTERY.voltage..V.)
 
  return(dat.0) 
  }
}

dat.0 <- load.log(dirname)

ggplot(data = dat.0) +
  geom_path(aes(x = Longitude, y = Latitude, color = Altitude_ft)) +
  annotate("text", x = dat.0$Longitude[1], 
           y = dat.0$Latitude[1], 
           color = "red", label = "Start") +
  annotate("text", x = dat.0$Longitude[nrow(dat.0)], 
           y = dat.0$Latitude[nrow(dat.0)], 
           color = "blue", label = "End")


