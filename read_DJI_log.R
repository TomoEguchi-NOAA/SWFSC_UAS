#read_DJI_log.R
#Reads DJI log file.
#
# Tomo Eguchi
# 2024-03-04
# 

rm(list = ls())
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# This script is designed for date and time recorded in local time (America/Los_Angeles).
# It needs to be changed when the date/time are recorded in GMT (or UTC)

# Leia and Han Solo have different output format
#dirname <- "data/Gray Whale Photogrammetry/Logs/20240108_F01_Leia/"
#dirname <- "data/Gray Whale Photogrammetry/Logs/20240109_F02_Han Solo/"

# a function to convert DJI system time
# DJI.time.conv <- function(x){
#   x1 <- strsplit(x, ":") %>% unlist() %>% as.numeric()
#   
#   if (x1[1] < 10){
#     x1.hr <- paste0("0", x1[1])
#   } else {
#     x1.hr <- x1[1]
#   }
#   
#   x1.min <- floor(x1[2])
#   if (x1.min < 10){
#     x1.min <- paste0("0", x1.min)
#   } 
#   
#   x1.sec <- round((x1[2] - floor(x1[2])) * 60)
#   if (x1.sec == 60){
#     x1.sec <- "00"
#     x1[2] <- x1[2] + 1
#   }
#   if (is.numeric(x1.sec) & x1.sec < 10){
#     x1.sec <- paste0("0", x1.sec)
#   }
#   
#   x2 <- paste0(x1.hr, ":", x1.min, ":", x1.sec)
#   return(x2)
# }

load.log <- function(dirname){
  filename <- dir(path = dirname, pattern = ".csv")
  #if (length(filename) == 1){
  
  all.lines <- read_lines(file = paste0(dirname, "/", filename))
  
  if (all.lines[1] == "sep=,"){
    header <- str_split(all.lines[2], ",") %>% unlist()
    
    k1 <- 3
  } else {
    header <- str_split(all.lines[1], ",") %>% unlist()
    k1 <- 2
  }
  
  header <- lapply(header, FUN = str_replace_all, "\\[", "_") %>% 
    unlist() %>%
    lapply(FUN = str_replace_all, "\\]", "_") 
  
  data.list <- lapply(all.lines[k1:length(all.lines)], 
                      FUN = function(x) str_split(x, ",") %>% unlist())

  data.df <- do.call("rbind", data.list) %>% as.data.frame()
  colnames(data.df) <- header
  
  data.df %>% filter(as.numeric(OSD.gpsNum) > 15) -> data.df
  
  data.df %>%
    dplyr::select(contains(c("date", "time", "latitude", "longitude", "height",
                             "altitude", "speed", "pitch", "roll", "yaw",
                             "gps", "drone", "battery", "mileage",
                             "direction"))) %>%
    dplyr::select(!contains(c(".is", ".goHome", ".low", ".serious"))) %>%
    transmute(Date.local = as.Date(`CUSTOM.date _local_`, format = "%m/%d/%Y"),
              Time.local = `CUSTOM.updateTime _local_`,
              #Time.local.HMS = DJI.time.conv(Time.local),
              Flight.time_s = as.numeric(`OSD.flyTime _s_`),
              Latitude = as.numeric(OSD.latitude),
              Longitude = as.numeric(OSD.longitude),
              Height_ft = as.numeric(`OSD.height _ft_`),
              Altitude_ft = as.numeric(`OSD.altitude _ft_`),
              Distance_ft = as.numeric(`OSD.mileage _ft_`),
              Horiz.Speed_MPH = as.numeric(`OSD.hSpeed _MPH_`),
              Pitch = as.numeric(OSD.pitch),
              Roll = as.numeric(OSD.roll),
              Yaw = as.numeric(OSD.yaw),
              Direction = as.numeric(OSD.directionOfTravel),
              Num.GPS = as.numeric(OSD.gpsNum),
              Level_GPS = as.numeric(OSD.gpsLevel),
              Drone = OSD.droneType,
              Gimbal.Pitch = as.numeric(GIMBAL.pitch),
              Gimbal.Roll = as.numeric(GIMBAL.roll),
              Gimbal.Yaw = as.numeric(GIMBAL.yaw),
              Battery.Level = as.numeric(BATTERY.chargeLevel),
              Battery.voltage = as.numeric(`BATTERY.voltage _V_`)) -> dat.0
 
   
  #dat.0 <- load.log(dirname)
  
  # compute great circle distances of all locations from where it took off
  dat.0$Distance.from.Loc1_m <- geosphere::distGeo(p1 = c(dat.0$Longitude[1], dat.0$Latitude[1]),
                                                   p2 = cbind(dat.0$Longitude, dat.0$Latitude)) 
  
  # time may be recorded in either HH and decimal minutes or HH:MM:SS AM/PM
  # time.str <- dat.0$Time.local[1] %>% strsplit(":") %>% unlist()
  # 
  # if (length(time.str) == 1){
  #   dat.0 %>%
  #     mutate(time.local.HMS = lapply(Time.local, DJI.time.conv)) -> dat.0
  # }
  
  dirname.parts <- strsplit(dirname, "/") %>% unlist()
  summary.df <- data.frame(ID = dirname.parts[length(dirname.parts)],
                           Start.Local = ymd_hms(paste(dat.0$Date.local[1], dat.0$Time.local[1]), 
                                                 tz = "America/Los_Angeles"),
                           End.Local = ymd_hms(paste(dat.0$Date.local[nrow(dat.0)], dat.0$Time.local[nrow(dat.0)]),
                                               tz = "America/Los_Angeles"),
                           Start.Lat = dat.0$Latitude[1],
                           Start.Lon = dat.0$Longitude[1],
                           Duration_s = dat.0$Flight.time_s[nrow(dat.0)],
                           Max.elevation_m = max(dat.0$Altitude_ft) * 0.3048,
                           Max.Distance_m = max(dat.0$Distance.from.Loc1_m),
                           Total.Distance_m = max(dat.0$Distance_ft) * 0.3048,
                           Max.vel_ms = max(dat.0$Horiz.Speed_MPH, na.rm = T) * 0.44704,
                           Mean.vel_ms = mean(dat.0$Horiz.Speed_MPH, na.rm = T) * 0.44704)
  
  # ggplot(data = dat.0) +
  #   geom_path(aes(x = Longitude, y = Latitude, color = Altitude_ft)) +
  #   annotate("text", x = dat.0$Longitude[1], 
  #            y = dat.0$Latitude[1], 
  #            color = "red", label = "Start") +
  #   annotate("text", x = dat.0$Longitude[nrow(dat.0)], 
  #            y = dat.0$Latitude[nrow(dat.0)], 
  #            color = "blue", label = "End")
 
  out.list <- list(data = dat.0,
                   summary = summary.df)
  return(out.list) 
}

dir.root <- "data/Gray Whale Photogrammetry/Logs/"
dir.names <- dir(dir.root)

dirs <- paste0(dir.root, dir.names[grep(pattern = "_Han", dir.names)])

summary.list <- lapply(dirs, FUN = load.log)

summary.all <- lapply(summary.list, FUN = function(x) x$summary) 
summary.df <- do.call("rbind", summary.all) %>%
  mutate(Duration_min = Duration_s/60)
