#read_DJI_log.R
#Reads DJI log files that were created by Airdata. This new application was used
#since May 2024.
#
# Tomo Eguchi
# 2024-03-04
# 

rm(list = ls())
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(exifr)  # for getting exif data from images

# This script is designed for date and time recorded in UTC.

#dir.root <- "data/Gray Whale Photogrammetry/Logs/"
#dir.root <- "data/San Diego Bay Green Turtles/20240509/"
#dir.root <- "data/San Diego Coastal Cetacean/20240612/"
#dir.root <- "data/Ruben Lasker Trawl/20240627/"
dir.root <- "data/Gray Whale Photogrammetry/Logs/20240108_F01_Leia/"

load.log <- function(dirname){

  # get the FAA registration code for all UAS in the inventory:
  reg.numbers <- readxl::read_excel(path = "data/UAS Inventory.xlsx")
  
  dir.name.parts <- strsplit(dirname, "_") %>% unlist()
  nick.name <- dir.name.parts[length(dir.name.parts)]
  
  FAA.ID <- reg.numbers %>% filter(Nickname == nick.name) %>% 
    select(`FAA Registration #`) %>%
    pull()
  
  # project name
  root.name.parts <- strsplit(dir.root, "/") %>% unlist()
  dir.name <- root.name.parts[2]
  proj.names <- readxl::read_excel(path = "data/Project list.xlsx")
  proj <- proj.names %>%
    filter(Directory == dir.name) %>%
    select(Project) %>%
    pull()
  
  filename <- dir(path = dirname, pattern = "Airdata.csv")
  #if (length(filename) == 1){
  
  all.lines <- read_lines(file = paste0(dirname, "/", filename))
  
  if (all.lines[1] == "sep=,"){
    header <- str_split(all.lines[2], ",") %>% unlist()
    
    k1 <- 3
  } else {
    header <- str_split(all.lines[1], ",") %>% unlist()
    k1 <- 2
  }
  
  #header <- lapply(header, FUN = str_replace_all, "\\[", "_") %>% 
   # unlist() %>%
    #lapply(FUN = str_replace_all, "\\]", "_") 
  
  data.list <- lapply(all.lines[k1:length(all.lines)], 
                      FUN = function(x) {
                        tmp <- str_split(x, ",") %>% unlist()
                        return(tmp[1:length(header)])
                      })

  data.df <- do.call("rbind", data.list) %>% as.data.frame()
  colnames(data.df) <- header
  
  data.df %>% filter(as.numeric(satellites) > 15) -> data.df
  
  data.df %>%
    dplyr::select(contains(c("date", "time", "latitude", "longitude", "height",
                             "altitude", "speed", "distance", "satellites", "gpslevel",
                             "pitch", "roll", "heading",
                             "rc", "battery", "mileage",
                             "gimbal", "elevation",
                             "direction", "Photo", "Video"))) %>%
    #dplyr::select(!contains(c(".is", ".goHome", ".low", ".serious"))) %>%
    transmute(Date.UTC = ymd_hms(`datetime(utc)`),
              Date.local = with_tz(Date.UTC, tzone = "America/Los_Angeles"),
              #Time.local = `CUSTOM.updateTime _local_`,
              #Time.local.HMS = DJI.time.conv(Time.local),
              Flight.time_s = as.numeric(`time(millisecond)`) / 1000,
              Latitude = as.numeric(latitude),
              Longitude = as.numeric(longitude),
              Height_ft = as.numeric(`height_above_takeoff(feet)`),
              Height_AG_ft = as.numeric(`height_above_ground_at_drone_location(feet)`),
              Elevation_ft = as.numeric(`ground_elevation_at_drone_location(feet)`),
              Altitude_ft = as.numeric(`altitude_above_seaLevel(feet)`),
              Distance_ft = as.numeric(`distance(feet)`),
              Horiz.Speed_MPH = as.numeric(`speed(mph)`),
              Pitch = as.numeric(` pitch(degrees)`),
              Roll = as.numeric(` roll(degrees)`),
              #Yaw = as.numeric(OSD.yaw),
              Direction = as.numeric(` compass_heading(degrees)`),
              Num.GPS = as.numeric(satellites),
              Level_GPS = as.numeric(gpslevel),
              #Drone = OSD.droneType,
              Gimbal.Pitch = as.numeric(`gimbal_pitch(degrees)`),
              Gimbal.Roll = as.numeric(`gimbal_roll(degrees)`),
              #Gimbal.Yaw = as.numeric(GIMBAL.yaw),
              Battery.Level = as.numeric(battery_percent),
              isPhoto = as.numeric(isPhoto),
              isVideo = as.numeric(isVideo)) -> dat.0
              #Battery.voltage = as.numeric(`BATTERY.voltage _V_`)) -> dat.0
 
   
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
  summary.df <- data.frame(Takeoff.Local = ymd_hms(dat.0$Date.local[1]),
                           UAS = FAA.ID,
                           VTOL_time = signif(dat.0$Flight.time_s[nrow(dat.0)]/60, 3),
                           Total_time = signif(dat.0$Flight.time_s[nrow(dat.0)]/3600, 3),
                           Landings = 1,
                           Pilot = NA,
                           Latitude = signif(dat.0$Latitude[1], 4),
                           Longitude = signif(dat.0$Longitude[1], 5),
                           Mission_Type = "Research",
                           Issues = NA,
                           COA = NA,
                           Project = proj,
                           Event = NA,
                           Project_Type = NA,
                           Data_Product = "Video",
                           VO = NA,
                           Remarks = NA)
  
                           # Duration_s = dat.0$Flight.time_s[nrow(dat.0)],
                           # Max.elevation_m = max(dat.0$Altitude_ft) * 0.3048,
                           # Max.Distance_m = max(dat.0$Distance.from.Loc1_m),
                           # Total.Distance_m = max(dat.0$Distance_ft) * 0.3048,
                           # Max.vel_ms = max(dat.0$Horiz.Speed_MPH, na.rm = T) * 0.44704,
                           # Mean.vel_ms = mean(dat.0$Horiz.Speed_MPH, na.rm = T) * 0.44704)
  
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

dir.names <- dir(dir.root, recursive = F)
# 
# #dirs <- paste0(dir.root, dir.names[grep(pattern = "_Han", dir.names)])
dirs <- paste0(dir.root, "/", dir.names)

summary.list <- lapply(dirs, FUN = load.log)

summary.all <- lapply(summary.list, FUN = function(x) x$summary) 

summary.df <- do.call("rbind", summary.all) %>%
  arrange(Takeoff.Local)

# summary.df <- do.call("rbind", summary.all) %>%
#   mutate(Duration_min = Duration_s/60)

# find the directory name (Date) and use it as part of the output filename
tmp.1 <- str_split(dir.root, "/") %>% unlist()
tmp.2 <- tmp.1[str_count(tmp.1) > 0]
#tmp.2[length(tmp.2)]

write.csv(summary.df,
          file = paste0(dir.root, "flight_summary_", tmp.2[length(tmp.2)], ".csv"))

