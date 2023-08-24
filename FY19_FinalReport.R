# Script to do some simple calculations for the final report on FY19 Search and
# detect funding from UASD
# 
# Tomo Eguchi
# 2023-08-22
#

library(tidyverse)
library(geosphere)
library(ggplot2)

# Bring in an example track from 2021

track_21070305 <- read.csv(file = "data/Leatherback_2021/20210703/21070305.csv")


n.row <- nrow(track_21070305)

bearings <- bearing(track_21070305[1:(n.row-1), c("lon", "lat")],
                    track_21070305[2:n.row, c("lon", "lat")])

track_21070305$bearing <- c(NA, bearings)

track_21070305 %>%
  mutate(bearing.idx = ifelse(bearing > 0, 1, -1)) -> track_21070305

# distance from the last time bearing changed the sign
track_21070305$D <- NA
p1 <- track_21070305[1, c("lon", "lat")]
for (k in 3:n.row){
  if (track_21070305$bearing.idx[k] == track_21070305$bearing.idx[k-1]){
    track_21070305$D[k] <- distHaversine(track_21070305[k, c("lon", "lat")],
                                         p1)
  } else {
    p1 <- track_21070305[k, c("lon", "lat")] 
  }
  
}

track_21070305 %>%
  mutate(Time = strptime(time, format = "%Y-%m-%dT%H:%M:%S"),
    dt = Time - Time[1]) -> track_21070305

ggplot(data = track_21070305) +
  geom_path(aes(x = dt, y = D))

# About 600m per track line. 

# Problem flight with loss of altitude
# 

track_21101602 <- read.csv(file = "data/Leatherback_HMB_2021/20211016_28R/21101602.csv")

track_21101602 %>%
  mutate(Time = strptime(time, format = "%Y-%m-%dT%H:%M:%S"),
         dt = Time - Time[1]) -> track_21101602

ggplot(data = track_21101602) +
  geom_path(aes(x = dt, y = ele))
