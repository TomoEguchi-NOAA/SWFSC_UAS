
rm(list=ls())

library(XML)
library(gsubfn)
library(lattice)

data.dir <- "data/210617/"

fig.dir <- paste0(data.dir, "figures/")
if (!dir.exists(fig.dir))
  dir.create(fig.dir)

summary.dir <- paste0(data.dir, "summary/")
if (!dir.exists(summary.dir)) 
  dir.create(summary.dir)

filename <- "21061700.GPX"
FILE <- paste0(data.dir, filename)

filename.root <- unlist(strsplit(filename, ".GPX"))

doc <- xmlParse(FILE, useInternalNodes=TRUE)

ele <- as.numeric(xpathSApply(doc, path='//trkpt/ele', xmlValue))
ele.raw <- as.numeric(xpathSApply(doc, path='//extensions/ele_raw', xmlValue))
head<- xpathSApply(doc, path='//extensions/Compass', xmlValue)

heading<-strapply(head, "(\\d+).*", as.numeric, simplify = c)
laser <- as.numeric(xpathSApply(doc, path='//extensions/Laser', xmlValue))
alt <- xpathSApply(doc, path='//extensions/Altimeter', xmlValue)
alt <- as.numeric(gsub(",.*$", "", alt))/20
time <- xpathSApply(doc, path = '//trkpt/time', xmlValue)
coords <- xpathSApply(doc, path = '//trkpt', xmlAttrs)
# Extract latitude and longitude from the coordinates
lat <- as.numeric(coords['lat',])
lon <- as.numeric(coords['lon',])

#df <- data.frame(time, lat, lon, laser, alt, ele, ele.raw)
#head(df)

time<-unlist(strsplit(time, "Z")) #remove the Z after the time
time<-unlist(strsplit(time, "T", fixed=TRUE)) # split the string into date and time by removing the T
n.records<-length(time)
date.col<-time[seq(from=1, by=2, to=n.records)] #pull out the date (odd numbered records)
time.col<-time[seq(from=2, by=2, to=n.records)] #pull out the times (even numbered records)
time<-paste(date.col, time.col, sep=" ") # paste them back together as a character vector
time<-strptime(time, format="%Y-%m-%d %H:%M:%S", tz="GMT") # convert the character vector to a timedate object


interval<-diff(time)
flight.time<-difftime(max(time),min(time), units="secs") 
run.time<-difftime(time,min(time), units="secs") 

plot(laser~run.time, xlab="Time (s)", ylab="Altitude (m)", type="l", col=1)
lines(ele~run.time, col="red")
lines(alt~run.time, col='blue')
lines(ele.raw~run.time, col ='yellow')

plot(lon, lat, xlab="Degrees Longitude", ylab="Degrees Latitude", type="l", col=1 )
dev.print(jpeg, 
          paste0(fig.dir, filename.root, "_xyplot.jpg") , 
          res=300, height=7, width=5, units="in")

plot(cloud(laser~lon*lat, xlab="Longitude", ylab="Latitude", zlab="heading (m)", 
           type="l", col="red", lwd=2, auto.key=TRUE, 
           default.scales=list(distance=rep(0.75,3), arrows=FALSE)))

dev.print(jpeg, paste0(fig.dir, filename.root, "_xyzplot.jpg") , 
          res=300, height=7, width=5, units="in")

dtr<-pi/180
dist.from.start <- 6371 * 1000 * acos((sin(lat[1] * dtr) * sin(lat * dtr)) + 
                                        (cos(lat[1] * dtr) * cos(lat * dtr) * cos(lon * dtr - lon[1] * dtr)))

if(is.na(dist.from.start[1])){
  x <- is.nan(dist.from.start)
  x.dist <- length(dist.from.start[x])
  print(paste("THE FIRST ", x.dist, " RECORD(S) REMOVED DUE TO ERROR IN COMPUTING MOVEMENT WHILE STATIONARY AFTER MOTOR START"))
  x.rm <- seq(from=1, by=1, to = x.dist)
  dist.from.start <- dist.from.start[-x.rm]
  max.elevation<-max(ele[-x.rm])
  rise<-diff(ele[-x.rm])
  run<-diff(dist.from.start)
  displacement<-sqrt(run^2+rise^2) # x-y-z movement between recording intervals
  total.displacement<-cumsum(displacement) # total displacement during flight
  max.displacement<-max(displacement)
  mst<-as.numeric(interval[displacement==max.displacement])
  max.speed<-unique(max.displacement/mst)
  distance<-sqrt(dist.from.start^2 + Ele[-x.rm]^2)
  max.distance<-max(distance)
  mean.speed<-max(total.displacement)/as.numeric(Flight.time)
} else {
  max.elevation<-max(ele)
  rise<-diff(ele)
  run<-diff(dist.from.start)
  displacement<-sqrt(run^2+rise^2) # x-y-z movement between recording intervals
  total.displacement<-cumsum(displacement) # total displacement during flight
  max.displacement<-max(displacement)
  mst<-as.numeric(interval[displacement==max.displacement])
  max.speed<-unique(max.displacement/mst)
  distance<-sqrt(dist.from.start[-1]^2+ele[-1]^2)   # TE added [-1] to ele 2021-06-24
  max.distance<-max(distance)
  mean.speed<-max(total.displacement)/as.numeric(flight.time)
}


time = as.character(time)
out<-c(min(time), max(time), round(lat[1],3), 
       round(lon[1],3), round(flight.time,2), 
       round(max.elevation,2), round(max.distance,2), 
       round(max(total.displacement),2), 
       round(max.speed,2), round(mean.speed, 2))

names(out)<-c("Start_GMT", "End_GMT","Start_Lat", "Start_Long", "Duration_s", "Max_elevation_m", "Max_distance_m", "Total_distance_m", "Max_vel_m/s", "Mean_vel_m/s")

naming1<-paste0(summary.dir, filename.root, "_SUMMARY.csv")
write.csv(out, file=naming1)

time <- as.character(time)
track <- as.data.frame(cbind(time,lon,lat,laser,alt,ele,ele.raw,heading))
naming2 <- paste0(data.dir, filename.root, ".csv")

write.csv(track, file=naming2, row.names=F)

#library(shapefiles)

#PolyLine
#Id=1
#dd <- data.frame(Id,Lon,Lat)
#ddTable <- data.frame(Id=1,Name=NEWFILE)
#ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 3)
#write.shapefile(ddShapefile, NEWFILE, arcgis=T)

#Points (could just add xy data)
#Id=as.numeric(row(getTrack[1]))
#dd <- data.frame(Id,Lon,Lat,Alt,Time)
#ddTable <- data.frame(Id=Id, Lon, Lat,Alt,Time)
#ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 1)
#write.shapefile(ddShapefile, "test", arcgis=T)

