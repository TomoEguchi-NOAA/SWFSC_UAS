# Runs readGPX_with laser v2 on all .GPX files in one folder
# This version makes plots with ggplot2 

# if just working on new directories, set over.write = F (default). Existing
# files are left untouched.
#
# over.write.data = T will write over all output data files. 
# over.write.fig = T will write over all output figure files. 

readGPX_v3 <- function(in.dir, write.file = T, save.fig = T, over.write.data = F, over.write.fig = F){
  library(XML)
  library(gsubfn)
  library(lattice)
  library(tidyverse)
  library(ggplot2)
  library(lubridate)

  k1 <- 1
  #write.file <- T
  dirs <- list.dirs(in.dir, recursive = F)
  
  p.altimeters <- p.tracks <- p.aux <- p.sticks <- list()
  out.data <- aux.data <- list()
  
  for (k1 in 1:length(dirs)){
    p.altimeters.dir <- p.tracks.dir <- p.aux.dir <- p.sticks.dir <- out.data.dir <- aux.data.dir <- list()
    
    data.dir <- dirs[k1]
    summary.file.root <- unlist(strsplit(data.dir, "/"))[3]
    fig.dir <- paste0(data.dir, "/figures/")
    if (!dir.exists(fig.dir))
      dir.create(fig.dir)
    
    summary.dir <- paste0(data.dir, "/summary/")
    if (!dir.exists(summary.dir)) 
      dir.create(summary.dir)
    
    # the extension should be in uppercase
    all.files <- list.files(path = paste0(data.dir, "/GPX/"), 
                            pattern = ".GPX")
    
    out <- data.frame(ID = character(),
                      Start_GMT =  character(), 
                      End_GMT = character(),
                      Start_Lat = numeric(), 
                      Start_Long = numeric(), 
                      Duration_s = numeric(), 
                      Max_elevation_m = numeric(), 
                      Max_distance_m = numeric(), 
                      Total_distance_m = numeric(), 
                      Max_vel_ms = numeric(), 
                      Mean_vel_ms = numeric())
    
    #out <- matrix(NA, nrow = length(all.files), ncol = 11)
    
    #names(out)<-c("Start_GMT", "End_GMT","Start_Lat", "Start_Long", "Duration_s", "Max_elevation_m", "Max_distance_m", "Total_distance_m", "Max_vel_m/s", "Mean_vel_m/s")
    
    naming1<-paste0(summary.dir, summary.file.root,  "_SUMMARY.csv")
    
    k <- 1
    for (k in 1:length(all.files)){
      filename <- all.files[k] #"21061700.GPX"
      FILE <- paste0(data.dir, "/GPX/", filename)
      
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
      
      # other information:
      volt <- xpathSApply(doc, path='//extensions/Voltage', xmlValue)
      nick <- xpathSApply(doc, path='//extensions/NickAngle', xmlValue)
      roll <- xpathSApply(doc, path='//extensions/RollAngle', xmlValue)
      gas <- xpathSApply(doc, path='//extensions/Gas', xmlValue)
      gas_actual <- unlist(lapply(gas, FUN = strsplit, ",")) %>% 
        as.numeric() %>% 
        matrix(ncol = 2, byrow = T) %>%
        data.frame() %>%    
        transmute(Actual = X1, Estimated = X2) %>% 
        select("Actual")
      
      RCQ <- xpathSApply(doc, path='//extensions/RCQuality', xmlValue)
      RCSticks <- xpathSApply(doc, path='//extensions/RCSticks', xmlValue)
      RCSticks_First4 <- unlist(lapply(RCSticks, FUN = strsplit, ",")) %>% 
        as.numeric() %>% 
        matrix(ncol = 16, byrow = T) %>%
        data.frame() %>%    
        transmute(Nick = X1, Roll = X2, Yaw = X3, Gas = X4) 
      
    
      #df <- data.frame(time, lat, lon, laser, alt, ele, ele.raw)
      #head(df)
      time2 <- sub("T", " ", time)
      time3 <- sub("Z", "", time2)
      # the following (OS1) preserves decimal seconds, although they are not printed
      time4 <- as_datetime(strftime(time3, format = "%Y-%m-%d %H:%M:%OS1", tz="GMT")) 
      
      # time<-unlist(strsplit(time, "Z")) #remove the Z after the time
      # time<-unlist(strsplit(time, "T", fixed=TRUE)) # split the string into date and time by removing the T
      n.records<-length(time4)
      # date.col<-time[seq(from=1, by=2, to=n.records)] #pull out the date (odd numbered records)
      # time.col<-time[seq(from=2, by=2, to=n.records)] #pull out the times (even numbered records)
      # time<-paste(date.col, time.col, sep=" ") # paste them back together as a character vector
      # time<-strptime(time, format="%Y-%m-%d %H:%M:%S", tz="GMT") # convert the character vector to a timedate object
      
      interval<-diff(time4)
      #interval <- lubridate::interval(time4)
      #flight.time<-difftime(max(time),min(time), units="secs") 
      #run.time<-difftime(time,min(time), units="secs") 
      flight.time <- as.numeric(difftime(max(time4),  min(time4), units = "sec"))
      run.time <- time4 - rep(min(time4), length(time4))
      
      dtr<-pi/180
      dist.from.start <- 6371 * 1000 * acos((sin(lat[1] * dtr) * sin(lat * dtr)) + 
                                              (cos(lat[1] * dtr) * cos(lat * dtr) * cos(lon * dtr - lon[1] * dtr)))
      
      x.dist <- 0
      if(is.na(dist.from.start[1])){
        x <- is.nan(dist.from.start)
        x.dist <- length(dist.from.start[x])
        print(paste("THE FIRST ", x.dist, " RECORD(S) REMOVED DUE TO ERROR IN COMPUTING MOVEMENT WHILE STATIONARY AFTER MOTOR START"))
        x.rm <- seq(from=1, by=1, to = x.dist)
        dist.from.start <- dist.from.start[-x.rm]
        ele <- ele[-x.rm]
      } 
      
      max.elevation <- max(ele)
      rise <- diff(ele)
      run <- diff(dist.from.start)
      displacement <- sqrt(run^2 + rise^2) # x-y-z movement between recording intervals
      total.displacement <- cumsum(displacement) # total displacement during flight
      max.displacement <- max(displacement)
      mst <- as.numeric(interval[displacement == max.displacement])
      max.speed <- unique(max.displacement/mst)
      distance <- sqrt(dist.from.start^2+ele^2)   
      max.distance <- max(distance)
      mean.speed <- max(total.displacement)/as.numeric(flight.time)
      
      out[k,] <- c(paste0(filename.root, ".GPX"), 
                   as.character(min(time4)), 
                   as.character(max(time4)), 
                   round(lat[1],3), 
                   round(lon[1],3),
                   round(flight.time,2), 
                   round(max.elevation,2), 
                   round(max.distance,2),
                   round(max(total.displacement),2), 
                   round(max.speed,2), 
                   round(mean.speed, 2))
      
      
      time <- as.character(time)
      #readings.df <- as.data.frame(cbind(time,lon,lat,laser,alt,ele,ele.raw,heading))
      
      if (x.dist > 0) {
        dist.from.start <- c(rep(NA, x.dist), dist.from.start)
        ele <- c(rep(NA, x.dist), ele)
      }
      
      readings.df <- data.frame(laser = laser,
                                time = time,
                                run.time = as.numeric(run.time),
                                ele = ele,
                                ele.raw = ele.raw,
                                lon = lon,
                                lat = lat,
                                dist.from.start = dist.from.start)
      
      aux.data.df <- data.frame(run.time = as.numeric(run.time),
                                volt = as.numeric(volt),
                                nick = as.numeric(nick),
                                roll = as.numeric(roll),
                                gas = gas_actual,
                                RCQ = as.numeric(RCQ),
                                stick.nick = as.numeric(RCSticks_First4$Nick),
                                stick.roll = as.numeric(RCSticks_First4$Roll),
                                stick.yaw = as.numeric(RCSticks_First4$Yaw),
                                stick.gas = as.numeric(RCSticks_First4$Gas))
      
      
      readings.df <- na.omit(readings.df)
      
      out.data.dir[[k]] <- readings.df
      aux.data.dir[[k]] <- aux.data.df
      
      p.altimeters.dir[[k]] <- ggplot(readings.df) + 
        geom_path(aes(x = run.time/60, y = laser), color = "black") +
        geom_path(aes(x = run.time/60, y = ele), color = "red") +
        geom_path(aes(x = run.time/60, y = ele.raw), color = "yellow") +
        xlab("Time (min)") +
        ylab("Altitude (m)")
      
      p.tracks.dir[[k]] <- ggplot(readings.df) + 
        geom_path(aes(x = lon, y = lat, 
                      size = ele, color = run.time/60)) + 
        geom_point(aes(x = lon[1], y = lat[1]), 
                   color = "red", shape = "circle") +
        geom_text(aes(x = lon[1], y = lat[1], label = "begin"),
                  color = "red")+
        geom_point(aes(x = lon[nrow(readings.df)], 
                       y = lat[nrow(readings.df)]), 
                   color = "red", shape = "circle") +
        geom_text(aes(x = lon[nrow(readings.df)], 
                      y = lat[nrow(readings.df)],
                      label = "end"),
                  color = "red") +
        xlab("Longitude") +
        ylab("Latitude")
      
      p.volt <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = volt)) +
        xlab("Time (sec)") + ylab("Voltage")
      
      p.nick <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = nick)) +
        xlab("Time (sec)") + ylab("Nick (degrees)")
      
      p.roll <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = roll)) +
        xlab("Time (sec)") + ylab("Roll (degrees)")

      p.gas <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = Actual)) +
        xlab("Time (sec)") + ylab("Gas")
      
      p.RCQ <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = RCQ)) +
        xlab("Time (sec)") + ylab("RC Quality")
      
      p.aux.dir[[k]] <- cowplot::plot_grid(p.volt, p.nick,
                                       p.roll, p.gas,
                                       p.RCQ)
      
      p.stick.nick <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = stick.nick)) +
        xlab("Time (sec)") + ylab("Nick input (stick)")
      
      p.stick.roll <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = stick.roll)) +
        xlab("Time (sec)") + ylab("Roll input (stick)")
      
      p.stick.yaw <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = stick.yaw)) +
        xlab("Time (sec)") + ylab("Yaw input (stick)")
      
      p.stick.gas <- ggplot(aux.data.df) +
        geom_path(aes(x = run.time, y = stick.gas)) +
        xlab("Time (sec)") + ylab("Gas input (stick)")
      
      p.sticks.dir[[k]] <- cowplot::plot_grid(p.stick.nick,
                                             p.stick.roll,
                                             p.stick.yaw,
                                             p.stick.gas)
      if (save.fig){
        fname1 <- paste0(fig.dir, filename.root, "_altimeters.png")
        fname2 <- paste0(fig.dir, filename.root, "_tracks.png")
        fname3 <- paste0(fig.dir, filename.root, "_aux.png")
        fname4 <- paste0(fig.dir, filename.root, "_sticks.png")
        
        if (file.exists(fname1) == F){
          ggsave(p.altimeters.dir[[k]], 
                 filename = fname1,
                 device = "png", dpi = 600)
        } else if (file.exists(fname1) == T & over.write.fig == T){
          ggsave(p.altimeters.dir[[k]], 
                 filename = fname1,
                 device = "png", dpi = 600)
        }
        
        if (file.exists(fname2) == F){
          ggsave(p.tracks.dir[[k]], 
                 filename = fname2,
                 device = "png", dpi = 600)
        } else if (file.exists(fname2) == T & over.write.fig == T){
          ggsave(p.altimeters.dir[[k]], 
                 filename = fname2,
                 device = "png", dpi = 600)
        }
        
        if (file.exists(fname3) == F){
          ggsave(p.aux.dir[[k]], 
                 filename = fname3,
                 device = "png", dpi = 600)
        } else if (file.exists(fname3) == T & over.write.fig == T){
          ggsave(p.altimeters.dir[[k]], 
                 filename = fname3,
                 device = "png", dpi = 600)
        }  
        
        if (file.exists(fname4) == F){
          ggsave(p.sticks.dir[[k]],
                 filename = fname4,
                 device = "png", dpi = 600)
        } else if (file.exists(fname4) == T & over.write.fig == T){
          ggsave(p.altimeters.dir[[k]], 
                 filename = fname4,
                 device = "png", dpi = 600)
        }  
        
      }
      
      if (write.file){
        naming1<-paste0(summary.dir, summary.file.root,  "_SUMMARY.csv")
        naming2 <- paste0(data.dir, "/", filename.root, ".csv")
        out.df <- as.data.frame(out)
        if (file.exists(naming1) == F){
          write.csv(out.df, 
                    file = naming1, 
                    row.names = F, quote = F)
        } else if (file.exists(naming1) == T & over.write.data == T){
          write.csv(out.df, 
                    file = naming1, 
                    row.names = F, quote = F)
        }
        
        if (
          file.exists(naming2) == F){
          write.csv(readings.df, 
                    file=naming2, 
                    row.names=F, quote = F)
        } else if (file.exists(naming2) == T & over.write.data == T){
          write.csv(readings.df, 
                    file=naming2, 
                    row.names=F, quote = F)
        }          
        
      }
      
    }
    
    p.altimeters[[k1]] <- p.altimeters.dir
    p.tracks[[k1]] <- p.tracks.dir
    p.aux[[k1]] <- p.aux.dir
    p.sticks[[k1]] <- p.sticks.dir
    out.data[[k1]] <- out.data.dir
    aux.data[[k1]] <- aux.data.dir
  }

  out.list <- list(plot_altimeter = p.altimeters,
                   plot_tracks = p.tracks,
                   plot_aux = p.aux,
                   plot_sticks = p.sticks,
                   df_out = out.data,
                   aux_data = aux.data)
  return(out.list)  
}


#time <- as.character(time)
# out.df <- as.data.frame(out)
# 
# write.csv(out.df, file=naming1, row.names = F, quote = F)

