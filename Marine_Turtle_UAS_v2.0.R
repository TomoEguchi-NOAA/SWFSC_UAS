#### REVIEWS ####

WORKING_DIR <- "/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Data/Marine_Turtle_UAS_Survey_Image_Review/Reviews"

# Create a list of all the files in the Working Directory
REVIEWS_FILES_temp <- list.files(WORKING_DIR,
                             recursive = T,
                             full.names = T)

# Select the files found in the "Labeled_Logs" folder corresponding to each day of flying
REVIEWS_FILES_temp <- REVIEWS_FILES_temp[stringr::str_sub(REVIEWS_FILES_temp,-4,-1)==".csv"]

# Create a data.frame to house all the REVIEWS log data
REVIEWS <- data.frame()

# Loop through REVIEWS log files and append data to REVIEWS
for(i in REVIEWS_FILES_temp){
  
  # Read REVIEWS log file
  REVIEWS_temp <- read.csv(i,stringsAsFactors = F,header = T)
  
  # Add columns for full directory, file, and flight specification from the log directory string
  REVIEWS_temp$LOG_DIR <- i
  REVIEWS_temp$LOG_FILE <- rev(stringr::str_split(i, pattern = "/")[[1]])[1]
  
  # append data to REVIEWS
  REVIEWS <- plyr::rbind.fill(REVIEWS,REVIEWS_temp)
  
  # Progress indicator
  print(i)
  
  # Clean-up
  remove(REVIEWS_temp)
  gc()
  
}


# Clean-up
remove(i, REVIEWS_FILES_temp)


REVIEWS <- REVIEWS[REVIEWS$EVENT_CODE != "",]

REVIEWS$FIRST_FRAME<-stringr::str_replace_all(string = REVIEWS$FIRST_FRAME, pattern = "\\\\", replacement = "/")
REVIEWS$LAST_FRAME<-stringr::str_replace_all(string = REVIEWS$LAST_FRAME, pattern = "\\\\", replacement = "/")

REVIEWS$REVIEW_DATE_TIME <- as.POSIXct(strptime(REVIEWS$DATE_TIME,format = "%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"))

for(i in which(!is.na(REVIEWS$FIRST_FRAME))){
  REVIEWS$IMAGE_FILE_FIRST[i] <- rev(stringr::str_split(REVIEWS$FIRST_FRAME[i], pattern = "/")[[1]])[1]
  REVIEWS$FLIGHT[i] <- rev(stringr::str_split(REVIEWS$FIRST_FRAME[i], pattern = "/")[[1]])[2]
  REVIEWS$REVIEWER[i] <- stringr::str_split(REVIEWS$FIRST_FRAME[i], pattern = "/")[[1]][3]
  REVIEWS$DATE_TIME_FIRST[i] <- stringr::str_split(REVIEWS$IMAGE_FILE_FIRST[i], pattern = "_")[[1]][2]
  REVIEWS$IMG_NUMBER_FIRST[i] <- stringr::str_split(REVIEWS$IMAGE_FILE_FIRST[i], pattern = "_")[[1]][3]
}

for(i in which(!is.na(REVIEWS$LAST_FRAME))){
  REVIEWS$IMAGE_FILE_LAST[i] <- rev(stringr::str_split(REVIEWS$LAST_FRAME[i], pattern = "/")[[1]])[1]
  REVIEWS$DATE_TIME_LAST[i] <- stringr::str_split(REVIEWS$IMAGE_FILE_LAST[i], pattern = "_")[[1]][2]
  REVIEWS$IMG_NUMBER_LAST[i] <- stringr::str_split(REVIEWS$IMAGE_FILE_LAST[i], pattern = "_")[[1]][3]
}


REVIEWS[REVIEWS$REVIEWER == "jaychung","REVIEWER"] <- "JC"
REVIEWS[REVIEWS$REVIEWER == "melissa.cook","REVIEWER"] <- "MC"
REVIEWS[REVIEWS$REVIEWER == "tomo.eguchi","REVIEWER"] <- "TE"
REVIEWS[REVIEWS$REVIEWER == "trevor.joyce","REVIEWER"] <- "TJ"

# Convert DATE string to POSIXct
REVIEWS$DATE_TIME_FIRST <- as.POSIXct(strptime(REVIEWS$DATE_TIME_FIRST,format = "%Y%m%d%H%M%S",tz="America/Los_Angeles"))
REVIEWS$DATE_TIME_LAST <- as.POSIXct(strptime(REVIEWS$DATE_TIME_LAST,format = "%Y%m%d%H%M%S",tz="America/Los_Angeles"))

# # Convert image file NUMBER to numeric
# REVIEWS$IMG_NUMBER_FIRST <- as.numeric(REVIEWS$IMG_NUMBER_FIRST)
# REVIEWS$IMG_NUMBER_LAST <- as.numeric(REVIEWS$IMG_NUMBER_LAST)


# Add a sequential ID column to reestablish original order after splits 
REVIEWS$SEQ_ID <- 1:nrow(REVIEWS)



for(i in unique(REVIEWS$REVIEWER)){
  for(j in unique(REVIEWS$FLIGHT)){
    REVIEWS[REVIEWS$REVIEWER==i 
            & REVIEWS$FLIGHT== j 
            & REVIEWS$EVENT_CODE %in% c("Start Flight","Resume Flight"),"TIME_BLOCK"] <- paste(i,j,
                                                                                               stringr::str_pad(string = seq(1,nrow(REVIEWS[REVIEWS$REVIEWER==i 
                                                                                                                      & REVIEWS$FLIGHT== j 
                                                                                                                      & REVIEWS$EVENT_CODE %in% c("Start Flight","Resume Flight"),])), width = 2, pad = "0"),
                                                                                               sep = "_")
  }
}

REVIEWS$TIME_BLOCK <-zoo::na.locf(REVIEWS$TIME_BLOCK)

for(i in unique(REVIEWS$TIME_BLOCK)){
  REVIEWS[REVIEWS$TIME_BLOCK ==i,"TIME_BLOCK_START"] <- REVIEWS[REVIEWS$TIME_BLOCK ==i 
                                                                & REVIEWS$EVENT_CODE %in% c("Start Flight","Resume Flight"),"REVIEW_DATE_TIME"]
  REVIEWS[REVIEWS$TIME_BLOCK ==i,"TIME_BLOCK_END"] <- REVIEWS[REVIEWS$TIME_BLOCK ==i 
                                                                & REVIEWS$EVENT_CODE %in% c("End Flight","Pause Flight"),"REVIEW_DATE_TIME"]
  REVIEWS[REVIEWS$TIME_BLOCK ==i,"TIME_BLOCK_FIRST"] <- REVIEWS[REVIEWS$TIME_BLOCK ==i 
                                                                & REVIEWS$EVENT_CODE %in% c("Start Flight","Resume Flight"),"DATE_TIME_FIRST"]
  REVIEWS[REVIEWS$TIME_BLOCK ==i,"TIME_BLOCK_LAST"] <- REVIEWS[REVIEWS$TIME_BLOCK ==i 
                                                              & REVIEWS$EVENT_CODE %in% c("End Flight","Pause Flight"),"DATE_TIME_FIRST"]
  
}
  
REVIEWS$TIME_BLOCK_DURATION <- as.numeric(difftime(REVIEWS$TIME_BLOCK_END,
                                                   REVIEWS$TIME_BLOCK_START,
                                                   units = "mins"))

#### IMAGES ####
WORKING_DIR <- "/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Data/Marine_Turtle_UAS_Survey_Image_Review/Flights"

# Create a list of all the files in the Working Directory
IMAGES <- list.files(WORKING_DIR,
                     recursive = T,
                     full.names = T)

# Select the files found in the "Labeled_Logs" folder corresponding to each day of flying
IMAGES <- IMAGES[stringr::str_sub(IMAGES,-4,-1)%in%c(".jpg",".JPG")]

# Convert from character vector to a data.frame
IMAGES <- data.frame(DIR = IMAGES, stringsAsFactors = F)

for(i in 1:nrow(IMAGES)){
  
  # Extract the FILE name from the DIR string (last item in DIR string)
  IMAGES$FILE[i] <- rev(stringr::str_split(IMAGES$DIR[i], pattern = "/")[[1]])[1]
  
  # Extract the FILE name from the DIR string (second to last item in DIR string)
  IMAGES$FLIGHT[i] <- rev(stringr::str_split(IMAGES$DIR[i], pattern = "/")[[1]])[2]
  
  # Extract DATE_TIME string information from FILE string (second item in FILE)
  IMAGES$DATE_TIME[i] <- stringr::str_split(IMAGES$FILE[i], pattern = "_")[[1]][2]
  
  # Extract photo NUMBER information from FILE string (third item in FILE)
  IMAGES$NUMBER[i] <- stringr::str_split(IMAGES$FILE[i], pattern = "_")[[1]][3]
  
}

#Clean-up
remove(i)

# Convert DATE string to POSIXct
IMAGES$DATE_TIME <- as.POSIXct(strptime(IMAGES$DATE_TIME,format = "%Y%m%d%H%M%S",tz="America/Los_Angeles"))

for(i in unique(REVIEWS$REVIEWER)){
  
  for(k in unique(REVIEWS[REVIEWS$REVIEWER == i,"TIME_BLOCK"])){
    
    IMAGES[IMAGES$DATE_TIME >= REVIEWS[REVIEWS$TIME_BLOCK == k,"TIME_BLOCK_FIRST"][1] &
             IMAGES$DATE_TIME <= REVIEWS[REVIEWS$TIME_BLOCK == k,"TIME_BLOCK_LAST"][1], paste("TIME_BLOCK_",i,sep = "")] <- k
  
  }
  
  for(j in which(REVIEWS$REVIEWER == i 
                 & REVIEWS$EVENT_CODE == "Sighting")){
    
    IMAGES[IMAGES$DATE_TIME >= REVIEWS$DATE_TIME_FIRST[j] &
             IMAGES$DATE_TIME <= REVIEWS$DATE_TIME_LAST[j], paste(c("REVIEWER","TURTLE_CERTAINTY","SPECIES","DEPTH"),"_",i,sep = "")] <- 
      
      REVIEWS[j,c("REVIEWER","TURTLE_CERTAINTY","SPECIES","DEPTH")]
  }
  
}


for(i in 1:nrow(IMAGES)){
  IMAGES$TURTLE_CERTAINTY_ALL[i] <- ifelse("Yes" %in% IMAGES[i,paste("TURTLE_CERTAINTY_",c("JC","MC","TE","TJ"), sep = "")],"Yes",
                                           ifelse("Maybe" %in% IMAGES[i,paste("TURTLE_CERTAINTY_",c("JC","MC","TE","TJ"), sep = "")],"Maybe",NA))
  
  IMAGES$SPECIES_ALL[i] <- ifelse("Green Sea Turtle" %in% IMAGES[i,paste("SPECIES_",c("JC","MC","TE","TJ"), sep = "")],"Green Sea Turtle",
                               ifelse("Unidentified Sea Turtle" %in% IMAGES[i,paste("SPECIES_",c("JC","MC","TE","TJ"), sep = "")],"Unidentified Sea Turtle",NA))
  
  IMAGES$DEPTH_ALL[i] <- ifelse("Surface" %in% IMAGES[i,paste("DEPTH_",c("JC","MC","TE","TJ"), sep = "")],"Surface",
                             ifelse("Near Surface" %in% IMAGES[i,paste("DEPTH_",c("JC","MC","TE","TJ"), sep = "")],"Near Surface",
                                    ifelse("Submerged" %in% IMAGES[i,paste("DEPTH_",c("JC","MC","TE","TJ"), sep = "")],"Submerged",NA)))
  
}


#### SIGHTINGS ####

SIGHTINGS <- IMAGES[!is.na(IMAGES$TURTLE_CERTAINTY_ALL),]

#### TIME_BLOCKS ####

TIME_BLOCKS <- REVIEWS[!duplicated(REVIEWS$TIME_BLOCK),]

for(i in 1:nrow(TIME_BLOCKS)){
  TIME_BLOCKS$NUMBER_IMAGES[i] <- nrow(IMAGES[IMAGES$DATE_TIME >= TIME_BLOCKS$TIME_BLOCK_FIRST[i] 
                                              & IMAGES$DATE_TIME <= TIME_BLOCKS$TIME_BLOCK_LAST[i],])
}

#### FRAMES #####

# Create a subset of all the logs containing just camera trigger (CAM) entries
# and associated columns
FRAMES <- IMAGES[,c("DIR","FILE","FLIGHT","DATE_TIME","NUMBER")]


# Append selected EXIF data fields pertaining to files specified in DIR
FRAMES <- cbind(FRAMES,exiftoolr::exif_read(IMAGES$DIR,tags = c("FileName","Directory","ExposureTime","FNumber", "ISO",
                                                                "DateTimeOriginal","CreateDate","ShutterSpeedValue","ApertureValue","ExposureCompensation",
                                                                "FocalLength","ExifImageWidth","ExifImageHeight",
                                                                "GpsSMISSION_PLANus","AltitudeType","AbsoluteAltitude","RelativeAltitude",
                                                                "GimbalRollDegree","GimbalYawDegree","GimbalPitchDegree",
                                                                "FlightRollDegree","FlightYawDegree","FlightPitchDegree",
                                                                "FlightXSpeed","FlightYSpeed","FlightZSpeed",
                                                                "SerialNumber","LensInfo","UniqueCameraModel","ProductName",
                                                                "ImageWidth","ImageHeight","ImageSize","Megapixels",
                                                                "GPSAltitude","GPSLatitude","GPSLongitude","CircleOfConfusion",
                                                                "FOV","HyperfocalDistance","LightValue")))

FRAMES$FOCAL_LENGTH <- FRAMES$FocalLength
FRAMES$ALT_MSL <- as.numeric(FRAMES$AbsoluteAltitude)
FRAMES$ALT_AGL <- as.numeric(FRAMES$RelativeAltitude)
FRAMES$PIXELS_X <- FRAMES$ImageWidth
FRAMES$PIXELS_Y <- FRAMES$ImageHeight
# FRAMES$YAW <- FRAMES$GimbalYawDegree
FRAMES$YAW <- as.numeric(FRAMES$FlightYawDegree)
FRAMES$YAW_AIRCRAFT <- as.numeric(FRAMES$FlightYawDegree)
FRAMES$PITCH <- as.numeric(FRAMES$GimbalPitchDegree)
FRAMES$ROLL <- as.numeric(FRAMES$GimbalRollDegree)
FRAMES$LAT <- as.numeric(FRAMES$GPSLatitude)
FRAMES$LONG <-as.numeric(FRAMES$GPSLongitude)
FRAMES$CAMERA <- FRAMES$UniqueCameraModel

# Simplify
FRAMES <- FRAMES[,c("DIR","FILE","FLIGHT","DATE_TIME","NUMBER",
              "FOCAL_LENGTH","ALT_MSL","ALT_AGL","PIXELS_X",
              "PIXELS_Y","YAW","YAW_AIRCRAFT","PITCH","ROLL",
              "LAT","LONG","CAMERA")]

# Convert GPS time to local time
FRAMES$DATE_TIME <- lubridate::with_tz(FRAMES$DATE_TIME, tzone = "America/Los_Angeles") 

# Convert FOCAL_LENGTH from milimeters to meters
FRAMES$FOCAL_LENGTH <- FRAMES$FOCAL_LENGTH/1000

# Specify SENSOR_WIDTH and SENSOR_HEIGHT for each camera in mm
FRAMES$SENSOR_WIDTH <- NA
FRAMES$SENSOR_HEIGHT <- NA

FRAMES$SENSOR_WIDTH <- ifelse(FRAMES$CAMERA == "Hasselblad L2D-20c Mavic3Pro", 
                           17.3 + 0.01 * 17.3, #  Hasselblad 20MP 24mm Equivalent 1x Sensor Width = 17.3 mm
                           FRAMES$SENSOR_WIDTH) 
FRAMES$SENSOR_HEIGHT <- ifelse(FRAMES$CAMERA == "Hasselblad L2D-20c Mavic3Pro", 
                            13.0 + 0.01 * 13.0, # Hasselblad 20MP 24mm Equivalent 1x Sensor Height = 13.0 mm
                            FRAMES$SENSOR_HEIGHT) 

FRAMES$SENSOR_WIDTH <- ifelse(FRAMES$CAMERA == "DJI FC4382 Mavic3Pro", 
                           10 - 0.03 * 10, # DJI 48MP Medium Telephoto 3x Zoom Sensor Width = 10 mm
                           FRAMES$SENSOR_WIDTH) 
FRAMES$SENSOR_HEIGHT <- ifelse(FRAMES$CAMERA == "DJI FC4382 Mavic3Pro", 
                            7.5 - 0.03 * 7.5, # DJI 48MP Medium Telephoto 3x Zoom Sensor Height = 7.5 mm
                            FRAMES$SENSOR_HEIGHT) 

FRAMES$SENSOR_WIDTH <- ifelse(FRAMES$CAMERA == "DJI FC4370 Mavic3Pro", 
                           6.4 - 0.15 * 6.4, # DJI 12MP Telephoto 7x Zoom Sensor Width = 6.4 mm
                           FRAMES$SENSOR_WIDTH) 
FRAMES$SENSOR_HEIGHT <- ifelse(FRAMES$CAMERA == "DJI FC4370 Mavic3Pro"  , 
                            5.8 - 0.15 * 5.8, # DJI 12MP Telephoto 7x Zoom Sensor Height = 5.8 mm
                            FRAMES$SENSOR_HEIGHT) 

# Convert SENSOR_WIDTH and SENSOR_HEIGHT from milimeters to meters
FRAMES$SENSOR_WIDTH <- FRAMES$SENSOR_WIDTH/1000
FRAMES$SENSOR_HEIGHT <- FRAMES$SENSOR_HEIGHT/1000


# RoMISSION_PLANe YAW from -180:180 to 0:360 used in previous calculations
FRAMES$YAW <- ifelse(FRAMES$YAW < 0, 360 + FRAMES$YAW, FRAMES$YAW)
FRAMES$YAW_AIRCRAFT <- ifelse(FRAMES$YAW_AIRCRAFT < 0, 360 + FRAMES$YAW_AIRCRAFT, FRAMES$YAW_AIRCRAFT)

# plot(FRAMES[FRAMES$FLIGHT == "F02","DATE_TIME"],FRAMES[FRAMES$FLIGHT == "F02","YAW_AIRCRAFT"],type = "p", ylim = c(-180,360))
# lines(FRAMES[FRAMES$FLIGHT == "F02","DATE_TIME"],FRAMES[FRAMES$FLIGHT == "F02","YAW_AIRCRAFT"],col = "black")
# lines(FRAMES[FRAMES$FLIGHT == "F02","DATE_TIME"],FRAMES[FRAMES$FLIGHT == "F02","YAW"],col = "red")
# points(FRAMES[FRAMES$FLIGHT == "F02","DATE_TIME"],FRAMES[FRAMES$FLIGHT == "F02","YAW"],col = "red")

# RoMISSION_PLANe PITCH from gimbal orienMISSION_PLANion frame to aircraft orienMISSION_PLANion frame (0 degrees = nadir)
FRAMES$PITCH <-  FRAMES$PITCH + 90


#Extract relevant date-time components
FRAMES$YEAR <- as.numeric(lubridate::year(FRAMES$DATE_TIME))
FRAMES$MONTH <- as.numeric(lubridate::month(FRAMES$DATE_TIME))
FRAMES$DAY <- as.numeric(lubridate::day(FRAMES$DATE_TIME))
FRAMES$HOUR <- as.numeric(lubridate::hour(FRAMES$DATE_TIME))
FRAMES$MIN <- as.numeric(lubridate::minute(FRAMES$DATE_TIME))
FRAMES$SEC <- as.numeric(lubridate::second(FRAMES$DATE_TIME))


# Synthesize a unique FLIGHT_ID by pasting together DATE 
# and the numeric FLIGHT number within each day
FRAMES$FLIGHT_ID <- paste(stringr::str_sub(FRAMES$DATE_TIME,1,10),
                       FRAMES$FLIGHT, sep = "_")

# Add a sequential ID column to reestablish original order after splits 
FRAMES$SEQ_ID <- 1:nrow(FRAMES)

# Split FRAMES into a list of data.frames by FLIGHT_ID
FRAMES <- split(FRAMES,FRAMES$FLIGHT_ID)

# For each entry in FRAMES list
for(i in 1:length(FRAMES)){
  
  # Copy the START and STOP of FLIGHT (i.e., first and last DATE_TIME) to every row
  FRAMES[[i]]$START <- FRAMES[[i]]$DATE_TIME[1]
  FRAMES[[i]]$STOP <- rev(FRAMES[[i]]$DATE_TIME)[1]
  
  # Copy the FIRST and LAST IMAGE name of FLIGHT to every row
  FRAMES[[i]]$FIRST_IMAGE <- FRAMES[[i]]$IMAGE_FILE[1]
  FRAMES[[i]]$LAST_IMAGE <- rev(FRAMES[[i]]$IMAGE_FILE)[1]
  
  # Calculate the TIME_DIFFerence
  # between sequential camera trigger events
  FRAMES[[i]]$TIME_DIFF[1] <- 0
  FRAMES[[i]]$TIME_DIFF[2:nrow(FRAMES[[i]])] <- difftime(time2 = FRAMES[[i]][1:(nrow(FRAMES[[i]])-1), c("DATE_TIME")],
                                                   time1 = FRAMES[[i]][2:nrow(FRAMES[[i]]), c("DATE_TIME")],
                                                   units = "secs")
  
  # Calculate the DISPLACEMENT distance
  # between sequential camera trigger events
  FRAMES[[i]]$DISPLACEMENT[1] <- 0
  FRAMES[[i]]$DISPLACEMENT[2:nrow(FRAMES[[i]])] <- geosphere::distGeo(p1 = FRAMES[[i]][1:(nrow(FRAMES[[i]])-1), c("LONG","LAT")],
                                                                p2 = FRAMES[[i]][2:nrow(FRAMES[[i]]), c("LONG","LAT")])
  
  # Calculate the total duration of each FLIGHT (as sum of sequential jumps)
  FRAMES[[i]]$TIME_DIFF_SUM  <- sum(FRAMES[[i]]$TIME_DIFF)
  
  # Calculate the total distance covered by each FLIGHT
  FRAMES[[i]]$DISPLACEMENT_SUM <- sum(FRAMES[[i]]$DISPLACEMENT)
  
}


# Recompile FRAMES into a single data.frame  
FRAMES <- do.call("rbind",FRAMES)

# Reestablish original order
FRAMES <- FRAMES[order(FRAMES$SEQ_ID),]

# Calculate the total duration of each FLIGHT (as difference between START and STOP)
FRAMES$DURATION <- as.numeric(difftime(time1 = FRAMES$STOP, 
                                    time2 = FRAMES$START, units = "mins"))



WORKING_DIR <- "/Users/trevor.joyce/Grad School/Research/1_2019_Gray_Whale_UAS_Survey/Analysis/GW_UAS_Survey_Analysis_v1.0/"

source(paste(WORKING_DIR,"GRID_CONVERGENCE_v1.0.R", sep = ""))

source(paste(WORKING_DIR,"YPR_to_OPK_v1.0.R", sep = ""))

source(paste(WORKING_DIR,"PIXEL_to_GROUND_COORD_v1.3.1.R", sep = ""))


for(i in 1:nrow(FRAMES)){
  
  
  # Calculate the grid convergence or the angular offset between true north and UTM grid north
  # at each FRAMES location. This allows the correction of the YAW parameter to orient
  # OMEGA, PHI, KAPPA angles to the UTM grid used in the calculation of ground coordinates.
  FRAMES[i,"YAW_GRID_N"] <- FRAMES[i,"YAW"] + GRID_CONVERGENCE(LONG = FRAMES[i,"LONG"],
                                                         LAT = FRAMES[i,"LAT"],
                                                         UTM_CRS = "+init=epsg:32611")
  
  # Calculate OMEGA,PHI,KAPPA angles from YAW, PITCH, ROLL outputs in FRAMES log and georef files
  FRAMES[i,c("OMEGA","PHI","KAPPA")] <- YPR_to_OPK(YAW = FRAMES[i,c("YAW_GRID_N")] ,
                                                PITCH = FRAMES[i,c("PITCH")],
                                                ROLL = FRAMES[i,c("ROLL")])
  
  
  for(j in 1:4){
    
    # Define the pixel coordinates of the frame corners relative to the center point at (0,0)
    # TL = top left, BL = bottom left, TR = top right, BR = bottom right
    VERT_temp <- data.frame(VERT = c("TL","BL","BR","TR"),
                            PIX_X = c(-FRAMES$PIXELS_X[i]/2,
                                      -FRAMES$PIXELS_X[i]/2,
                                      FRAMES$PIXELS_X[i]/2,
                                      FRAMES$PIXELS_X[i]/2),
                            PIX_Y = c(FRAMES$PIXELS_Y[i]/2,
                                      -FRAMES$PIXELS_Y[i]/2,
                                      -FRAMES$PIXELS_Y[i]/2,
                                      FRAMES$PIXELS_Y[i]/2))
    
    # Calculate GROUND_COORD for the corners of image footprint using PIXEL_to_GROUND_COORD
    GROUND_COORD_temp <- try(PIXEL_to_GROUND_COORD(PIXEL_COORD_X = VERT_temp$PIX_X[j],
                                                   PIXEL_COORD_Y = VERT_temp$PIX_Y[j],
                                                   LONG = FRAMES$LONG[i],
                                                   LAT = FRAMES$LAT[i],
                                                   ALT_MSL = FRAMES$ALT_AGL[i],
                                                   OMEGA = FRAMES$OMEGA[i],
                                                   PHI = FRAMES$PHI[i],
                                                   KAPPA = FRAMES$KAPPA[i],
                                                   PIXELS_X = FRAMES$PIXELS_X[i], 
                                                   SENSOR_WIDTH = FRAMES$SENSOR_WIDTH[i],
                                                   PIXELS_Y = FRAMES$PIXELS_Y[i], 
                                                   SENSOR_HEIGHT = FRAMES$SENSOR_HEIGHT[i],
                                                   FOCAL_LENGTH = FRAMES$FOCAL_LENGTH[i],
                                                   UTM_CRS = "+init=epsg:32611"))
    
    # Error handling
    if(class(GROUND_COORD_temp) == "try-error"){
      print(paste("try error in calc_mapped_Point;","i = ", i))
      next
    }
    
    # Report STATUS == 0 results
    if(GROUND_COORD_temp$STATUS == 0){
      print(paste("Vector does not intersect with plane (i.e., above horizon);","i = ", i))
      next
    }
    
    # Populate corner coordinates in FRAMES data.frame
    FRAMES[i,paste(VERT_temp$VERT[j],c("LONG","LAT"),sep = "_")] <- GROUND_COORD_temp$GROUND_COORD
    # FRAMES[i,paste(VERT_temp$VERT[j],c("X_OFFSET","Y_OFFSET"),sep = "_")] <- GROUND_COORD_temp$XYZ_OFFSETS[c(1,2)]
    
    # Close j loop
  }
  
  # Close i loop
}

# Clean up
remove(VERT_temp,GROUND_COORD_temp,i,j)



#### FRAMES_POLY ####

# Create a list with each list entry representing one SIGHTING
FRAMES_POLY <- split(FRAMES[!is.na(FRAMES$TL_LONG) & !is.na(FRAMES$BL_LONG)
                      & !is.na(FRAMES$BR_LONG) & !is.na(FRAMES$TR_LONG),],
                  FRAMES[!is.na(FRAMES$TL_LONG) & !is.na(FRAMES$BL_LONG)
                      & !is.na(FRAMES$BR_LONG) & !is.na(FRAMES$TR_LONG),"SEQ_ID"])

# For each list entry create a Lines object,
# containing a list containing one Line object
FRAMES_POLY <- lapply(FRAMES_POLY, function(x){
  
  sp::Polygons(list(sp::Polygon(coords = cbind(c(x$TL_LONG,x$BL_LONG,x$BR_LONG,x$TR_LONG),
                                               c(x$TL_LAT,x$BL_LAT,x$BR_LAT,x$TR_LAT)))),
               ID = x$FILE[1])
})

# convert list of Lines into a SpatialLines object
FRAMES_POLY <- sp::SpatialPolygons(FRAMES_POLY)

# Create a temporary data.frame from FLIGHTS
FRAMES_POLY_DF_temp <- FRAMES[!is.na(FRAMES$TL_LONG) & !is.na(FRAMES$BL_LONG)
                        & !is.na(FRAMES$BR_LONG) & !is.na(FRAMES$TR_LONG),]

# Re-define data.frame row names to match IDs from SpatialLines object
rownames(FRAMES_POLY_DF_temp) <- FRAMES_POLY_DF_temp$FILE

# create SpatialLinesDataFrame object
FRAMES_POLY <- sp::SpatialPolygonsDataFrame(FRAMES_POLY, FRAMES_POLY_DF_temp)

# define projection of SpatialLinesDataFrame object
sp::proj4string(FRAMES_POLY) <- sp::CRS("+init=epsg:4326")

#clean-up
remove(FRAMES_POLY_DF_temp)

# # Write KML version to display in
# rgdal::writeOGR(FRAMES_POLY[FRAMES_POLY$PITCH == 0,],
#                 dsn=paste("/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Data/Exported_Data/",
#                           paste("FRAMES_POLY",stringr::str_sub(FRAMES_POLY$DATE_TIME[1],1,10),sep = "_"),
#                           ".kml",sep = ""),
#                 layer=paste("FRAMES_POLY",stringr::str_sub(FRAMES_POLY$DATE_TIME[1],1,10),sep = "_"),
#                 driver="KML")
# 
# for(i in unique(FRAMES_POLY$FLIGHT_ID)){
# 
#   dir.create(paste("/Users/trevor.joyce/Grad School/Research/1_2019_Gray_Whale_UAS_Survey/Data/Exported Data","/",i,sep = ""))
# 
#   rgdal::writeOGR(FRAMES_POLY[!is.na(FRAMES_POLY$FLIGHT_ID) & FRAMES_POLY$FLIGHT_ID == i &
#                              FRAMES_POLY$PITCH == 0,],
#                   dsn=paste("/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Data/Exported_Data/",
#                             "FRAMES_POLY","_",i,".kml",sep = ""),
#                   layer=paste("FRAMES_POLY",i,sep = "_"),
#                   driver="KML")
# 
# }

#### INDIV ####

WORKING_DIR <- "/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Data/Marine_Turtle_UAS_Survey_Image_Review/Sightings"


INDIV_FILES_temp <- list.files(WORKING_DIR,
                               recursive = T,
                               full.names = T)

INDIV_FILES_temp <- INDIV_FILES_temp[stringr::str_sub(INDIV_FILES_temp,-13,-1)=="-Measured.csv"]

# INDIV_FILES_temp <- list.files(WORKING_DIR,
#                                pattern = "-Measured.csv",
#                                recursive = T,
#                                full.names = T)

#Create a data.frame to house invididual animal measurements
INDIV <- data.frame()

### Loop through the Measured files produced by ImageJ macro
for(i in INDIV_FILES_temp){
  
  # import ImageJ measurement table for a single image
  INDIV_temp <- read.csv(i,stringsAsFactors = F,header = T)
  
  # # Remove extraneous columns added by ImageJ (e.g., Angle, Length)
  # INDIV_temp <- INDIV_temp[,c("DIRECTORY","FILE_NAME","ANIMAL_ID",
  #                             "HEAD_X","HEAD_Y","TAIL_X","TAIL_Y",
  #                             "SPECIES","SURFACE","NEAR_SURFACE","CALF")]
  
  # append ImageJ measurement table for a single image to overall table
  INDIV <- rbind(INDIV,INDIV_temp)
  
  # Clean-up
  remove(INDIV_temp)
  gc()
  
}

# Clean-up
remove(i,INDIV_FILES_temp)


#Create a SEQ_ID code for reordering data after merging
INDIV$SEQ_ID = seq(1:nrow(INDIV))

FRAMES_temp <- FRAMES[,c("SEQ_ID","FLIGHT","DIR","FILE",
                   "DATE_TIME","NUMBER","FOCAL_LENGTH","ALT_MSL","ALT_AGL",
                   "PIXELS_X","PIXELS_Y","YAW","YAW_AIRCRAFT",
                   "PITCH","ROLL","LAT","LONG","CAMERA",
                   "SENSOR_WIDTH","SENSOR_HEIGHT","OMEGA","PHI","KAPPA")]

colnames(FRAMES_temp)[1] <- "FRAMES_SEQ_ID"


#Merge telemetry information with image information
INDIV <- merge(x = INDIV,
               y = FRAMES_temp, 
               by.x = "FILE_NAME", 
               by.y = "FILE", 
               all.x = T)


remove(FRAMES_temp)

INDIV <- INDIV[order(INDIV$SEQ_ID),]


for(i in 1:nrow(INDIV)){
  
  # Calculate GROUND_COORD for TL pixel coordinates using PIXEL_to_GROUND_COORD
  GROUND_COORD_TL_temp <- try(PIXEL_to_GROUND_COORD(PIXEL_COORD_X = INDIV$TL_x[i]-(INDIV$PIXELS_X[i]/2),
                                                      PIXEL_COORD_Y = abs(INDIV$TL_y[i]-INDIV$PIXELS_Y[i])-(INDIV$PIXELS_Y[i]/2),
                                                      LONG = INDIV$LONG[i],
                                                      LAT = INDIV$LAT[i],
                                                      ALT_MSL = INDIV$ALT_AGL[i],
                                                      OMEGA = INDIV$OMEGA[i],
                                                      PHI = INDIV$PHI[i],
                                                      KAPPA = INDIV$KAPPA[i],
                                                      PIXELS_X = INDIV$PIXELS_X[i], 
                                                      SENSOR_WIDTH = INDIV$SENSOR_WIDTH[i],
                                                      PIXELS_Y = INDIV$PIXELS_Y[i], 
                                                      SENSOR_HEIGHT = INDIV$SENSOR_HEIGHT[i],
                                                      FOCAL_LENGTH = INDIV$FOCAL_LENGTH[i],
                                                      UTM_CRS = "+init=epsg:32611"))
  
  
  # Calculate GROUND_COORD for BR pixel coordinates using PIXEL_to_GROUND_COORD
  GROUND_COORD_BR_temp <- try(PIXEL_to_GROUND_COORD(PIXEL_COORD_X = INDIV$BR_x[i]-(INDIV$PIXELS_X[i]/2),
                                                      PIXEL_COORD_Y = abs(INDIV$BR_y[i]-INDIV$PIXELS_Y[i])-(INDIV$PIXELS_Y[i]/2),
                                                      LONG = INDIV$LONG[i],
                                                      LAT = INDIV$LAT[i],
                                                      ALT_MSL = INDIV$ALT_AGL[i],
                                                      OMEGA = INDIV$OMEGA[i],
                                                      PHI = INDIV$PHI[i],
                                                      KAPPA = INDIV$KAPPA[i],
                                                      PIXELS_X = INDIV$PIXELS_X[i], 
                                                      SENSOR_WIDTH = INDIV$SENSOR_WIDTH[i],
                                                      PIXELS_Y = INDIV$PIXELS_Y[i], 
                                                      SENSOR_HEIGHT = INDIV$SENSOR_HEIGHT[i],
                                                      FOCAL_LENGTH = INDIV$FOCAL_LENGTH[i],
                                                      UTM_CRS = "+init=epsg:32611"))
  
  
  # Error handling
  if(class(GROUND_COORD_TL_temp) == "try-error"){
    print(paste("try error in calc_mapped_Point;","i = ", i))
    next
  }
  
  # # Report STATUS == 0 results
  # if(GROUND_COORD_TL_temp$STATUS == 0){
  #   print(paste("Vector does not intersect with plane (i.e., above horizon);","i = ", i))
  #   next
  # }
  
  # Populate corner coordinates in FRAMES data.frame
  INDIV[i,c("LONG_TL","LAT_TL")] <- GROUND_COORD_TL_temp$GROUND_COORD
  INDIV[i,c("LONG_BR","LAT_BR")] <- GROUND_COORD_BR_temp$GROUND_COORD
  
  # Close i loop  
}

# Clean up
remove(GROUND_COORD_TL_temp,
       GROUND_COORD_BR_temp,i)

# Calculate the midpoint (MID) coordinates for each individual

INDIV$LONG_MID <- (INDIV$LONG_TL + INDIV$LONG_BR)/2

INDIV$LAT_MID <- (INDIV$LAT_TL + INDIV$LAT_BR)/2


#Merge SIGHTING information with INDIV geometry information
INDIV <- merge(x = INDIV,
               y = SIGHTINGS[,c("FILE","TURTLE_CERTAINTY_ALL","SPECIES_ALL","DEPTH_ALL",
                                "TURTLE_CERTAINTY_TJ","SPECIES_TJ","DEPTH_TJ",
                                "TURTLE_CERTAINTY_TE","SPECIES_TE","DEPTH_TE")], 
               by.x = "FILE_NAME", 
               by.y = "FILE", 
               all.x = T)

#### INDIV_POLY ####

# Create a list with each list entry representing one INDIV bounding box
INDIV_POLY <- split(INDIV[!is.na(INDIV$LONG_TL) & !is.na(INDIV$LONG_BR),],
                    INDIV[!is.na(INDIV$LONG_TL) & !is.na(INDIV$LONG_BR),"SEQ_ID"])

# For each list entry create a Lines object,
# containing a list containing one Line object
INDIV_POLY <- lapply(INDIV_POLY, function(x){
  
  sp::Polygons(list(sp::Polygon(coords = cbind(c(x$LONG_TL,x$LONG_BR,x$LONG_BR,x$LONG_TL),
                                               c(x$LAT_TL,x$LAT_TL,x$LAT_BR,x$LAT_BR)))),
               ID = x$ANIMAL_ID[1])
})

# convert list of Lines into a SpatialLines object
INDIV_POLY <- sp::SpatialPolygons(INDIV_POLY)

# Create a temporary data.frame from FLIGHTS
INDIV_POLY_DF_temp <- INDIV[!is.na(INDIV$LONG_TL) & !is.na(INDIV$LONG_BR),]

# Re-define data.frame row names to match IDs from SpatialLines object
rownames(INDIV_POLY_DF_temp) <- INDIV_POLY_DF_temp$ANIMAL_ID

# create SpatialLinesDataFrame object
INDIV_POLY <- sp::SpatialPolygonsDataFrame(INDIV_POLY, INDIV_POLY_DF_temp)

# define projection of SpatialLinesDataFrame object
sp::proj4string(INDIV_POLY) <- sp::CRS("+init=epsg:4326")

#clean-up
remove(INDIV_POLY_DF_temp)

# # Write KML version to display in
# rgdal::writeOGR(INDIV_POLY[INDIV_POLY$PITCH == 0,],
#                 dsn=paste("/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_INDIV/Data/Exported_Data/",
#                           paste("INDIV_POLY",stringr::str_sub(INDIV_POLY$DATE_TIME[1],1,10),sep = "_"),
#                           ".kml",sep = ""),
#                 layer=paste("INDIV_POLY",stringr::str_sub(INDIV_POLY$DATE_TIME[1],1,10),sep = "_"),
#                 driver="KML")
# 
# for(i in unique(INDIV_POLY$FLIGHT_ID)){
# 
#   dir.create(paste("/Users/trevor.joyce/Grad School/Research/1_2019_Gray_Whale_INDIV_Survey/Data/Exported Data","/",i,sep = ""))
# 
#   rgdal::writeOGR(INDIV_POLY[!is.na(INDIV_POLY$FLIGHT_ID) & INDIV_POLY$FLIGHT_ID == i &
#                              INDIV_POLY$PITCH == 0,],
#                   dsn=paste("/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_INDIV/Data/Exported_Data/",
#                             "INDIV_POLY","_",i,".kml",sep = ""),
#                   layer=paste("INDIV_POLY",i,sep = "_"),
#                   driver="KML")
# 
# }

#### INDIV_POINTS ####

INDIV_POINTS <- INDIV

sp::coordinates(INDIV_POINTS) <- ~ LONG_MID + LAT_MID

# define projection of SpatialLinesDataFrame object
sp::proj4string(INDIV_POINTS) <- sp::CRS("+init=epsg:4326")



#### UAS ####

WORKING_DIR <- "/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Data/Marine_Turtle_UAS_Survey_Image_Review/Logs"

# Create a list of all the files in the Working Directory
UAS_FILES_temp <- list.files(WORKING_DIR,
                             recursive = T,
                             full.names = T)

# Select the files found in the "Labeled_Logs" folder corresponding to each day of flying
UAS_FILES_temp <- UAS_FILES_temp[stringr::str_sub(UAS_FILES_temp,-4,-1)==".csv"]

# Create a data.frame to house all the UAS log data
UAS <- data.frame()

# Loop through UAS log files and append data to UAS
for(i in UAS_FILES_temp){
  
  # Read UAS log file
  UAS_temp <- read.csv(i,stringsAsFactors = F,header = T,skip = 0)
  
  # Add columns for full directory, file, and flight specification from the log directory string
  UAS_temp$LOG_DIR <- i
  UAS_temp$LOG_FILE <- rev(stringr::str_split(i, pattern = "/")[[1]])[1]
  UAS_temp$FLIGHT <- rev(stringr::str_split(i, pattern = "/")[[1]])[2]
  
  # append data to UAS
  UAS <- plyr::rbind.fill(UAS,UAS_temp)
  
  # Progress indicator
  print(i)
  
  # Clean-up
  remove(UAS_temp)
  gc()
  
}


# Clean-up
remove(i, UAS_FILES_temp)


# # Create character string DATE_TIME
# UAS$DATE_TIME <- paste(UAS$CUSTOM.date..local.,
#                            stringr::str_sub(UAS$CUSTOM.updateTime..local., 1,11))
# 
# # Format DATE_TIME as POSIXlt (avoids millisecond rounding errors with POSIXct)
# UAS$DATE_TIME <-as.POSIXlt(strptime(UAS$DATE_TIME, format = "%m/%d/%Y %H:%M:%OS", tz="America/Los_Angeles")) - lubridate::hours(3)
# UAS$DATE_TIME <- as.POSIXlt(strptime(UAS$LOG_FILE, format = "DJIFlightRecord_%Y-%m-%d_[%H-%M-%S].csv", tz="America/Los_Angeles")) + lubridate::seconds(UAS$OSD.flyTime..s.)
UAS$DATE_TIME <- as.POSIXct(strptime(UAS$datetime.utc.,format = "%Y-%m-%d %H:%M:%S",tz="GMT"))

# Convert GMT time to local time
UAS$DATE_TIME <- lubridate::with_tz(UAS$DATE_TIME, tzone = "America/Los_Angeles") 

# Add a sequential ID column to reestablish original order after splits 
UAS$SEQ_ID <- 1:nrow(UAS)

# Split UAS into a list of data.frames by FLIGHT_ID
UAS <- split(UAS,UAS$FLIGHT)

# For each entry in UAS list
for(i in 1:length(UAS)){
  
  # Copy the START and STOP of FLIGHT (i.e., first and last DATE_TIME) to every row
  UAS[[i]]$START <- UAS[[i]]$DATE_TIME[1]
  UAS[[i]]$STOP <- rev(UAS[[i]]$DATE_TIME)[1]
  
}

# Recompile UAS into a single data.frame  
UAS <- do.call("rbind",UAS)

# Reestablish original order
UAS <- UAS[order(UAS$SEQ_ID),]


# Extract UAS LONG, LAT, and ALT
UAS$LONG <- UAS$longitude
UAS$LAT <- UAS$latitude
UAS$ALT <- UAS$height_above_takeoff.feet. / 3.28084 


#### UAS_ABREV ####

UAS_ABREV <- data.frame()

for(j in unique(UAS$FLIGHT)){
  
  # Create animated KML of UAS track during each flight
  UAS_ABREV_temp <- UAS[UAS$FLIGHT==j,]   
  
  UAS_ABREV_temp <- UAS_ABREV_temp[seq(1,nrow(UAS_ABREV_temp),by = 25),] 
  
  UAS_ABREV <- rbind(UAS_ABREV,UAS_ABREV_temp)
  
}

#Clean up
remove(j,UAS_ABREV_temp)



#### Flight KML #####


for(j in unique(FRAMES$FLIGHT)){
  
  sink(file=paste("/Users/trevor.joyce/Grad School/Research/1_2022_Marine_Turtle_UAS/Figures/Marine_Turtle_UAS_Survey_Image_Review/",
                  "Flight_",j,".kml", sep = ""))
  
  cat(c('<?xml version="1.0" encoding="UTF-8"?>\n',
        '<kml xmlns="http://www.opengis.net/kml/2.2">\n',
        '\n',
        '  <Document>\n',
        '  <name>Flight_',j,'</name>\n',
        '\n',
        '\n'))
  
  
  ##### FRAMES #####
  
  # Create a top-level folder with line segments representing UAS progress from DATE_TIME [i-1] to DATE_TIME [i]
  cat(c('  <Folder>\n',
        paste('  <name>FRAMES_',j,'</name>\n',sep = ""),
        '\n'))
  
  FRAMES_temp <- FRAMES[FRAMES$FLIGHT==j 
                        & FRAMES$PITCH <= 5,]   
  
  for(i in 2:nrow(FRAMES_temp)){
    
    cat(c('    <Placemark>\n',
          
          # Define the metadata that will display when you click on the line segment
          paste('\n',
                '      <ExtendedData><SchemaData>\n',
                '        <SimpleData name="FLIGHT">',FRAMES_temp$FLIGHT[i],'</SimpleData>\n',
                '        <SimpleData name="FILE">',FRAMES_temp$FILE[i],'</SimpleData>\n',
                '        <SimpleData name="DATE_TIME">',FRAMES_temp$DATE_TIME[i],'</SimpleData>\n',
                '      </SchemaData></ExtendedData>\n',
                '\n',sep = ""),
          
          paste('      <TimeSpan><begin>',
                lubridate::format_ISO8601(x = FRAMES_temp$DATE_TIME[i],usetz = F),'Z',
                '</begin><end>',
                lubridate::format_ISO8601(x = UAS[UAS$FLIGHT==j,"STOP"][1] + lubridate::seconds(30),usetz = F),'Z',
                '</end></TimeSpan>\n',sep = ""),
          '\n',
          
          '      <LineString>\n',
          '        <altitudeMode>absolute</altitudeMode>\n',
          '        <coordinates>\n',
          paste('        ',FRAMES_temp$TL_LONG[i],',',FRAMES_temp$TL_LAT[i],',',0,'\n',sep=""),
          paste('        ',FRAMES_temp$BL_LONG[i],',',FRAMES_temp$BL_LAT[i],',',0,'\n',sep=""),
          paste('        ',FRAMES_temp$BR_LONG[i],',',FRAMES_temp$BR_LAT[i],',',0,'\n',sep=""),
          paste('        ',FRAMES_temp$TR_LONG[i],',',FRAMES_temp$TR_LAT[i],',',0,'\n',sep=""),
          paste('        ',FRAMES_temp$TL_LONG[i],',',FRAMES_temp$TL_LAT[i],',',0,'\n',sep=""),
          '        </coordinates>\n',
          '      </LineString>\n',
          '\n'))
    

      cat(c('      <Style>\n',
            '        <LineStyle>\n',
            '          <color>#ffababab</color>\n',
            '          <width>1</width>\n',
            '        </LineStyle>\n',
            '     </Style>\n',        
            '\n'))  
    
    cat(c('    </Placemark>\n',
          '\n',
          '\n',
          '\n'))
  }
  
  # Close Folder "FRAMES"  
  cat(c('</Folder>\n',
        '\n'))

  
  #### UAS_Track #####
  
  # Create a top-level folder with line segments representing UAS progress from DATE_TIME [i-1] to DATE_TIME [i]
  cat(c('  <Folder>\n',
        paste('  <name>UAS_Track_',j,'</name>\n',sep = ""),
        '\n'))
  
  UAS_ABREV_temp <- UAS_ABREV[UAS_ABREV$FLIGHT==j,]   
  
  for(i in 2:nrow(UAS_ABREV_temp)){
    
    cat(c('    <Placemark>\n',
          
          # Define the metadata that will display when you click on the line segment
          paste('\n',
                '      <ExtendedData><SchemaData>\n',
                '        <SimpleData name="FLIGHT">',UAS_ABREV_temp$FLIGHT[i],'</SimpleData>\n',
                '        <SimpleData name="DATE_TIME">',UAS_ABREV_temp$DATE_TIME[i],'</SimpleData>\n',
                '        <SimpleData name="LONG">',UAS_ABREV_temp$LONG[i],'</SimpleData>\n',
                '        <SimpleData name="LAT">',UAS_ABREV_temp$LAT[i],'</SimpleData>\n',
                '      </SchemaData></ExtendedData>\n',
                '\n',sep = ""),
          
          paste('      <TimeSpan><begin>',
                lubridate::format_ISO8601(x = UAS_ABREV_temp$DATE_TIME[i],usetz = F),'Z',
                '</begin><end>',
                lubridate::format_ISO8601(x = UAS[UAS$FLIGHT==j,"STOP"][1] + lubridate::seconds(30),usetz = F),'Z',
                '</end></TimeSpan>\n',sep = ""),
          '\n',
          
          '      <LineString>\n',
          '        <altitudeMode>absolute</altitudeMode>\n',
          '        <coordinates>\n',
          paste('        ',UAS_ABREV_temp$LONG[(i-1)],',',UAS_ABREV_temp$LAT[(i-1)],',',UAS_ABREV_temp$ALT[(i-1)],'\n',sep=""),
          paste('        ',UAS_ABREV_temp$LONG[i],',',UAS_ABREV_temp$LAT[i],',',UAS_ABREV_temp$ALT[i],'\n',sep=""),
          '        </coordinates>\n',
          '      </LineString>\n',
          '\n'))
      
      cat(c('      <Style>\n',
            '        <LineStyle>\n',
            '          <color>#ff06ff21</color>\n',
            '          <width>2</width>\n',
            '        </LineStyle>\n',
            '     </Style>\n',        
            '\n'))  
    
    cat(c('    </Placemark>\n',
          '\n',
          '\n',
          '\n'))
  }
  
  # Close Folder "UAS_Tracks"  
  cat(c('</Folder>\n',
        '\n'))
  
  

  # #### UAS_Points ####
  # 
  # # Create a top-level folder with line segments representing UAS progress from DATE_TIME [i-1] to DATE_TIME [i]
  # cat(c('  <Folder>\n',
  #       '\n',
  #       paste('  <name>','UAS_Points_',j,'</name>\n', sep = ""),
  #       '\n',
  #       '\n',
  #       '    <Style id="sn_placemark_diamond_UAS">\n',
  #       '      <IconStyle>\n',
  #       '        <color>#ff06ff21</color>\n',
  #       '        <scale>1.0</scale>\n',
  #       '        <Icon>\n',
  #       '          <href>http://maps.google.com/mapfiles/kml/shapes/open-diamond.png</href>\n',
  #       '        </Icon>\n',
  #       '      </IconStyle>\n',
  #       '      <LabelStyle>\n',
  #       '        <scale>0.8</scale>\n',
  #       '      </LabelStyle>\n',
  #       '    </Style>\n',
  #       '\n',
  #       '\n'))
  # 
  # cat(c('    <Placemark>\n',
  #       paste('      <name>',"FLIGHT: ",UAS_ABREV_temp$FLIGHT[1],'</name>\n', sep = ""),
  #       
  #       
  #       
  #       # Define the metadata that will display when you click on the point marker
  #       paste('\n',
  #             '      <ExtendedData><SchemaData>\n',
  #             '        <SimpleData name="FLIGHT">',UAS_ABREV_temp$FLIGHT[i],'</SimpleData>\n',
  #             '        <SimpleData name="DATE_TIME">',UAS_ABREV_temp$DATE_TIME[i],'</SimpleData>\n',
  #             '        <SimpleData name="LONG">',UAS_ABREV_temp$LONG[i],'</SimpleData>\n',
  #             '        <SimpleData name="LAT">',UAS_ABREV_temp$LAT[i],'</SimpleData>\n',
  #             '      </SchemaData></ExtendedData>\n',
  #             '\n',sep = ""),
  #       
  #       
  #       
  #       paste('      <Point>\n',
  #             '        <altitudeMode>absolute</altitudeMode>\n',
  #             '        <coordinates>\n',
  #             paste('        ',UAS_ABREV_temp$LONG[1],',',UAS_ABREV_temp$LAT[1],',',UAS_ABREV_temp$ALT[1],'\n',sep=""),
  #             '        </coordinates>\n',
  #             '      </Point>\n',sep=""),
  #       
  #       
  #       paste('      <TimeSpan><begin>',
  #             lubridate::format_ISO8601(x = UAS_ABREV_temp$DATE_TIME[1],usetz = F),'Z',
  #             '</begin><end>',
  #             lubridate::format_ISO8601(x = UAS[UAS$FLIGHT==j,"STOP"][1] + lubridate::seconds(30),usetz = F),'Z',
  #             '</end></TimeSpan>\n',sep = ""),
  #       
  #       '       <styleUrl>#sn_placemark_diamond_UAS</styleUrl>',
  #       
  #       '    </Placemark>\n',
  #       '\n')) 
  # 
  # for(i in 2:nrow(UAS_ABREV_temp)){
  #   cat(c('    <Placemark>\n',
  #         # paste('      <name>',"UAS:",substr(as.character(UAS_ABREV_temp$DATE_TIME[i]),12,19),'</name>\n', sep = ""),
  #         paste('      <Point>\n',
  #               '        <altitudeMode>absolute</altitudeMode>\n',
  #               '        <coordinates>\n',
  #               paste('        ',UAS_ABREV_temp$LONG[i],',',UAS_ABREV_temp$LAT[i],',',UAS_ABREV_temp$ALT[i],'\n',sep=""),
  #               '        </coordinates>\n',
  #               '      </Point>\n',sep=""),
  #         paste('      <TimeSpan><begin>',
  #               lubridate::format_ISO8601(x = UAS_ABREV_temp$DATE_TIME[i],usetz = F),'Z',
  #               '</begin><end>',
  #               lubridate::format_ISO8601(x = UAS_ABREV_temp$DATE_TIME[i+1],usetz = F),'Z',
  #               '</end></TimeSpan>\n',sep = ""),
  #         '       <styleUrl>#sn_placemark_diamond_UAS</styleUrl>',
  #         '    </Placemark>\n',
  #         '\n'))
  # }
  # 
  # # Close Folder "UAS_Points"
  # cat(c('</Folder>\n',
  #       '\n'))
  
  
  #### INDIV (Yes) #####
  
  # Create an animated KML of observed and simulated visual survey INDIV during each flight
  
  # Select all the simulated SIGHTING records from 5 min before to 5 min after FLIGHT[j]
  INDIV_temp <- INDIV[INDIV$FLIGHT==j 
                      & !is.na(INDIV$TURTLE_CERTAINTY_ALL)
                      & INDIV$TURTLE_CERTAINTY_ALL == "Yes", ]
  
  if(nrow(INDIV_temp) > 0){

  # Create a top-level folder with with SIGHTINGs
  cat(c('  <Folder>\n',
        '\n',
        paste('  <name>INDIV_Turtle_',j,'</name>\n', sep = ""),
        '\n',
        '\n'))
    
    cat(c('    <Style id="sn_placemark_circle_OBS_Yes">\n',
          '      <IconStyle>\n',
          '        <color>#ff0600f5</color>\n',
          '        <scale>0.8</scale>\n',
          '        <Icon>\n',
          '          <href>http://maps.google.com/mapfiles/kml/shapes/triangle.png</href>\n',
          '        </Icon>\n',
          '      </IconStyle>\n',
          '      <LabelStyle>\n',
          '        <scale>0.8</scale>\n',
          '      </LabelStyle>\n',
          '    </Style>\n',
          '\n'))
    
  for(i in 1:nrow(INDIV_temp)){
    
      cat(c('    <Placemark>\n',
            
            # # Define the name that will display next to point (S followed by SIGHTING_ID number followed by Time)
            # paste('      <name>',"S",stringr::str_split_fixed(INDIV_temp$ANIMAL_ID[i],pattern = "_",n= 2)[,2],": ",
            #       substr(as.character(INDIV_temp$DATE_TIME[i]),12,19),'</name>\n', sep = ""),
            
            
            # Define the metadata that will display when you click on a point
            paste('\n',
                  '      <ExtendedData><SchemaData>\n',
                  '        <SimpleData name="ANIMAL_ID">',INDIV_temp$ANIMAL_ID[i],'</SimpleData>\n',
                  '        <SimpleData name="DATE_TIME">',INDIV_temp$DATE_TIME[i],'</SimpleData>\n',
                  '        <SimpleData name="TURTLE_CERTAINTY">',INDIV_temp$TURTLE_CERTAINTY_ALL,'</SimpleData>\n',
                  '        <SimpleData name="SPECIES">',INDIV_temp$SPECIES_ALL[i],'</SimpleData>\n',
                  '        <SimpleData name="DEPTH">',INDIV_temp$DEPTH_ALL[i],'</SimpleData>\n',
                  '        <SimpleData name="LONG">',INDIV_temp$LONG[i],'</SimpleData>\n',
                  '        <SimpleData name="LAT">',INDIV_temp$LAT[i],'</SimpleData>\n',
                  '      </SchemaData></ExtendedData>\n', sep = ""),
            
            # Define the geometry of the Placemark (Point) and specify coordinates
            # (altitude set to 3m absolute to avoid getting lost in surface clutter)
            paste('      <Point>\n',
                  '        <altitudeMode>absolute</altitudeMode>\n',
                  '        <coordinates>\n',
                  paste('        ',INDIV_temp$LONG_MID[i],',',INDIV_temp$LAT_MID[i],',',3,'\n',sep=""),
                  '        </coordinates>\n',
                  '      </Point>\n',sep=""),
            
            # Define TimeSpan that the Placemark will be displayed starting at DATE_TIME[i] 
            # and extending until the last timestamp in INDIV_temp (i.e., 5 min after end of Flight)
            paste('      <TimeSpan><begin>',
                  lubridate::format_ISO8601(x = INDIV_temp$DATE_TIME[i],usetz = F),'Z',
                  '</begin><end>',
                  lubridate::format_ISO8601(x = UAS[UAS$FLIGHT==j,"STOP"][1] + lubridate::seconds(30),usetz = F),'Z',
                  '</end></TimeSpan>\n',sep = "")))
      
      # Define style of Placemark referring back to style definition at the folder level 
      cat(c('       <styleUrl>#sn_placemark_circle_OBS_Yes</styleUrl>'))
      
      
      # Close the Placemark
      cat(c('    </Placemark>\n',
            '\n'))
      
      
    } #i
  
  # Close top level folder "Sightings_[j]"
  cat(c('</Folder>\n',
        '\n'))  
  
  } # if(nrow(INDIV_temp) > 0) 
  
  remove(INDIV_temp)
  
  #### INDIV (Maybe) #####
  
  # Create an animated KML of observed and simulated visual survey INDIV during each flight
  
  # Select all the simulated SIGHTING records from 5 min before to 5 min after FLIGHT[j]
  INDIV_temp <- INDIV[INDIV$FLIGHT==j 
                      & !is.na(INDIV$TURTLE_CERTAINTY_ALL)
                      & INDIV$TURTLE_CERTAINTY_ALL == "Maybe", ]
  
  if(nrow(INDIV_temp) > 0){
  
  # Create a top-level folder with with SIGHTINGs
  cat(c('  <Folder>\n',
        '\n',
        paste('  <name>INDIV_Maybe_',j,'</name>\n', sep = ""),
        '\n',
        '\n'))
  
  cat(c('    <Style id="sn_placemark_circle_OBS_Maybe">\n',
        '      <IconStyle>\n',
        '        <color>#fff8006a</color>\n',
        '        <scale>0.8</scale>\n',
        '        <Icon>\n',
        '          <href>http://maps.google.com/mapfiles/kml/shapes/triangle.png</href>\n',
        '        </Icon>\n',
        '      </IconStyle>\n',
        '      <LabelStyle>\n',
        '        <scale>0.8</scale>\n',
        '      </LabelStyle>\n',
        '    </Style>\n',
        '\n'))
  
  for(i in 1:nrow(INDIV_temp)){
    
    cat(c('    <Placemark>\n',
          
          # # Define the name that will display next to point (S followed by SIGHTING_ID number followed by Time)
          # paste('      <name>',"S",stringr::str_split_fixed(INDIV_temp$ANIMAL_ID[i],pattern = "_",n= 2)[,2],": ",
          #       substr(as.character(INDIV_temp$DATE_TIME[i]),12,19),'</name>\n', sep = ""),
          
          
          # Define the metadata that will display when you click on a point
          paste('\n',
                '      <ExtendedData><SchemaData>\n',
                '        <SimpleData name="ANIMAL_ID">',INDIV_temp$ANIMAL_ID[i],'</SimpleData>\n',
                '        <SimpleData name="DATE_TIME">',INDIV_temp$DATE_TIME[i],'</SimpleData>\n',
                '        <SimpleData name="TURTLE_CERTAINTY">',INDIV_temp$TURTLE_CERTAINTY_ALL,'</SimpleData>\n',
                '        <SimpleData name="SPECIES">',INDIV_temp$SPECIES_ALL[i],'</SimpleData>\n',
                '        <SimpleData name="DEPTH">',INDIV_temp$DEPTH_ALL[i],'</SimpleData>\n',
                '        <SimpleData name="LONG">',INDIV_temp$LONG[i],'</SimpleData>\n',
                '        <SimpleData name="LAT">',INDIV_temp$LAT[i],'</SimpleData>\n',
                '      </SchemaData></ExtendedData>\n', sep = ""),
          
          # Define the geometry of the Placemark (Point) and specify coordinates
          # (altitude set to 3m absolute to avoid getting lost in surface clutter)
          paste('      <Point>\n',
                '        <altitudeMode>absolute</altitudeMode>\n',
                '        <coordinates>\n',
                paste('        ',INDIV_temp$LONG_MID[i],',',INDIV_temp$LAT_MID[i],',',3,'\n',sep=""),
                '        </coordinates>\n',
                '      </Point>\n',sep=""),
          
          # Define TimeSpan that the Placemark will be displayed starting at DATE_TIME[i] 
          # and extending until the last timestamp in INDIV_temp (i.e., 5 min after end of Flight)
          paste('      <TimeSpan><begin>',
                lubridate::format_ISO8601(x = INDIV_temp$DATE_TIME[i],usetz = F),'Z',
                '</begin><end>',
                lubridate::format_ISO8601(x = UAS[UAS$FLIGHT==j,"STOP"][1] + lubridate::seconds(30),usetz = F),'Z',
                '</end></TimeSpan>\n',sep = "")))
    
    # Define style of Placemark referring back to style definition at the folder level 
    cat(c('       <styleUrl>#sn_placemark_circle_OBS_Maybe</styleUrl>'))
    
    
    # Close the Placemark
    cat(c('    </Placemark>\n',
          '\n'))
    
    
  } #i
  
  # Close top level folder "Sightings_[j]"
  cat(c('</Folder>\n',
        '\n'))  
  
  
  } # if(nrow(INDIV_temp) > 0) 
  
  remove(INDIV_temp)
  
  # Close "Flight" document
  cat(c('</Document>\n',
        '</kml>\n'))
  
  sink()
} #j 



# Clean-up
remove(i,j,k,UAS_ABREV_temp,FRAMES_temp,INDIV_temp)






