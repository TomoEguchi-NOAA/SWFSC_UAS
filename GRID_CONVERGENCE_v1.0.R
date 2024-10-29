#### GRID_CONVERGENCE ######

#### This function calculates Grid Convergence or the angular offset 
#### in degrees between true north and UTM grid north 
#### at a set of input coordinates in WGS84 geographic coordinates

GRID_CONVERGENCE <- function(LONG,LAT,UTM_CRS = "+init=epsg:32610"){
  
  # Create a data.frame with input coordinates
  COORD <- data.frame(X = LONG,
                      Y = LAT)
  
  # Create a dummy point directly N of input coordinate 
  # by a small latitudinal offset 0.00001 degrees)
  COORD[2,] <- COORD[1,]
  COORD[2,"Y"] <- COORD[2,"Y"] + 0.00001
  
  # Create a SpatialPointsDataFrame object in WGS84 geographic coordinates
  sp::coordinates(COORD) <- ~ X + Y
  sp::proj4string(COORD) <- sp::CRS("+init=epsg:4326")
  
  # Transform from WGS84 geographic coordinates 
  # to UTM coordinates in units of m (default set to UTM Zone 10N EPSG:32610)
  COORD_PROJ <- sp::spTransform(COORD, sp::CRS(UTM_CRS))
  COORD_PROJ <- as.data.frame(COORD_PROJ)
  colnames(COORD_PROJ)<- c("X","Y")
  
  # Calculate the angular offset in radians between true north and grid north 
  # at the input coordinate using the arctangent of the Easting offset (opposite) 
  # over the Northing offset (adjacent) in m
  GRID_CONVERGENCE <- atan2(COORD_PROJ[2,"X"] - COORD_PROJ[1,"X"],
                            COORD_PROJ[2,"Y"] - COORD_PROJ[1,"Y"]) 
  
  # Convert from radians to degrees
  GRID_CONVERGENCE <- GRID_CONVERGENCE * 180/pi
  
  return(GRID_CONVERGENCE)
  
}

# # Test
# GRID_CONVERGENCE(LONG = UAS$LONG[1],LAT = UAS$LAT[1])
# # [1] -0.6401142

