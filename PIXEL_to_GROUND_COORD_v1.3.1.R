# PIXEL_to_GROUND_COORD
# 
# Function to take x y pixel coordinates from a tilted photograph axis system 
# and transform them into X Y coordinates in a ground coordinate system 
# using location (LONG,LAT), altitude (ALT_MSL), and orientation outputs in 
# the PATB convention (OMEGA,PHI,KAPPA) from GPS and inertial motion sensors on a UAS.
#
# This script is based on Python functions (intersectionPlane and calc_mapped_Point)
# written by Martin Wieser and has been translated to R by Trevor Joyce.
# May 14, 2021


# # Test values for troubleshooting
# PIXEL_COORD_X = 6058/2
# PIXEL_COORD_Y = 4012/2
# LONG = -121.9256868
# LAT = 36.4390031
# ALT_MSL = 72.0
# OMEGA = 1.60
# PHI = 0.00
# KAPPA = 145.55
# SENSOR_WIDTH = 0.0235
# SENSOR_HEIGHT = 0.0156
# PIXELS_X = 6058
# PIXELS_Y = 4012
# FOCAL_LENGTH = 0.009

# def calc_mapped_Point(projection_center, omega, phi, kappa, pixel_size, point_in_pixel, plane_point):
PIXEL_to_GROUND_COORD <- function(PIXEL_COORD_X,PIXEL_COORD_Y,
                              LONG,LAT,ALT_MSL,
                              OMEGA,PHI,KAPPA,
                              PIXELS_X = 6058, SENSOR_WIDTH = 0.0235, #23.5 mm sensor width
                              PIXELS_Y = 4012, SENSOR_HEIGHT = 0.0156, #15.6 mm sensor height
                              FOCAL_LENGTH = 0.009,
                              UTM_CRS = "+init=epsg:32610" #UTM Zone 10N (UTM Zone 11N (San Diego) = "+init=epsg:32611")
                              ){
  
  
  # Take pixel coordinates and convert to m from the center of the sensor
  x <- (SENSOR_WIDTH/2) * (PIXEL_COORD_X/(PIXELS_X/2))
  y <- (SENSOR_HEIGHT/2) * (PIXEL_COORD_Y/(PIXELS_Y/2))
  
  # Define the X, Y, Z coordinates of the exposure station L 
  # where the aerial photo was taken in UTM Zone 10N and meters MSL
  X_L <- 0
  Y_L <- 0
  Z_L <- ALT_MSL
  
  # Define the parameter f as the FOCAL_LENGTH of the lens
  f <- FOCAL_LENGTH
  
  # Here would be your image extends
  # point_in_pixel = numpy.array([SENSOR_WIDTH / 2, SENSOR_HEIGHT / 2, -f])
  point_in_pixel = c(x, y, -f)
  
  # projection_center = numpy.array([0, 0, 72.0])  # could be UTM coordinates already
  projection_center = c(X_L, Y_L, Z_L)  # could be UTM coordinates already
  
  # plane_point = numpy.array([0, 0, 0])  # For example if you want 10meter abouve see it would be numpy.array([0,0,10.0])
  plane_point = c(0, 0, 0)  # For example if you wanted 10 meters above sea level it would be c(0,0,10.0)
  
  pixel_size = (SENSOR_WIDTH / PIXELS_X)
  
  
  # P_RAD = phi * numpy.pi / 180
  # O_RAD = omega * numpy.pi / 180
  # K_RAD = kappa * numpy.pi / 180
  P_RAD = PHI * pi / 180
  O_RAD = OMEGA * pi / 180
  K_RAD = KAPPA * pi / 180
  
  
  m11 = cos(P_RAD) * cos(K_RAD)
  m12 = sin(O_RAD) * sin(P_RAD) * cos(K_RAD) + cos(O_RAD) * sin(K_RAD)
  m13 = -cos(O_RAD) * sin(P_RAD) * cos(K_RAD) + sin(O_RAD) * sin(K_RAD)
  m21 = -cos(P_RAD) * sin(K_RAD)
  m22 = -sin(O_RAD) * sin(P_RAD) * sin(K_RAD) + cos(O_RAD) * cos(K_RAD)
  m23 = cos(O_RAD) * sin(P_RAD) * sin(K_RAD) + sin(O_RAD) * cos(K_RAD)
  m31 = sin(P_RAD)
  m32 = -sin(O_RAD) * cos(P_RAD)
  m33 = cos(O_RAD) * cos(P_RAD)
  
  # R = numpy.array([[m11, m12, m13], [m21, m22, m23], [m31, m32, m33]])
  R = matrix(data = c(m11, m12, m13, 
                      m21, m22, m23, 
                      m31, m32, m33), 
             nrow = 3, ncol = 3, byrow = T)
  
  # vector = R @ point_in_pixel
  vector = R %*% point_in_pixel
  
  # XYZ, status = intersectionPlane(vector, projection_center, plane_point)
  lineVEC = vector
  linePoint = projection_center
  planePoint = plane_point
  
  # def intersectionPlane(lineVEC, linePoint, planePoint):
  chek_direction = 0
  
  epsilon = 1e-6
  
  # Define plane
  #planeNormal = numpy.array([0, 0, 1])
  planeNormal = c(0, 0, 1)
  
  
  # Define ray
  # rayDirection = numpy.array([0, -1, -1])
  # rayPoint = numpy.array([0, 0, 10])  # Any point along the ray
  
  # ndotu = planeNormal.dot(lineVEC)
  ndotu <- planeNormal %*% lineVEC
  
  
  # if abs(ndotu) < epsilon:
  #   chek_direction = 1
  #   return numpy.array([0, 0, 0]), chek_direction
  # # print "no intersection or line is within plane"
  if(abs(ndotu) < epsilon){
    chek_direction = 1
    return(c(0, 0, 0), chek_direction)
    print("no intersection or line is within plane")
  }
  
  w = linePoint - planePoint
  
  
  # si = -planeNormal.dot(w) / ndotu
  si = -planeNormal%*%w / ndotu
  
  # Psi = w + si * lineVEC + planePoint
  Psi = w + lineVEC%*%si + planePoint
  
  # v1 = linePoint - Psi
  v1 = linePoint - Psi
  
  # v2 = linePoint - (linePoint + lineVEC)
  v2 = linePoint - (linePoint + lineVEC)
  
  # cosA = v1.dot(v2) / numpy.linalg.norm(v1) / numpy.linalg.norm(v2)
  cosA = as.vector(v1)%*%as.vector(v2) / norm(v1) / norm(v2)
  
  
  # if cosA < 0.9:
  #   chek_direction = 1
  # else:
  #   chek_direction = 0
  if(cosA < 0.9){
    chek_direction = 1
  }else{
    chek_direction = 0 
  }
  
  # XYZ, status = intersectionPlane(vector, projection_center, plane_point)
  XYZ = Psi
  status = chek_direction
  
  c = f / pixel_size
  X = XYZ[1] - projection_center[1]
  Y = XYZ[2] - projection_center[2]
  Z = XYZ[3] - projection_center[3]
  
  # R1 = R.T
  R1 = t(R)
  
  x = -c * (R1[1, 1] * X + R1[1, 2] * Y + R1[1, 3] * Z) / (R1[3, 1] * X + R1[3, 2] * Y + R1[3, 3] * Z)
  y = -c * (R1[2, 1] * X + R1[2, 2] * Y + R1[2, 3] * Z) / (R1[3, 1] * X + R1[3, 2] * Y + R1[3, 3] * Z)
  
  # Project LAT and LONG coordinates of the UAS 
  # in WGS84 geographic coordinates
  UAS_COORD <- data.frame(X = LONG,
                          Y = LAT,
                          Z = ALT_MSL)
  sp::coordinates(UAS_COORD) <- ~ X + Y
  sp::proj4string(UAS_COORD) <- sp::CRS("+init=epsg:4326")
  
  # Convert from WGS84 geographic coordinates 
  # to UTM Zone 10N coordinates in units of m 
  UAS_COORD_PROJ <- sp::spTransform(UAS_COORD, sp::CRS(UTM_CRS))
  UAS_COORD_PROJ <- as.data.frame(UAS_COORD_PROJ)
  colnames(UAS_COORD_PROJ) <- c("Z", "X", "Y")
  
  # Transform from X and Y offsets to  
  GROUND_COORD_PROJ <- data.frame(EASTING = X + UAS_COORD_PROJ$X,
                                  NORTHING = Y + UAS_COORD_PROJ$Y)
  
  # Project X and Y ground coordinates in UTM Zone 10N
  sp::coordinates(GROUND_COORD_PROJ) <- ~ EASTING + NORTHING
  sp::proj4string(GROUND_COORD_PROJ) <- sp::CRS(UTM_CRS)
  
  # Convert from UTM Zone 10N coordinates in m to WGS84 geographic coordinates
  GROUND_COORD <- sp::spTransform(GROUND_COORD_PROJ, sp::CRS("+init=epsg:4326"))
  GROUND_COORD <- as.data.frame(GROUND_COORD)
  colnames(GROUND_COORD) <- c("LONG","LAT")
  
  
  # Output pixel coordinates as a data frame 
  return(list(UAS_COORD = as.data.frame(UAS_COORD),
              UAS_COORD_PROJ = as.data.frame(UAS_COORD_PROJ),
              XYZ_OFFSETS = as.vector(XYZ),
              xy_COORD = c(2*x*pixel_size, 2*y*pixel_size),
              xy_PIXEL_COORD = c(x,y),
              GROUND_COORD_PROJ = as.data.frame(GROUND_COORD_PROJ),
              GROUND_COORD = as.data.frame(GROUND_COORD),
              STATUS = status))
  
}


# PIXEL_to_GROUND_COORD(PIXEL_COORD_X = c(6058/2),
#                       PIXEL_COORD_Y = c(4012/2),
#                       LONG = -121.9256868,
#                       LAT = 36.4390031,
#                       ALT_MSL = 72.0,
#                       OMEGA = 1.60,
#                       PHI = 0.00,
#                       KAPPA = 145.55)

