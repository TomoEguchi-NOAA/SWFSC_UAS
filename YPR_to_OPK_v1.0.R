# YPR_to_OPK
# 
# Function to take YAW, PITCH, ROLL from inertial motion sensors on a UAS
# and convert them to Euler angles the PATB convention (OMEGA,PHI,KAPPA)
# 
# This script is based on Python functions (rotFromEul and eulFromRot)
# from Github: https://github.com/nasa/georef_geocamutilweb/geocamUtil/registration.py
# and has been translated to R by Trevor Joyce.
# May 14, 2021


YPR_to_OPK <- function(YAW, PITCH, ROLL){
  
  # Convert YAW, PITCH, ROLL from degrees to radians
  yaw = YAW * pi/180
  pitch = PITCH * pi/180
  roll = ROLL * pi/180

  # # Convert YAW, PITCH, ROLL from degrees to radians
  # yaw = YAW * pi/180
  # pitch = ROLL * pi/180
  # roll = PITCH * pi/180
  
  
  # Calculation of Rotation matrix (rotMatrix) from YPR orientation 
  # based on the function rotFromEul
  # from Github: https://github.com/nasa/georef_geocamutilweb/geocamUtil/registration.py
  # 
  # def rotFromEul(roll, pitch, yaw):
  #     """
  #     Converts euler angles to a rotation matrix
  #     """
  # 
  # size = (3,3)
  # RX = np.zeros(size)
  # RY = np.zeros(size)
  # RZ = np.zeros(size)
  # 
  # # convert from array to matrix
  # RX = np.matrix(RX)
  # RY = np.matrix(RY)
  # RZ = np.matrix(RZ)
  
  # Create the empty matrices that define the rotations around each axis 
  # and will be used generate the rotation matrix (rotMatrix)
  RX = matrix(0, ncol = 3, nrow = 3)
  RY = matrix(0, ncol = 3, nrow = 3)
  RZ = matrix(0, ncol = 3, nrow = 3)
  
  
  # Define the rotation around the Z axis based on yaw
  # c = np.cos(yaw)
  # s = np.sin(yaw)
  # RZ[0,0] = c
  # RZ[0,1] = -s
  # RZ[1,0] = s
  # RZ[1,1] = c
  # RZ[2,2] = 1
  c = cos(yaw)
  s = sin(yaw)
  RZ[1,1] = c
  RZ[1,2] = -s
  RZ[2,1] = s
  RZ[2,2] = c
  RZ[3,3] = 1
  
  
  # Define the rotation around the Y axis based on pitch
  # c = np.cos(pitch)
  # s = np.sin(pitch)
  # RY[0,0] = c
  # RY[0,2] = s
  # RY[2,0] = -s
  # RY[2,2] = c
  # RY[1,1] = 1
  c = cos(pitch)
  s = sin(pitch)
  RY[1,1] = c
  RY[1,3] = s
  RY[3,1] = -s
  RY[3,3] = c
  RY[2,2] = 1
  
  # Define the rotation around the X axis based on roll
  # c = np.cos(roll)
  # s = np.sin(roll)
  # RX[1,1] = c 
  # RX[1,2] = -s
  # RX[2,1] = s
  # RX[2,2] = c
  # RX[0,0] = 1
  c = cos(roll)
  s = sin(roll)
  RX[2,2] = c 
  RX[2,3] = -s
  RX[3,2] = s
  RX[3,3] = c
  RX[1,1] = 1
  
  # Calculate the rotation matrix as the product of RZ * RY * RX matrices
  # # combine to final rotation matrix
  # return RZ*RY*RX
  rotMatrix = RZ %*% RY %*% RX
  
  # rotMatrix = rotMatrix %*% matrix(c(0,1,0,
  #                                    1,0,0,
  #                                    0,0,-1), 
  #                                  ncol = 3, nrow = 3, byrow = T)
  
  # rotMatrix = rotMatrix %*% matrix(c(-1,0,0,
  #                                    0,1,0,
  #                                    0,0,-1),
  #                                  ncol = 3, nrow = 3, byrow = T)
  # 
  # # System PATB
  # phi = asin(rotMatrix[1,3])
  # omega = atan2(-rotMatrix[2,3], rotMatrix[3,3])
  # kappa = atan2(-rotMatrix[1,2], rotMatrix[1,1])

  
  # System BLUH
  phi = atan2(rotMatrix[3,1], rotMatrix[3,3])
  omega = asin(-rotMatrix[3,2])
  kappa = atan2(-rotMatrix[1,2], rotMatrix[2,2])
  
  # # Calculation of Euler angles from rotMatrix
  # # based on the function eulFromRot
  # # from Github: https://github.com/nasa/georef_geocamutilweb/geocamUtil/registration.py
  # 
  # # Define starting values for the Euler angles: omega, phi, kappa
  # # phi = 0
  # # omega = np.arcsin(-rotMatrix.item(2,0))
  # # kappa = None;
  # phi = 0
  # omega = asin(-rotMatrix[3,1])
  # kappa = NULL
  # 
  # # if np.absolute(omega - (math.pi/2.0)) < 0.0000001:
  # if(abs(omega - (pi/2.0)) < 0.0000001){
  # 
  #   # kappa = np.arctan2(rotMatrix.item(1,2), rotMatrix.item(0,2))
  #   kappa = atan2(rotMatrix[2,3], rotMatrix[1,3])
  # 
  # }
  # 
  # # if np.absolute(omega + (math.pi/2.0)) < 0.0000001:
  # if(abs(omega + (pi/2.0)) < 0.0000001){
  # 
  #   # kappa = np.arctan2(-rotMatrix.item(1,2), -rotMatrix.item(0,2))
  #   kappa = atan2(-rotMatrix[2,3], -rotMatrix[1,3])
  # 
  # }
  # 
  # # else:
  # else{
  #   # phi = np.arctan2(rotMatrix.item(2,1), rotMatrix.item(2,2))
  #   phi = atan2(rotMatrix[3,2], rotMatrix[3,3])
  # 
  #   # kappa = np.arctan2(rotMatrix.item(1,0), rotMatrix.item(0,0))
  #   kappa = atan2(rotMatrix[2,1], rotMatrix[1,1])
  # }
  
  phi = phi * 180/pi
  omega = omega * 180/pi
  kappa = kappa * 180/pi
  
  phi = ifelse(phi<0, phi + 360, phi)
  omega = ifelse(omega<0, omega + 360, omega)
  kappa = ifelse(kappa<0, kappa + 360, kappa)
  
  # return [phi, omega, kappa]
  return(data.frame(PHI = phi, OMEGA = omega, KAPPA = kappa))
  
}
