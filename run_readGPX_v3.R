# run_readGPX_v3.R
# Runs readGPX_with laser v3 batch.R on all subdirectories in one root directory

# To use this script, place this script and readGPS_with_laser_fcn.R into a same
# directory. Enter a relative or full path to the directory that contains log
# directories on line 8 (between double quotation marks).

rm(list=ls())
source("readGPX_with_laser_fcn.R")
#in.dir <- "data/Gray whale PB 2022"
#in.dir <- "data/Leatherback_2021"
#in.dir <- "data/Leatherback_HMB_2021"
#in.dir <- "data/Leatherback_HMB_2022"
#in.dir <- "data/Agua Hedionda/"
#in.dir <- "data/Piedras Blancas/Flight data/"
#in.dir <- "data/Jamul training/"
#in.dir <- "data/San Diego Bay training/"
#in.dir <- "data/San Diego Coastal Cetacean/APH-28 Goliath/"
#in.dir <- "data/San Diego Coastal Cetacean/APH-22 Ruffian/"
#in.dir <- "data/San Diego Bay Green Turtles/LOG - Mavrick/"
in.dir <- "data/San Diego Bay Green Turtles/LOG - Poppins/"

output <- readGPX_v3(in.dir, 
                     write.file = T, 
                     save.fig = T, 
                     over.write.data = F,   # Whether or not to overwrite data outputs
                     over.write.fig = F)    # Whether or not to overwrite figures
