# run_readGPX_v3.R
# Runs readGPX_with laser v3 batch.R on all subdirectories in one root directory

rm(list=ls())
source("readGPX_with_laser_fcn.R")
#in.dir <- "data/leatherback_HMB_2021"
in.dir <- "data/Gray whale PB 2022"

output <- readGPX_v3(in.dir, 
                     write.file = T, 
                     save.fig = T, 
                     over.write.data = T,   # Whether or not to overwrite data outputs
                     over.write.fig = F)    # Whether or not to overwrite figures
