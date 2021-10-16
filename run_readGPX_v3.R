# run_readGPX_v3.R
# Runs readGPX_with laser v3 batch.R on all subdirectories in one root directory

source("readGPX_with_laser_fcn.R")
in.dir <- "data/leatherback_HMB_2021"

output <- readGPX_v3(in.dir, write.file = F, save.fig = F)
