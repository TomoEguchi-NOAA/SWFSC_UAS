# run_readGPX_v3.R
# Runs readGPX_with laser v3 batch.R on all subdirectories in one root directory

source("readGPX_with laser v3 batch.R")
in.dir <- "data/leatherback_2021"

readGPX_v3(in.dir, write.file = T)
