# Script to adjust Filenames 

# Steps:
# 1. Create a folder to save all images for a day, including .jpg and .raw files
# 2. Determine the diffeence in time between the GPS time in the picture (this
# should be the first piecture of the day) and the time of file creation (in)

rm(list = ls())
library(tidyverse) # our swiss army knife
library(fs) # reading in files in a tidyish way
library(lubridate) # working with dates 

# specialized packages
library(exifr) # for reading photo metadata

# provide the directory in which photos are stored
in.dir <- "C:/Users/tomo.eguchi/Documents/TomosFolders/SWFSC/CHLHP/Piedras Blancas/TempFolder/"

# Difference in time. Provide hours and minutes, turn them into hours.
delta.time <- 7 + 10/60   

# The following code was originally found here: 
# https://www.thomasvanhoey.com/post/changing-photo-metadata-with-r/

# Some modifications were made.

# regexp: (?i) means case insensitive. $ means the end of line or string
files <- dir_ls(in.dir,
                recurse = TRUE,
                regexp = "(?i).JPG$")

# defining a function
change_file_create_date <- function(afb){
  # get correct exif date
  read_exif(path = afb,
            tags = c("DateTimeOriginal")) %>%
    pull(DateTimeOriginal) %>%
    parse_date_time("Ymd HMS", 
                    tz = "UTC" #timezone
    ) -> correct_time
  
  # changing the creation date
  Sys.setFileTime(afb, correct_time)
  
}


# run the function over all the different files
purrr::walk(files, change_file_create_date)