
##ADDING CALIBRATION DATA TO OUR DF

library(readr)
library(dplyr)

#read in calibration csv 
wet_chem_data <- read_csv("Coding-Ready-Wet-Chem-Data.csv")
##View(wet_chem_data)

transformedData <- read_csv("transformedData.csv")
##View(transformedData)

#Rename wet_chem_data columns 
names(wet_chem_data)[1] <- "Sample"
names(wet_chem_data)[2] <- "BSiPercent"

#Read in Sample IDs

fname <- list.files("OPUS", full.names = T) ## read in txt files automatically 

filelist <- lapply(fname, read.delim, header = F) ## creates list of txt files

str(filelist, give.attr = FALSE) ##Check structure of filelist 

lapply(filelist, names) ##Checking Names (We need to rename...)

names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname) ##Adding names of data frames to refer to each lake core 

Sample <- names(filelist) #saving names as vector

#Add new column to transformed df so we can join
transformedData <- cbind(Sample, transformedData)

#bind calibration data to transformed data
Complete_data <- full_join(wet_chem_data, transformedData, by = "Sample")
