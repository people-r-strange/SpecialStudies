
##ADDING CALIBRATION DATA TO OUR DF

#read in calibration csv 
library(readr)
wet_chem_data <- read_csv("Coding-Ready-Wet-Chem-Data.csv")
View(wet_chem_data)

transformedData <- read_csv("transformedData.csv")
View(transformedData)
