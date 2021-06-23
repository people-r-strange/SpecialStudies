##ADDING CALIBRATION DATA TO OUR DF

library(readr)
library(dplyr)

#Read in calibration csv with same number of samples as our transformedData 
wet_chem_data <- read_csv("csvFiles/resolved-sample-name.csv")

#Read in absorbance values for each sample
transformedData <- read_csv("csvFiles/transformedData.csv")

## Check if sample names are present 
names(transformedData)[ncol(transformedData)] 

## rearranging so dataset id is first, should do this in the transformating function instead moving forward
transformedData <- cbind.data.frame(dataset = transformedData$dataset, transformedData[,-ncol(transformedData)]) 

#Rename wet_chem_data columns 
names(wet_chem_data)[1] <- "dataset"
names(wet_chem_data)[2] <- "BSiPercent"

#bind calibration data to transformed data
Complete_data <- full_join(wet_chem_data, transformedData, by = "dataset")

## this replaces .0 with a space, the backslashes escape the special character . in regular expressions
Complete_data$dataset = gsub("\\.0","",Complete_data$dataset) 

## this replaces cm with a space, the backslashes escape the special character . in regular expressions
Complete_data$dataset = gsub("cm","",Complete_data$dataset)

##Remove sample names *can we keep sample names and run model on the data?*
#Complete_data <- select(Complete_data,-dataset)

#Write csv file 
write.csv(Complete_data,"csvFiles/resolvedSampleNames-2.csv",row.names=F)