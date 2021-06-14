
##ADDING CALIBRATION DATA TO OUR DF

library(readr)
library(dplyr)

#read in calibration csv 
wet_chem_data <- read_csv("Coding-Ready-Wet-Chem-Data.csv")
##View(wet_chem_data)

transformedData <- read_csv("transformedData.csv")
##View(transformedData)

-------------#test-branch-feedback
names(transformedData)[ncol(transformedData)] ## dataset name is already here, but lurking

-------------#test-branch
#Rename wet_chem_data columns 
names(wet_chem_data)[1] <- "Sample"
names(wet_chem_data)[2] <- "BSiPercent"

#Read in Sample IDs

fname <- list.files("OPUS", full.names = T) ## read in txt files automatically 

------------#test-branch-feedback


 
-------------#test-branch
filelist <- lapply(fname, read.delim, header = F) ## creates list of txt files

str(filelist, give.attr = FALSE) ##Check structure of filelist 

lapply(filelist, names) ##Checking Names (We need to rename...)

names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname) ##Adding names of data frames to refer to each lake core 

Sample <- names(filelist) #saving names as vector

#Add new column to transformed df so we can join

-------------#test-branch-feedback
transformedData <- cbind(Sample, transformedData) ## this works too, but it looks like we already put this step in the other function, so something to keep in mind when you function-ify this

#bind calibration data to transformed data
Complete_data <- full_join(wet_chem_data, transformedData, by = "Sample")
## nice, I like how this keeps rows for missing data to remind us

dim(Complete_data) ## I would expect this to have the same number of rows as wet_chem_data
dim(wet_chem_data)
dim(transformedData)

setdiff(wet_chem_data$Sample, transformedData$dataset) ## read "in wet_chem_data but not in transformedData"

setdiff(transformedData$dataset, wet_chem_data$Sample) ## read "in transformedData but not in wet_chem_data"

## I wasn't expecting there to be anything in this (saying we don't have response data for these)

## if we look at the names, it looks like some decimal trimming might solve the issue if these are in fact the same

transformedData$Sample = gsub("\\.0","",transformedData$dataset) ## this replaces .0 with a space, the backslashes escape the special character . in regular expressions

Complete_data <- full_join(wet_chem_data, transformedData, by = "Sample")
dim(Complete_data)

setdiff(wet_chem_data$Sample, transformedData$Sample) ## read "in wet_chem_data but not in transformedData"

setdiff(transformedData$Sample, wet_chem_data$Sample) ## read "in transformedData but not in wet_chem_data"

## now it looks like the multiple replicates are what are giving us problems
## not sure what makes sense in context, let's talk to Greg about it

## only 10 of these are not NA though which is a bummer
intersect(transformedData$Sample, wet_chem_data$Sample) ## wow ok, string matching is not our friend

#install.packages("fuzzyjoin")
library(fuzzyjoin)
joined <- transformedData %>%
  stringdist_inner_join(wet_chem_data, by = c(Sample = "Sample"), max_dist = 1)
# saying if the string is off by one, match them

names(joined)[(ncol(joined)-3):ncol(joined)]

View(joined[,c("dataset","Sample.y")])
## these are extra based on visual inspection, double check me
remove <- c(2, 3, 9, 11, 12, 17, 42)

joined2 = joined[-remove,]

dim(joined2) ## this is still not quite what I expect but much better

View(joined2[(ncol(joined)-5):ncol(joined)])

write.csv(joined2,"dataForProofOfConcept.csv",row.names=F) ## we can use this to try out PLS while we work out the string matching issues


## it might also be useful to provide an option to only end up with a dataset that matches what we actually have, it'll be a version of right_join

data_we_have <- right_join(wet_chem_data, transformedData, by = "Sample")

dim(data_we_have)
---------#test-branch
transformedData <- cbind(Sample, transformedData)

#bind calibration data to transformed data
Complete_data <- full_join(wet_chem_data, transformedData, by = "Sample")

