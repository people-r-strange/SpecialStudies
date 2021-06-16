
##ADDING CALIBRATION DATA TO OUR DF

library(readr)
library(dplyr)

#read in calibration csv 
wet_chem_data <- read_csv("Coding-Ready-Wet-Chem-Data.csv")
##View(wet_chem_data)

transformedData <- read_csv("transformedData.csv")

#####test-branch-feedback

## rearranging so dataset id is first, should do this in the transformating function instead moving forward
transformedData <- cbind.data.frame(dataset = transformedData$dataset, transformedData[,-ncol(transformedData)]) 
=======
-------------#test-branch-feedback
names(transformedData)[ncol(transformedData)] ## dataset name is already here, but lurking

# -------------#test-branch
# #Rename wet_chem_data columns 
# names(wet_chem_data)[1] <- "Sample"
# names(wet_chem_data)[2] <- "BSiPercent"
# #####test-branch

head(names(transformedData))
tail(names(transformedData))

#####test-branch-feedback
###test-branch
#Rename wet_chem_data columns 
names(wet_chem_data)[1] <- "dataset"
names(wet_chem_data)[2] <- "BSiPercent"
=======
# fname <- list.files("OPUS", full.names = T) ## read in txt files automatically 
# 
# ------------#test-branch-feedback
# 
# 
#  
# -------------#test-branch
# filelist <- lapply(fname, read.delim, header = F) ## creates list of txt files
# 
# str(filelist, give.attr = FALSE) ##Check structure of filelist 
# 
# lapply(filelist, names) ##Checking Names (We need to rename...)
# 
# names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname) ##Adding names of data frames to refer to each lake core 
# 
# Sample <- names(filelist) #saving names as vector
# 
# #Add new column to transformed df so we can join
# 
# -------------#test-branch-feedback
# transformedData <- cbind(Sample, transformedData) ## this works too, but it looks like we already put this step in the other function, so something to keep in mind when you function-ify this
# #####test-branch

#bind calibration data to transformed data
Complete_data <- full_join(wet_chem_data, transformedData, by = "dataset")

dim(Complete_data) ## I would expect this to have the same number of rows as wet_chem_data
dim(wet_chem_data)
dim(transformedData)

setdiff(wet_chem_data$dataset, transformedData$dataset) ## read "in wet_chem_data but not in transformedData"

setdiff(transformedData$dataset, wet_chem_data$dataset) ## read "in transformedData but not in wet_chem_data"

## I wasn't expecting there to be anything in this (saying we don't have response data for these)

## if we look at the names, it looks like some decimal trimming might solve the issue if these are in fact the same

#wet_chem_data$dataset
#transformedData$dataset



transformedData$dataset = gsub("\\.0","",transformedData$dataset) ## this replaces .0 with a space, the backslashes escape the special character . in regular expressions

transformedData$dataset = gsub("cm","",transformedData$dataset) ## this replaces .0 with a space, the backslashes escape the special character . in regular expressions


Complete_data <- full_join(wet_chem_data, transformedData, by = "dataset")
dim(Complete_data)

setdiff(wet_chem_data$dataset, transformedData$dataset) ## read "in wet_chem_data but not in transformedData"

setdiff(transformedData$dataset, wet_chem_data$dataset) ## read "in transformedData but not in wet_chem_data"

## now it looks like the multiple replicates are what are giving us problems
## not sure what makes sense in context, let's talk to Greg about it

## only 10 of these are not NA though which is a bummer
intersect(transformedData$dataset, wet_chem_data$dataset) ## wow ok, string matching is not our friend

#install.packages("fuzzyjoin")
library(fuzzyjoin)
joined <- transformedData %>%
  stringdist_inner_join(wet_chem_data, by = c(dataset = "dataset"), max_dist = 1)
# saying if the string is off by one, match them

names(joined)[(ncol(joined)-3):ncol(joined)]
names(joined)[1:5]

View(joined[,c("dataset.x","dataset.y")])
## these are extra based on visual inspection, double check me
remove <- c(2, 3, 9, 11, 12, 14, 15, 17, 44)

joined2 = joined[-remove,]
View(joined2[,c("dataset.x","dataset.y")]) ## have Greg check this

write.csv(joined2[,c("dataset.x","dataset.y")], "toCheck.csv", row.names = F)


dim(joined2) ## this is still not quite what I expect but much better

head(names(joined2))
tail(names(joined2))

nice <- cbind.data.frame(BSiPercent = joined2$BSiPercent, joined2[2:(ncol(joined2)-2)])

head(names(nice))
tail(names(nice))

write.csv(nice,"dataForProofOfConceptNice.csv",row.names=F) ## we can use this to try out PLS while we work out the string matching issues

setdiff(wet_chem_data$dataset, joined2$dataset.y)
# missing these spectra
#[1] "NANB3A1-126.5" "NANDB-10A"     "NANDB-10B"     "NANDB-31A"     "NANDB-31B"    
#[6] "NANDB-31C"  

joined2$dataset.y
#[1] "FISK-10"        "FISK-110"       "FISK-270"       "LSA1-30A"       "LSA1-30B"       "LSA2-35"       
#[7] "LSA2-50A"       "LSA2-55"        "LSA3-55"        "LSA3-95A"       "LSA3-95B"       "NANDB-2"       
#[13] "NANDB-4"        "NANA1A-109"     "NANA1A 116"     "NANA2-132A"     "NANA2-132B"     "NANA2-132C"    
#[19] "NANA2-50A"      "NANA2-50B"      "NANA2-55A"      "NANA2-55B"      "NANA2-60A"      "NANA2-60B"     
#[25] "NANA2-65A"      "NANA2-65B"      "NANB3A1-131.5A" "NANB3A1-131.5B" "NANB3A2-10.5"   "NANB3A2-6"     
#[31] "NANB3A2-6B"     "NANDB-13.5A"    "NANDB-13.5B"    "NANDB-17A"      "NANDB-17B"      "NANDB-20A"     
#[37] "NANDB-20B"      "SS"             "WQ"     

wet_chem_data$dataset
#[1] "FISK-10"        "FISK-110"       "FISK-270"       "LSA1-30A"       "LSA1-30B"       "LSA2-35"       
#[7] "LSA2-50A"       "LSA2-55"        "LSA3-55"        "LSA3-95A"       "LSA3-95B"       "NANA1A 116"    
#[13] "NANA1A-109"     "NANA2-132A"     "NANA2-132B"     "NANA2-132C"     "NANA2-50A"      "NANA2-50B"     
#[19] "NANA2-55A"      "NANA2-55B"      "NANA2-60A"      "NANA2-60B"      "NANA2-65A"      "NANA2-65B"     
#[25] "NANB3A1-126.5"  "NANB3A1-131.5A" "NANB3A1-131.5B" "NANB3A2-10.5"   "NANB3A2-6"      "NANB3A2-6B"    
#[31] "NANDB-2"        "NANDB-4"        "NANDB-10A"      "NANDB-10B"      "NANDB-13.5A"    "NANDB-13.5B"   
#[37] "NANDB-17A"      "NANDB-17B"      "NANDB-20A"      "NANDB-20B"      "NANDB-31A"      "NANDB-31B"     
#[43] "NANDB-31C"      "SS"             "WQ"            

setdiff(transformedData$dataset, joined2$dataset.y)
#[1] "LSA1-30"       "LSA2-50"       "LSA3-95"       "NAN-DB-10"     "NAN-DB-2"      "NAN-DB-31"    
#[7] "NAN-DB-4"      "NANA1A-116"    "NANA2-132"     "NANA2-50"      "NANA2-55"      "NANA2-60"     
#[13] "NANA2-65"      "NANB3A1-131.5" "NANDB-13.5"    "NANDB-17"      "NANDB-20"     

transformedData$dataset
#[1] "FISK-10"       "FISK-110"      "FISK-270"      "LSA1-30"       "LSA2-35"       "LSA2-50"      
#[7] "LSA2-55"       "LSA3-55"       "LSA3-95"       "NAN-DB-10"     "NAN-DB-2"      "NAN-DB-31"    
#[13] "NAN-DB-4"      "NANA1A-109"    "NANA1A-116"    "NANA2-132"     "NANA2-50"      "NANA2-55"     
#[19] "NANA2-60"      "NANA2-65"      "NANB3A1-131.5" "NANB3A2-10.5"  "NANB3A2-6"     "NANDB-13.5"   
#[25] "NANDB-17"      "NANDB-20"      "SS"            "WQ" 

##### test-branch-feedback
setdiff( joined2$dataset.y, wet_chem_data$dataset) ## good


=======
dim(data_we_have)
---------#test-branch
transformedData <- cbind(Sample, transformedData)

#bind calibration data to transformed data
Complete_data <- full_join(wet_chem_data, transformedData, by = "Sample")

#####test-branch
