#Load relevant libraries----
library(tidyverse)

## read in txt files automatically----
fname <- list.files("OPUS", full.names = T) ###1:100

###Components of Function 1-----
##List with all text files (Two methods depending on separator) 
#filelist <- lapply(fname, read.delim, header = F) 
#add , for alaska samples
filelist <- lapply(fname, read.table, sep="") ### 100 [1:1882]
 
## Adding sample IDs (reference to lake core)
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

###Components of Function 2-------
# save the transformed df to a new list of df called reformattedData [1:3697]
reformattedData <- lapply(filelist, function(x){pivot_wider(x, names_from = V1, values_from = V2)})

# Unlist the reformattedData list into matrix (each of the 28 elements has one row of 3697 wavenumber values)
allNames <- lapply(reformattedData, names)

# convert matrix into dataframe [28:3697]
allNames2 <- as.data.frame(do.call("rbind",allNames))
(allNames2$V1) #3996.31543 ALASKA
(allNames2$V1882) #368.38622 ALASKA

(allNames2$V1) #7496.97825 // 7496.94889 GREENLAND
(allNames2$V3697) #368.38766 GREENLAND


# add row names permanently
allNames2$dataset <- names(filelist) ## make this a specific column, don't trust it to store

#Rename column header from "wavenumbers" to "Vi" (FUNCTION #3)
dropNames <- function(data){
  names(data)=paste("V", 1:ncol(data), sep="")
  return(data)
}

# creating new list of df where there aren't any wavenumbers...only absorbance values [1:3697]
reformattedData2 <- lapply(reformattedData, dropNames)

# Dataframe of [28:3697]where absorbance values are in cells
##need to resolve mismatch in wavenumbers before moving forward
newData <- do.call(rbind.data.frame,reformattedData2)

lapply(reformattedData, ncol) %>% unlist() %>% summary()

#checking summary
#lapply(reformattedData, ncol) %>% unlist() %>% summary()
#which are not 1882
#which(unlist(lapply(reformattedData, ncol)) != 1882)
###AW-34.5 (8_31_16).0  AW-7.5 (8_31_16).0   AW-73 (8_31_16).0 

## adds column for each row to remind us which file it is
newData$dataset <- names(filelist)

## Make data sample name in first column
newData <- newData[,c(ncol(newData),1:(ncol(newData)-1))]

allNames2 <- allNames2[,c(ncol(allNames2),1:(ncol(allNames2)-1))]

write.csv(newData, "newData.csv")

###Components of Function 4-----
#Read in calibration csv with same number of samples as our transformedData 
wet_chem_data <- read_csv("csvFiles/resolved-sample-name.csv") ###28

#Read in absorbance values for each sample
transformedData <- read_csv("csvFiles/transformedData.csv") ###28:3698

#Rename wet_chem_data columns 
names(wet_chem_data)[1] <- "dataset"
names(wet_chem_data)[2] <- "BSiPercent"
