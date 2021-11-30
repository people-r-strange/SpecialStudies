#Load relevant libraries----
library(tidyverse)
library(janitor)
## read in txt files automatically----
fname <- list.files("OPUS", full.names = T) ###1:100

###Components of Function 1-----
##List with all text files (Two methods depending on separator) 
#filelist <- lapply(fname, read.delim, header = F) 
#add , for alaska samples
filelist <- lapply(fname, read.table, sep="") ### 100 [1:3697]

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
(allNames2$V3697) #368.38766 // 368.38622 GREENLAND

# add row names permanently
allNames2$dataset <- names(filelist) ## make this a specific column, don't trust it to store



# creating new list of df where there aren't any wavenumbers...only absorbance values [1:3697]
reformattedData2 <- lapply(reformattedData, allNames2)