#Load relevant libraries----
library(tidyverse)

#GREENLAND----------
## read in txt files automatically----
fname_GREENLAND <- list.files("OPUS", full.names = T) ###1:100

###Components of Function 1-----
##List with all text files (Two methods depending on separator) 
#filelist <- lapply(fname, read.delim, header = F) 
#add , for alaska samples
filelist_GREENLAND <- lapply(fname_GREENLAND, read.table, sep="") ### 100 [1:1882]

## Adding sample IDs (reference to lake core)
names(filelist_GREENLAND) <- gsub(".*/(.*)\\..*", "\\1", fname_GREENLAND)

###Components of Function 2-------
# save the transformed df to a new list of df called reformattedData [1:3697]
reformattedData_GREENLAND <- lapply(filelist_GREENLAND, function(x){pivot_wider(x, names_from = V1, values_from = V2)})

# Unlist the reformattedData list into matrix (each of the 28 elements has one row of 3697 wavenumber values)
allNames_GREENLAND <- lapply(reformattedData_GREENLAND, names)

# convert matrix into dataframe [28:3697]
allNames2_GREENLAND <- as.data.frame(do.call("rbind",allNames_GREENLAND))
(allNames2$V1) #7496.97825 // 7496.94889 GREENLAND
(allNames2$V3697) #368.38766 GREENLAND

#ALASKA-------
## read in txt files automatically----
fname_ALASKA <- list.files("Alaska100", full.names = T) ###1:100

###Components of Function 1-----
##List with all text files (Two methods depending on separator) 
#filelist <- lapply(fname, read.delim, header = F) 
#add , for alaska samples
filelist_ALASKA <- lapply(fname_ALASKA, read.table, sep=",") ### 100 [1:1882]

## Adding sample IDs (reference to lake core)
names(filelist_ALASKA) <- gsub(".*/(.*)\\..*", "\\1", fname_ALASKA)

###Components of Function 2-------
# save the transformed df to a new list of df called reformattedData [1:3697]
reformattedData_ALASKA <- lapply(filelist_ALASKA, function(x){pivot_wider(x, names_from = V1, values_from = V2)})

# Unlist the reformattedData list into matrix (each of the 28 elements has one row of 3697 wavenumber values)
allNames_ALASKA <- lapply(reformattedData_ALASKA, names)

# convert matrix into dataframe [28:3697]
allNames2_ALASKA <- as.data.frame(do.call("rbind",allNames_ALASKA))
(allNames2_ALASKA$V1) #3996.31543 ALASKA
(allNames2_ALASKA$V1882) #368.38622 ALASKA

#Identify unique wavenumbers for each column 
#Match wavenumbers based  on two data sets 
