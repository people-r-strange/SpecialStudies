library(tidyverse)

## read in txt files automatically 
fname <- list.files("OPUS", full.names = T)

## creates list of txt files 
filelist <- lapply(fname, read.delim, header = F)

##Check structure of filelist 
str(filelist, give.attr = FALSE)

##Checking Names (We need to rename...)
lapply(filelist, names)

##Adding names of data frames to refer to each lake core 
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

## Viewing what the list file looks like 
View(filelist)

glimpse(filelist)


##Transform dataframe
# my_data <- do.call(rbind.data.frame, filelist) 
## this combines the datasets too soon, we need to reshape them first

## SAS attempt

dim(filelist[[1]]) ## each dataframe is 2 columns, we want it to be one giant row

filelist[[1]] %>% pivot_wider(names_from = V1, values_from = V2) %>% dim()
filelist[[1]] %>% pivot_wider(names_from = V1, values_from = V2) %>% View()

reformattedData <- lapply(filelist, function(x){pivot_wider(x, names_from = V1, values_from = V2)})

lapply(reformattedData, ncol) %>% unlist() %>% summary() ## double check this is what we expect, all sould be the same

allNames <- lapply(reformattedData, names) %>% unlist()

length(unique(allNames)) ## should be equal to ncol(reformattedData[[1]]) = 3697

## pain point! stopping here for now

## need to resolve mismatch in wavenumbers before moving forward
newData <- do.call(rbind.data.frame,reformattedData)

## add dataset id
newData$dataset = names(filelist)

dim(newData) ## should be 28 x 3698
