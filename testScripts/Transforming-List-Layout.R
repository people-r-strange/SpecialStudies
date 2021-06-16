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

##test-branch-feedback

# my_data <- do.call(rbind.data.frame, filelist) 
## this combines the datasets too soon, we need to reshape them first


## SAS attempt

dim(filelist[[1]]) ## each dataframe is 2 columns, we want it to be one giant row

filelist[[1]] %>% pivot_wider(names_from = V1, values_from = V2) %>% dim()
filelist[[1]] %>% pivot_wider(names_from = V1, values_from = V2) %>% View()

reformattedData <- lapply(filelist, function(x){pivot_wider(x, names_from = V1, values_from = V2)})

lapply(reformattedData, ncol) %>% unlist() %>% summary() ## double check this is what we expect, all should be the same

allNames <- lapply(reformattedData, names) %>% unlist()

length(unique(allNames)) ## should be equal to ncol(reformattedData[[1]]) = 3697

table(allNames)


# round_zero <- function(data){
#   val = as.numeric(names(data))
#   val <- round(val, 0)
#   names(data) = as.character(val)
#   return(data)
# }
# 
# drop <- function(data){
#   val = as.numeric(names(data))
#   val <- floor(val)
#   names(data) = as.character(val)
#   return(data)
#   
# }

allNames = do.call("rbind",lapply(reformattedData, names))
## save this, and pull out chunks of spectrum with this

dropNames <- function(data){
  names(data)=paste("V", 1:ncol(data), sep="")
  return(data)
}

reformattedData2 <- lapply(reformattedData, dropNames)

reformattedData2[[1]] %>% names()

allNames <- lapply(reformattedData2, names) %>% unlist()


## need to resolve mismatch in wavenumbers before moving forward
newData <- do.call(rbind.data.frame,reformattedData2)

## add dataset id
newData$dataset = names(filelist)

dim(newData) ## should be 28 x 3698
--------------------------------------------------------------------------------------------------
library(tidyr)
  
#Trying the Transpose Function 
df_new <- as.data.frame(t(filelist))
  
  
  
  
  
  
  my_data <- do.call(rbind.data.frame, filelist)

##figuring out names 
colnames(my_data)

##test-branch
