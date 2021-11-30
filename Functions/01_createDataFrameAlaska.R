#Load relevant libraries----
library(tidyverse)

## read in txt files automatically----
fname <- list.files("Alaska100", full.names = T) ###1:100

##FUNCTION 1: Add Sample Names----

addSampleNames <- function(fname) {
  ## creates list of txt files
  filelist <- lapply(fname, read.table, sep=",")
  
  ## Adding sample IDs (reference to lake core)
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)
  
  return(filelist)
}

#Save list of txt files with sample names
filelist <- addSampleNames(fname) ### 100 $V1:[1:1882] $V2:[1:1882]

lapply(filelist, ncol) %>% unlist() %>% summary()

## can pull this out so we can use it independently of transform_df function
# manually renaming wavenumber in column headers with "Vi"
dropNames <- function(data) {
  names(data) <- paste("V", 1:ncol(data), sep = "")
  return(data)
}

# Create function that transforms the df into correct format

# assign name to our function, input list of df we want to function to work on
transform_df <- function(filelist) {
  
  # save the transformed df to a new list of df called reformattedData
  reformattedData <- lapply(filelist, function(x) {
    pivot_wider(x, names_from = V1, values_from = V2)
  })
  
  # Unlist the reformattedData list into matrix
  allNames <- lapply(reformattedData, names) 
  
  # convert matrix into dataframe
  allNames2 <- as.data.frame(do.call("rbind",allNames))
  
  
  # add row names permanently
  allNames2$dataset <- row.names(allNames2) ## make this a specific column, don't trust it to store
  
  # creating new list of df where there aren't any wavenumbers...only absorbance values
  reformattedData2 <- lapply(reformattedData, dropNames)
  
  ## adds column for each row to remind us which file it is
  reformattedData2$dataset <- names(filelist)
  
  ## returning the waveNumberInfo too
  return(list(newData = reformattedData2, waveNumberInfo = allNames2))
}

## how you have it written expects filelist to be a list of datasets
output <- transform_df(filelist)