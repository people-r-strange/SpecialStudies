#Load relevant libraries

library(tidyverse)
----------------------------------------------------------------------------
  ## read in txt files automatically
fname <- list.files("OPUS", full.names = T)
----------------------------------------------------------------------------
##FUNCTION 0: How would I write a function that Contains the following 3 functions (addSampleNames, dropNames, transform_df)?
  
  
  ##FUNCTION 1: Add Sample Names
  addSampleNames <- function(fname) {
  ## creates list of txt files
  filelist <- lapply(fname, read.delim, header = F)
  
  ## Adding sample IDs (reference to lake core)
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)
  
  return(filelist)
}

  #Save list of txt files with sample names
  filelist <- addSampleNames(fname)
------------------------------------------------------------------------------
  ##FUNCTION 2: Rename column header from "wavenumbers" to "Vi"
  dropNames <- function(data) {
    names(data) <- paste("V", 1:ncol(data), sep = "")
    return(data)
  }
------------------------------------------------------------------------------
    ##FUNCTION 3: Transforms the large list of dataframes [3697:2] into correct format [1:3697]
    
    # assign name to our function, input list of dataframes we want function to work on (filelist)
    transform_df <- function(filelist) {
      
      # save the transformed df to a new list of df called reformattedData [1:3697]
      reformattedData <- lapply(filelist, function(x) {
        pivot_wider(x, names_from = V1, values_from = V2)
      })
      
      # Unlist the reformattedData list into matrix (each of the 28 elements has one row of 3697 wavenumber values)
      allNames <- lapply(reformattedData, names) 
      
      # convert matrix into dataframe [28:3697]
      allNames2 <- as.data.frame(do.call("rbind",allNames))
      
      
      # add row names permanently
      allNames2$dataset <- row.names(allNames2) ## make this a specific column, don't trust it to store
      
      # creating new list of df where there aren't any wavenumbers...only absorbance values [1:3697]
      reformattedData2 <- lapply(reformattedData, dropNames)
      
      # Dataframe of [28:3697]where absorbance values are in cells
      ##need to resolve mismatch in wavenumbers before moving forward
      newData <- do.call(rbind.data.frame, reformattedData2)
      
      ## adds column for each row to remind us which file it is
      newData$dataset <- names(filelist)
      
      ## Make data sample name in first column
      newData <- newData[,c(ncol(newData),1:(ncol(newData)-1))]
      
      allNames2 <- allNames2[,c(ncol(allNames2),1:(ncol(allNames2)-1))]
      
      ## returning the waveNumberInfo too
      return(list(newData = newData, waveNumberInfo = allNames2))
    }
  
  
 #output <- transform_df(filelist)
  -----------------------------------------------------------
  ## Big Function
  runAll <- function(fname) {
    sampleNames <- addSampleNames(fname)
    listAbsorbanceWavenumber <- transform_df(sampleNames)
    return(listAbsorbanceWavenumber)
  }
  
  output <- runAll(fname)
  