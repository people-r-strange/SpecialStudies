#Load relevant libraries----
library(tidyverse)

## read in txt files automatically----
fname <- list.files("OPUS", full.names = T) ###1:28

##FUNCTION 1: Add Sample Names----
  
  addSampleNames <- function(fname) {
  ## creates list of txt files
  filelist <- lapply(fname, read.delim, header = F)
  
  ## Adding sample IDs (reference to lake core)
  names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)
  
  return(filelist)
}
  #Save list of txt files with sample names
  filelist <- addSampleNames(fname) ###28
  
##FUNCTION 2: Rename column header from "wavenumbers" to "Vi"----
  
  dropNames <- function(data) {
    names(data) <- paste("V", 1:ncol(data), sep = "")
    return(data)
  }
##FUNCTION 3: Transforms the large list of dataframes [3697:2] into correct format [1:3697]----
    
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
  
  
 output <- transform_df(filelist) ###1:28
  
  #write csv files 
  write.csv(output$newData, "transformedData.csv", row.names = F)
  write.csv(output$waveNumberInfo, "waveNumberInfo.csv", row.names = F)
  
##FUNCTION 4: Add Calibration Data ----
  
    #Read in calibration csv with same number of samples as our transformedData 
    wet_chem_data <- read_csv("csvFiles/resolved-sample-name.csv") ###28
  
    #Read in absorbance values for each sample
    transformedData <- read_csv("csvFiles/transformedData.csv") ###28:3698
    
    addWetChem <- function(transformedData) {
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
      
      return(Complete_data)
    }
    
    Data <- addWetChem(transformedData) ###28:3699
    
    #Write csv file 
    write.csv(Data,"csvFiles/resolvedSampleNames-2.csv",row.names=F)
    
    
    
    
    
    
    
    
    
    
    
## Big Function (Not completed) ----
  runAll <- function(fname) {
    sampleNames <- addSampleNames(fname)
    listAbsorbanceWavenumber <- transform_df(sampleNames)
    return(listAbsorbanceWavenumber)
  }
  
  output <- runAll(fname)
  