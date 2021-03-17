library(tidyverse)

#reading in list of txt files and converting it to a df 

## read in txt files automatically 
fname <- list.files("OPUS", full.names = T)

## creates list of txt files 
filelist <- lapply(fname, read.delim, header = F)

##Adding sample IDs (reference to lake core)
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

--------------------------------------------------------------------------------------------------------------------------
#Create function that transforms the df into correct format 

#assign name to our function, input df we want to function to work on
transform_df <- function(filelist) {
  
  #save the transformed df to a new list of df called reformattedData
  reformattedData <- lapply(filelist, function(x){pivot_wider(x, names_from = V1, values_from = V2)})
  
  #Unlist the reformattedData list into ?   
  allNames <- lapply(reformattedData, names) %>% unlist()
  
  #create matrix with sample names in rows and wavenumber in columns, data in cells is absorbance values
  allNames = do.call("rbind",lapply(reformattedData, names))
  
  #manually renaming wavenumber in column headers with "Vi"
  dropNames <- function(data){
    names(data)=paste("V", 1:ncol(data), sep="")
    return(data)
  }
  
  #creating new list of df where there aren't any wavenumbers...only absorbance values 
  reformattedData2 <- lapply(reformattedData, dropNames)
  
  #write over previous ? "allNames" so that the new ? "allNames" does not have wavenumber; only contains absorbance 
  allNames <- lapply(reformattedData2, names) %>% unlist()
  
  ## need to resolve mismatch in wavenumbers before moving forward
  newData <- do.call(rbind.data.frame,reformattedData2)
  
  ## removes sample IDs
  newData$dataset = names(filelist)
  
  return(newData)
}

transform_df(1)
