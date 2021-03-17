library(tidyverse)

# reading in list of txt files and converting it to a df

## read in txt files automatically
fname <- list.files("OPUS", full.names = T)

## creates list of txt files
filelist <- lapply(fname, read.delim, header = F)

## Adding sample IDs (reference to lake core)
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

--------------------------------------------------------------------------------------------------------------------------

  ## can pull this out so we can use it independently of transform_df function
  # manually renaming wavenumber in column headers with "Vi"
  dropNames <- function(data) {
  names(data) <- paste("V", 1:ncol(data), sep = "")
  return(data)
}


# Create function that transforms the df into correct format

# assign name to our function, input df we want to function to work on
transform_df <- function(filelist) {

  # save the transformed df to a new list of df called reformattedData
  reformattedData <- lapply(filelist, function(x) {
    pivot_wider(x, names_from = V1, values_from = V2)
  })

  # Unlist the reformattedData list into ?
  allNames <- lapply(reformattedData, names) 
  allNames2 <- as.data.frame(do.call("rbind",allNames))



  allNames2$dataset <- row.names(allNames2) ## make this a specific column, don't trust it to store


  # creating new list of df where there aren't any wavenumbers...only absorbance values
  reformattedData2 <- lapply(reformattedData, dropNames)

  # write over previous ? "allNames" so that the new ? "allNames" does not have wavenumber; only contains absorbance
  ## I don't think we need this. We just want to save allNames to use later.
  # allNames <- lapply(reformattedData2, names) %>% unlist()

  ## need to resolve mismatch in wavenumbers before moving forward
  newData <- do.call(rbind.data.frame, reformattedData2)

  ## adds column for each row to remind us which file it is
  newData$dataset <- names(filelist)

  ## returning the waveNumberInfo too
  return(list(newData = newData, waveNumberInfo = allNames2))
}

## how you have it written expects filelist to be a list of datasets
output <- transform_df(filelist)

write.csv(output$newData, "transformedData.csv", row.names = F)
write.csv(output$waveNumberInfo, "waveNumberInfo.csv", row.names = F)
