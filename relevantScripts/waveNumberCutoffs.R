library(dplyr)

waveNum <- read.csv("csvFiles/waveNumberInfoU.csv")

dim(waveNum) # 28 3698
names(waveNum)[ncol(waveNum)]

waveNum <- waveNum[, c(3698, 1:3697)] ## reorganize so dataset label is first

## Summary statistics 
summary(unname(unlist(waveNum[1, 2:ncol(waveNum)])))

## Searchwave function: ------------------------------------------------------------------------------------
## Gives column ID for corresponding wavenumber value

searchWave <- function(cutoffValue, rowID, dataset) {
  # browser() ## for testing/debugging
  
  ref <- unname(unlist(dataset[rowID, 2:ncol(dataset)]))
  
  thoseGreater <- which(ref > cutoffValue)
  
  firstOneGreater <- thoseGreater[length(thoseGreater)] ## furthest right greater will be smallest one greater
  
  # ref[firstOneGreater] ## check
  # dataset[rowID, firstOneGreater] ## check
  
  return(firstOneGreater + 1) ## because first column is dataset id
}
# Isolate wavenumber ---------------------------------------------------------------------------------------
## Wavenumber smaller than 3750
searchWave(368, 1, waveNum) ## 3698
searchWave(3750, 1, waveNum) + 1 ## 1945

# Rosen et al 2011 cutoffs-----------------------------------------------------------------------------------

  # 435-480, 790-830, and 1050-1280
  # 3663:3641, 3479:3459, 3344:3226

## Wavenumber 435-480
searchWave(435, 1, waveNum) ## 3663
searchWave(480, 1, waveNum) + 1 ## 3641

## Wavenumber 790-830 
searchWave(790, 1, waveNum) ## 3479
searchWave(830, 1, waveNum) + 1 ## 3459

##Wavenumber 1050-1280
searchWave(1050, 1, waveNum) ## 3344
searchWave(1280, 1, waveNum) + 1 ## 3226
# Truncated Dataset with three intervals 

