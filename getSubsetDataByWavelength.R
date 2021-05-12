library(dplyr)

waveNum <- read.csv("waveNumberInfoU.csv")

dim(waveNum)
names(waveNum)[ncol(waveNum)]

waveNum <- waveNum[, c(3698, 1:3697)] ## reorganize so dataset label is first

head(names(waveNum))

head(waveNum[, 1:5]) ## goes from big to small as you go left to right

# Rosen et al 2011 cutoffs
# 435-480, 790-830, and 1050-1280

summary(unname(unlist(waveNum[1, 2:ncol(waveNum)])))

searchWave <- function(cutoffValue, rowID, dataset) {
  # browser() ## for testing/debugging

  ref <- unname(unlist(dataset[rowID, 2:ncol(dataset)]))

  thoseGreater <- which(ref > cutoffValue)

  firstOneGreater <- thoseGreater[length(thoseGreater)] ## furthest right greater will be smallest one greater

  # ref[firstOneGreater] ## check
  # dataset[rowID, firstOneGreater] ## check

  return(firstOneGreater + 1) ## because first column is dataset id
}

searchWave(435, 1, waveNum) ## 3663
searchWave(480, 1, waveNum) + 1 ## 3641
# (since returns the first value that is greater than upper bound going big to small need to go one more smaller (to the right) to not include too much)

searchWave(790, 1, waveNum) ## 3479
searchWave(830, 1, waveNum) + 1 ## 3459

searchWave(1050, 1, waveNum) ## 3344
searchWave(1280, 1, waveNum) + 1 ## 3226

## check same for every dataset
lapply(1:nrow(waveNum), function(x) {
  searchWave(435, x, waveNum)
}) %>%
  unlist() %>%
  sd()
## should be zero if all the same

lapply(1:nrow(waveNum), function(x) {
  searchWave(480, x, waveNum)
}) %>%
  unlist() %>%
  sd()
lapply(1:nrow(waveNum), function(x) {
  searchWave(790, x, waveNum)
}) %>%
  unlist() %>%
  sd()
lapply(1:nrow(waveNum), function(x) {
  searchWave(830, x, waveNum)
}) %>%
  unlist() %>%
  sd()
lapply(1:nrow(waveNum), function(x) {
  searchWave(1050, x, waveNum)
}) %>%
  unlist() %>%
  sd()
lapply(1:nrow(waveNum), function(x) {
  searchWave(1280, x, waveNum)
}) %>%
  unlist() %>%
  sd()
## all fine

## it would be nice if we could use all of this in a bigger function that would wrap all the answers together and spit out c(3641:3663, 3459:3479, 3226, 3344) instead of manually doing this all (I'll leave as a TO DO)

## then that could be used to pull the columns out of the dataForProofOfConceptNice
## no need to shift around because first column is BSiPercent instead of dataset name
