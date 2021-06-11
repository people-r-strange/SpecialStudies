library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)


#Load in data 
data <- read.csv("dataForProofOfConceptNice.csv")
dim(data) ## 39 3698
## 39 datasets, 3697 absorbances, 1 response

--------------------------------------------------------------------------------------------------------
#Filter out specific portions of absorbance spectrum
  
  ##3663 - 3641
  
  data <- 