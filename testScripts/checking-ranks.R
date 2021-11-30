library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)
library(dplyr)
library(Metrics)
library(moderndive)

#Load data
actual_bsi <- read_csv("csvFiles/dataForProofOfConceptNice.csv")
dim(actual_bsi) # 39 3698
names(actual_bsi)

#Run pls // calibration model
pls2 <- plsr(BSiPercent~., ncomp = 10, data=actual_bsi, validation = "CV", segments = 5)
dim(pls2)

pls2$fitted.values

#Convert predicted BSi percents into data frame
predicted_bsi <- as.data.frame(pls2$fitted.values)
dim(predicted_bsi) # 39 10

#select model with 3 components
predicted_bsi_3 <- as.data.frame (predicted_bsi[,c(3)])

#Isolate actual BSi 
actual_bsi <- actual_bsi[,c(1)]

#compare predicted vs actual 
BSi <- bind_cols(actual_bsi, predicted_bsi_3)

#Long Format 
BSi_Long <- BSi %>%
  select(Actual_Bsi, predicted_bsi_3)%>%
  gather(key = "variable", value = "value", -Sample)

#visually map to see whether the trend is the same 

ggplot(BSi, aes(x = actual_bsi)) + geom_line()
