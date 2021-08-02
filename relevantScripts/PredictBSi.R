library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)

#Load in data 
#data <- read.csv("csvFiles/dataForProofOfConceptNice.csv")
data <- read_csv("csvFiles/resolvedSampleNames-2.csv")
dim(data) ## 28 3699
## 28 datasets, 1 sample ID, 1 actual BSi, 3697 absorbances

## All Spectrum
### Select wavenumbers smaller than 3750 cm-1
pls2_AS <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
summary(pls2_AS)
print(summary(pls2_AS)) #28 3724 

##Load data without wet chem
data_Predict <- read_csv("csvFiles/OhneWetChem.csv")
dim(data) ## 28 3698

##Predict BSi Using pls2 model
predicted_bsi <- as.data.frame(predict(pls2_AS, ncomp = 1:10, newdata = data_Predict))
dim(pls2) #28 10
