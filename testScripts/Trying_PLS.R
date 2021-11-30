library(readr)
library(pls)
library(tidyverse)

#Read in complete dataset
dataForProofOfConcept <- read_csv("dataForProofOfConcept.csv")
View(dataForProofOfConcept)

#Read in NICE dataset
dataForProofOfConceptNice <- read_csv("dataForProofOfConceptNice.csv")
View(dataForProofOfConceptNice)



#Checking column names
#names(dataForProofOfConcept[(ncol(dataForProofOfConcept)-5):ncol(dataForProofOfConcept)])

#Checking position of undesired columns
#which(names(dataForProofOfConcept)== "dataset")
#which(names(dataForProofOfConcept)== "dataset_adjusted")
# which(names(dataForProofOfConcept)== "Sample.y")
# which(names(dataForProofOfConcept)== "Sample.x")

#remove undesired columns
# dataForProofOfConcept <- dataForProofOfConcept %>%
#   select(-1,-(3699:3701))

#Check that undesired columns are removed
# names(dataForProofOfConcept[(ncol(dataForProofOfConcept)-5):ncol(dataForProofOfConcept)])

#PLS validation = "LOO" 
pls1 <- plsr(BSiPercent~., ncomp = 10, data=dataForProofOfConceptNice, validation = "LOO")

summary(pls1)

plot(RMSEP(pls1), legendpos = 'topright')

summary(wet_chem_data)

#List of all attributes 
PLS_list <-  RMSEP(pls1)

#What we're working with 
names(pls1)

#SlotNames 
#slotNames(pls1)

#What we're working with 
#names(PLS_list)

#Select only relevant attributes 

pls_useful <- pls1 %>%
  select("coefficients", "scores", "loadings", )

