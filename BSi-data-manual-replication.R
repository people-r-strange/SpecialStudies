#Replicating figures from PLS manual but with our data set 

library(readr)
library(pls)
library(tidyverse)

#Read in NICE dataset
dataForProofOfConceptNice <- read_csv("dataForProofOfConceptNice.csv")
View(dataForProofOfConceptNice)

#Fitting PLSR model 
pls1 <- plsr(BSiPercent~., ncomp = 10, data=dataForProofOfConceptNice, validation = "LOO")

summary(pls1)

#Plotting RMSEPs (Root Mean Squared Error of Prediction)
RMSEP_plot <- plot(RMSEP(pls1), legendpos = 'topright')

-----------------------------------------------------------------------------------------------------
##Inspecting different aspects of the fit (prediction plot, other plots) 

#Prediction plot 
predict_plot <- plot(pls1, ncomp = 2, asp = 1, line = TRUE)

##This figure shows the cross-validated predictions with two components versus measured values. 

------------------------------------------------------------------------------------------------------
  
  #Scores plot 
  plot(pls1, plottype = "scores", comps = 1:3)

##This gives pairwise plot of the score values for the three first components. Score plots are used to look for patterns, groups, or outliers. The numbers in parentheses are relative amount of X variance explained by each component 

---------------------------------------------------------------------------------------------------------

  #Explained variance 
  explvar(pls1)

#Comp 1     Comp 2     Comp 3     Comp 4     Comp 5     Comp 6     Comp 7     Comp 8     Comp 9    Comp 10 
#61.6590392 11.3105871 12.0007517 10.0192310  1.9166517  0.7630870  0.5555907  0.3011697  0.4700198  0.2200515 

------------------------------------------------------------------------------------------------------------

  #Loading plot 
  plot(pls1, "loadings", comps = 1:2) #need label and xlab

  abline(h=0)

##This plot is used for interpretation purposes, like looking for known spectral peaks or profiles
  
------------------------------------------------------------------------------------------------------------
    
    


