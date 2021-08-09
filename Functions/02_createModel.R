#Load relevant libraries
library(pls)
library(tidyverse)
library(scales)
library(readr)
------------------------------------------------
  #Read in data containing wet chem BSi percentages 
  data <- read_csv("csvFiles/resolvedSampleNames-2.csv")
  dim(data) ## 28 3699
  
------------------------------------------------
  #Code for PLS model
    ##Select CV based on number of samples
    
    ### Cross-Validation is 5-fold ###
    pls2 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
    ### Summary 
    print(summary(pls2))
------------------------------------------------
  #Root Mean Squared Error Plots
      #Plot RMSEP
      plot(RMSEP(pls2))
    
      ## save root mean squared error of prediction to object res
      res2 <- RMSEP(pls2)
      
      #Dataframe with cross validation and number of components
      Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)
      
      #Number of components graph 
      ggplot(Five_fold_CV, aes(ncomps, cv)) +
        geom_line() +
        labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
             subtitle="CV is 5-fold",
             y = "RMSEP", 
             x = "Number of Components") +
        scale_x_continuous(breaks = c(1:10)) + 
        theme_minimal()
----------------------------------------------------
  #Create "test data" with different loadings 
        ## Load wavenumber data 
        waveNumberInfo <- read_csv("csvFiles/waveNumberInfo.csv") ## 28 3698
        
        wavenumber <- as.numeric(as.vector(unname(t(waveNumberInfo[1,]))))
      
      
      #binding the two
      #cross_validated_errors <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)
      
      #Data frame for different loadings 
      #loadings, #first column of random opus txt file)
      testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                                  loadings_2= pls2$loadings[,2], 
                                  loadings_3 = pls2$loadings[,3], 
                                  weighted_loading_1 = pls2$loading.weights[,1], 
                                  weighted_loading_2 = pls2$loading.weights[,2], 
                                  weighted_loading_3 = pls2$loading.weights[,3], 
                                  wavenumber = wavenumber)
      
------------------------------------------------------
  #Loading Plots
         #Loading plot for Full Spectrum loading 1 with legend and vertical lines
         
         ggplot(testdata, aes(x=wavenumber)) + 
         geom_line (aes(y = weighted_loading_1, colour = "1st Component")) +
         geom_line (aes(y = weighted_loading_2, color = "2nd Component")) + 
         geom_line (aes(y = weighted_loading_3, colour = "3rd Component")) + 
         
         scale_colour_manual("", 
                             breaks = c("1st Component", "2nd Component", "3rd Component"),
                             values = c("blue", "dark green", "orange")) +
         
         labs(y="Weighted Loadings", 
              x=expression(Wavenumber(cm^-1)), 
              title='Loading Plot for Three Components: Full Spectrum, n = 28',
              subtitle= expression("Where wavenumber ranges from ~7500 to ~370" ~ cm^{-1}))+
         scale_x_reverse() + 
         theme_minimal()
      