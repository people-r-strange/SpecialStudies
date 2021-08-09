#Load relevant libraries----
library(pls)
library(tidyverse)
library(scales)
library(readr)
#Load Absorbance Data----
  #Read in data containing wet chem BSi percentages 
  data <- read_csv("csvFiles/resolvedSampleNames-2.csv") ### 28 3699
  data <- data %>%
    select(-1)
  dim(data) ## 28 3698
  
#Code for PLS model----
  
    ##Select CV based on number of samples
    
    ### Cross-Validation is 5-fold ###
    pls2 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
    ### Summary 
    print(summary(pls2))
#Root Mean Squared Error Plots----
  
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
#Create "test data" with different loadings ----
  
    ## Load Opus txt file 
       # reading in list of txt files and converting it to a df
        
      ## read in txt files automatically
      fname <- list.files("OPUS", full.names = T)
      
      ##FUNCTION 1: Add Sample Names
      addSampleNames <- function(fname) {
        ## creates list of txt files
        filelist <- lapply(fname, read.delim, header = F)
        
        ## Adding sample IDs (reference to lake core)
        names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)
        
        return(filelist)
      }
      
      #Save list of txt files with sample names
      filelist <- addSampleNames(fname)
      
      # save the transformed df to a new list of df called reformattedData
      reformattedData <- lapply(filelist, function(x) {
        pivot_wider(x, names_from = V1, values_from = V2)
      })
      
      # Unlist the reformattedData list into matrix
      allNames <- lapply(reformattedData, names) 
      
      # convert matrix into dataframe
      allNames2 <- as.data.frame(do.call("rbind",allNames))
      
      wavenumber <- as.numeric(as.vector(unname(t(allNames2[1,])))) ### 1 3697
  
      
#Data frame for different loadings
      #loadings, #first column of random opus txt file)
      testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                                  loadings_2= pls2$loadings[,2], 
                                  loadings_3 = pls2$loadings[,3], 
                                  weighted_loading_1 = pls2$loading.weights[,1], 
                                  weighted_loading_2 = pls2$loading.weights[,2], 
                                  weighted_loading_3 = pls2$loading.weights[,3], 
                                  wavenumber = wavenumber)
      
#Loading Plots----
 
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
      