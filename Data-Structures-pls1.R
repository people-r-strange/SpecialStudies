library(pls)
library(tidyverse)

data <- read.csv("dataForProofOfConceptNice.csv")
dim(data) ## 39 3698
## 39 datasets, 3697 absorbances, 1 response

### LOO ###
pls1 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "LOO")

names(pls1)

### Check Class and Dim for all names ###
---------------------------------------------------------
## 39 3698 ##
## 39 datasets, 3697 absorbances, 1 response
  ##model
  pls1$model %>% class() ## data.frame
  pls1$model %>% dim() ## 39 3698
----------------------------------------------------------
## 3697 10 ## 
## 3697 absorbances, 10 components
  ##Loadings
  pls1$loadings %>% class() ##loadings
  pls1$loadings %>% dim() ## 3697 10
  ##Loading.weights
  pls1$loading.weights %>% class() ##loads
  pls1$loading.weights %>% dim() ## 3697 10
  ##Projection
  pls1$projection %>% class() ## matrix array
  pls1$projection %>% dim() ## 3697 10
-------------------------------------------------------------
## 3697 1 10 ## 
## 3697 absorbances, 10 coefficients each (1 per component)
  ##Coefficients
  pls1$coefficients %>% class() ## array
  pls1$coefficients %>% dim() ## 3697 1 10
--------------------------------------------------------------
## 39 10 ## 
## 39 datasets, 10 components
  ##Scores
  pls1$scores %>% class() ## scores
  pls1$scores %>% dim() ## 39 10
  ##Yscores
  pls1$Yscores %>% class() ##scores
  pls1$Yscores %>% dim() ## 39 10
-------------------------------------------------------------
## 39 1 10 ##
## 39 datasets, 1 ???, 10 components
  ##fitted.values
  pls1$fitted.values %>% class() ## array
  pls1$fitted.values %>% dim() ## 39 1 10
  ##residuals
  pls1$residuals %>% class() ## array
  pls1$residuals %>% dim() ## 39 1 10
------------------------------------------------------------
## 1 10 ##
## 1 ??? , 10 components  
  ##YLoadings
  pls1$Yloadings %>% class() ##loadings
  pls1$Yloadings %>% dim() ## 1 10
------------------------------------------------------------
## NULL ## 
    
##Xmeans
  pls1$Xmeans %>% class() ## numeric
  pls1$Xmeans %>% dim() ## NULL
##Ymeans
  pls1$Ymeans %>% class() ## numeric
  pls1$Ymeans %>% dim() ## NULL
##Xvar
  pls1$Xvar %>% class() ## numeric
  pls1$Xvar %>% dim() ## NULL
##Xtotvar
  pls1$Xtotvar %>% class() ## numeric
  pls1$Xtotvar %>% dim() ## NULL
##fit.time
  pls1$fit.time %>% class() ## numeric
  pls1$fit.time %>% dim() ## NULL
##ncomp 
  pls1$ncomp %>% class() ## numeric
  pls1$ncomp %>% dim() ## NULL
##method
  pls1$method %>% class() ## character
  pls1$method %>% dim() ## NULL
##call
  pls1$call %>% class() ## call
  pls1$call%>% dim() ## NULL
##terms
  pls1$terms%>% class() ## terms formula
  pls1$terms %>% dim() ## NULL
--------------------------------------------------------
## List ##
##validation 
  pls1$validation %>% class() ## list
  pls1$validation %>% length() ## 9

##names in validation 
  names(pls1$validation)
  #"method","pred","coefficients","gammas","PRESS0","PRESS","adj","segments","ncomp"     
  
##method
  pls1$validation$method ## CV
##pred
  pls1$validation$pred ## BSiPercent
  
  pls1$validation$pred %>% class()
  pls1$validation$pred %>% dim() ## 39 1 10
  ## from help file for mvCv (sent to from plsr help file)
  ## cross-validated predictions
  
  pls1$validation$pred[,,1]
  
##coefficients
  pls1$validation$coefficients ## NULL 
##gammas
  pls1$validation$gammas ## NULL 
##PRESS0
  pls1$validation$PRESS0 ## BSiPercent 1860.271  
##PRESS
  pls1$validation$PRESS ## BSiPercent for 10 Components
##adj
  pls1$validation$adj ## BSiPercent for 10 components (smaller numbers)
  ## from help file for mvCv (sent to from plsr help file)
  ## a matrix of adjustment values for calculating bias corrected MSEP
  ## probably don't have to go there, at least for right  now
##segments
  pls1$validation$segments %>% length() ## 39, leave one out
##ncomp
  pls1$validation$ncomp ## 10 components
-----------------------------------------------------------------------------
## root mean squared error of prediction
    RMSEP(pls1) 
    plot(RMSEP(pls1))
  
    names(RMSEP(pls1))
    # "val"        "type"       "comps"      "cumulative" "call" 
    
## save root mean squared error of prediction to object res
  res <- RMSEP(pls1)
  
##val
  res$val ## ugh why
  res$val %>% class() ## array
  res$val %>% dim() ## 2 x 1 x 11
  
  res$val[1,,2] ## CV, one component
  res$val[1,,11] ## CV, ten components
  
  res$val[2,,2] ## adjusted CV, one component
  res$val[2,,11] ## adjustedCV, ten components
##type
  res$type ## RMSEP
  res$type%>%class() ## character
##comps
  res$comps ## 11 (0-10)
  res$comps %>% class() ## numeric
##cumulative
  res$cumulative ## TRUE
  res$cumulative %>% class() ##logical
##call
  res$call ## object = pls1
  
  
  
  
  
  
  









