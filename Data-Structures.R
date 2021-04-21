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
##validation 
  pls1$validation %>% class() ## list
  pls1$validation %>% dim() ## NULL
##call
  pls1$call %>% class() ## call
  pls1$call%>% dim() ## NULL
##terms
  pls1$terms%>% class() ## terms formula
  pls1$terms %>% dim() ## NULL










