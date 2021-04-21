library(pls)
library(tidyverse)

data <- read.csv("dataForProofOfConceptNice.csv")
dim(data) ## 39 3698
## 39 datasets, 3697 absorbances, 1 response

### LOO ###
pls1 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "LOO")

names(pls1)

### Check Class and Dim for all names ###

##Coefficients
pls1$coefficients %>% class() ## array
pls1$coefficients %>% dim() ## 3697 1 10
## 3697 absorbances, 10 coefficients each (1 per component)

##Scores
pls1$scores %>% class() ## scores
pls1$scores %>% dim() ## 39 10
## 39 datasets, 10 components

##Loadings
pls1$loadings %>% class() ##loadings
pls1$loadings %>% dim() ## 3697 10
## 3697 absorbances, 10 components

##Loading.weights
pls1$loading.weights %>% class() ##loads
pls1$loading.weights %>% dim() ## 3697 10
## 3697 absorbances, 10 components

##Yscores
pls1$Yscores %>% class() ##scores
pls1$Yscores %>% dim() ## 39 10
## 39 datasets, 10 components

##YLoadings
pls1$Yloadings %>% class() ##loadings
pls1$Yloadings %>% dim() ## 1 10
## What does the 1 refer to? , 10 components 

##Projection
pls1$projection %>% class() ## matrix array
pls1$projection %>% dim() ## 3697 10
## 3697 absorbances, 10 components 



plot(pls1$loadings[,1])
points(pls1$loading.weights[,1], col="red")
## which do we want?




## TO DO: keep going 

## ... skip ahead for now

pls1$residuals %>%  class()
pls1$residuals %>%  dim()
## 39 1 10
## 39 datasets, 10 components (one for each model with 1, ... 10 components)

summary(pls1$residuals[,,1]) ## one component
summary(pls1$residuals[,,2]) ## two components
## observed - expected (predicted), can be used to calculate RMSE

pls1$Xtotvar %>% class()
pls1$Xtotvar ## not sure about this, need to dig into documentation

pls1$validation %>% class()
pls1$validation %>% length()
names(pls1$validation)

pls1$validation$method ## CV
pls1$validation$segments %>% length() ## 39, leave one out

pls1$validation$adj
## from help file for mvCv (sent to from plsr help file)
## a matrix of adjustment values for calculating bias corrected MSEP
## probably don't have to go there, at least for right  now


pls1$validation$pred %>% class()
pls1$validation$pred %>% dim() ## 39 1 10
## from help file for mvCv (sent to from plsr help file)
## cross-validated predictions

pls1$validation$pred[,,1]


RMSEP(pls1) ## root mean squared error of prediction
plot(RMSEP(pls1))

names(RMSEP(pls1))

res <- RMSEP(pls1)

res$comps
res$type
res$val ## ugh why
res$val %>% class()
res$val %>% dim() ## 2 x 1 x 11

res$val[1,,2] ## CV, one component
res$val[1,,11] ## CV, ten components

res$val[2,,2] ## adjusted CV, one component
res$val[2,,11] ## adjustedCV, ten components

data <- cbind.data.frame(cv = res$val[1,,], ncomps = 0:10)
ggplot(data, aes(ncomps, cv))+geom_point()+geom_line()

### CV ###

pls2 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
summary(pls2)
# help for cvsegments
#The function generates a list of segments for cross-validation. It can generate random, consecutive and interleaved segments, and supports keeping replicates in the same segment.
# it would be helpful to know what segments are what, so pre-specifying as a list them might be our best bet

pls2$coefficients %>% class() ## array
pls2$coefficients %>% dim() ## 3697 1 10
## 3697 absorbances, 10 coefficients each (1 per component)


pls2$scores %>% class()
pls2$scores %>% dim() ## 39 10
## 39 datasets, 10 components

pls2$loadings %>% class()
pls2$loadings %>% dim() ## 3697 10
## 3697 absorbances, 10 components


## TO DO: keep going 

## ... skip ahead for now

pls2$residuals %>%  class()
pls2$residuals %>%  dim()
## 39 1 10
## 39 datasets, 10 components (one for each model with 1, ... 10 components)

summary(pls2$residuals[,,1]) ## one component
summary(pls2$residuals[,,2]) ## two components
## observed - expected (predicted), can be used to calculate RMSE

pls2$Xtotvar %>% class()
pls2$Xtotvar ## not sure about this, need to dig into documentation

pls2$validation %>% class()
pls2$validation %>% length()
names(pls2$validation)

pls2$validation$method ## CV
pls2$validation$segments %>% length() ## 39, leave one out

pls2$validation$adj
## from help file for mvCv (sent to from plsr help file)
## a matrix of adjustment values for calculating bias corrected MSEP
## probably don't have to go there, at least for right  now


pls2$validation$pred %>% class()
pls2$validation$pred %>% dim() ## 39 1 10
## from help file for mvCv (sent to from plsr help file)
## cross-validated predictions


RMSEP(pls2) ## root mean squared error of prediction
plot(RMSEP(pls2))

names(RMSEP(pls2))

res <- RMSEP(pls2)

res$comps
res$type
res$val ## ugh why
res$val %>% class()
res$val %>% dim()

res$val[1,,2] ## CV, one component
res$val[1,,11] ## CV, ten components

res$val[2,,2] ## adjusted CV, one component
res$val[2,,11] ## adjustedCV, ten components

