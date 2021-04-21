library(pls)
library(tidyverse)

data <- read.csv("dataForProofOfConceptNice.csv")
dim(data) ## 39 3698
## 39 datasets, 3697 absorbances, 1 response

### PLS2 ###
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
summary(pls2)