library(pls)
library(tidyverse)

data <- read.csv("dataForProofOfConceptNice.csv")
dim(data) ## 39 3698
## 39 datasets, 3697 absorbances, 1 response

### PLS2 ###
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
summary(pls2)

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

data <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)
ggplot(data, aes(ncomps, cv))+geom_point()+geom_line()+
  labs(title = "RMSEP pls2", 
       subtitle="CV= 5-fold",
       y = "CV", 
       x = "Components") 
