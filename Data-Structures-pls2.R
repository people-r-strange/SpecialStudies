library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)


#Load in data 
data <- read.csv("dataForProofOfConceptNice.csv")
dim(data) ## 39 3698
## 39 datasets, 3697 absorbances, 1 response


### Cross-Validation is 5-fold ###
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
summary(pls2)

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated RMSEP Curve", 
       subtitle="CV is 5-fold",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()
-------------------------------------------------------------------------------------------------------------------
## Load Opus txt file 
# reading in list of txt files and converting it to a df

## read in txt files automatically
fname <- list.files("OPUS", full.names = T)

## creates list of txt files
filelist <- lapply(fname, read.delim, header = F)

## Adding sample IDs (reference to lake core)
names(filelist) <- gsub(".*/(.*)\\..*", "\\1", fname)

# save the transformed df to a new list of df called reformattedData
reformattedData <- lapply(filelist, function(x) {
  pivot_wider(x, names_from = V1, values_from = V2)
})

# Unlist the reformattedData list into matrix
allNames <- lapply(reformattedData, names) 

# convert matrix into dataframe
allNames2 <- as.data.frame(do.call("rbind",allNames))

wavenumber <- as.numeric(as.vector(unname(t(allNames2[1,]))))


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

#Export this data as csv
write.csv(testdata, "testdata.csv")

#Load in data 
testdata <- read.csv("testdata.csv")
## using weighted loadings for loading plot 

#Loading plot for loading 1
# g<- ggplot(testdata, aes(x=wavenumber,y=weighted_loading_1)) + 
#   geom_line () +
#   geom_line (aes(x=wavenumber, y = weighted_loading_2), color = "red") + 
#   geom_line (aes(x=wavenumber, y = weighted_loading_3), color = "blue") + 
#   
#   labs(title = "Loading Plot for Three Components",
#        y = "Weighted Loading", 
#        x = "Wavenumber (cm−1)") + 
#   scale_x_reverse() + 
#   theme_minimal()
# g
# 
# ggplotly(g)

#Loading plot for Full Spectrum loading 1 with legend and vertical lines

#vline <- c("4000", "450")               # Define positions of vline
#vline <- which(testdata$wavenumber %in% vline)

  ggplot(testdata, aes(x=wavenumber)) + 
  geom_line (aes(y = weighted_loading_1, colour = "1st Component")) +
  geom_line (aes(y = weighted_loading_2, color = "2nd Component")) + 
  geom_line (aes(y = weighted_loading_3, colour = "3rd Component")) + 
  
  scale_colour_manual("", 
                      breaks = c("1st Component", "2nd Component", "3rd Component"),
                      values = c("blue", "dark green", "orange")) +
  
  #geom_vline(xintercept = 450) + 
  #geom_vline(xintercept = 4000) +
  
  labs(y="Weighted Loadings", 
       x=expression(Wavenumber(cm^-1)), 
       title='Loading Plot for Three Components: Full Spectrum',
       subtitle= expression("Where wavenumber ranges from ~7500 to ~370" ~ cm^{-1})) +
  scale_x_reverse() + 
  theme_minimal()



#https://eigenvector.com/different-kinds-of-pls-weights-loadings-and-what-to-look-at/

## R = W(P’W)-1
## Answer: R is the same thing as weighted loading, so going to just use loading 

testdata$Loading_1_R = as.vector(testdata$weighted_loading_1 %*% solve(t(testdata$loadings_1 ) %*% testdata$weighted_loading_1)) 

ggplot(testdata, aes(x=wavenumber,y=loadings_1))  + geom_point() + 
  geom_point(aes(x=wavenumber,y=weighted_loading_1), color = "blue") + 
    geom_point(aes(x=wavenumber,y=Loading_1_R), color = "green")

ggplot(testdata, aes(x = wavenumber, y = Loading_1_R)) + geom_point()

png("testloadingplot.png")
print(p)
dev.off()
  
  