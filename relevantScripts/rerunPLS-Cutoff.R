library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)

#Load in data 
#data <- read.csv("csvFiles/dataForProofOfConceptNice.csv")
data <- read_csv("csvFiles/resolvedSampleNames-2.csv")
dim(data) ## 28 3699
## 28 datasets, 1 sample ID, 1 actual BSi, 3697 absorbances

#Create truncated dataframe 
data_trunc <- data %>%
  select(2, 3642:3664, 3460:3480, 3227:3345)
dim(data_trunc) #28 164

## < 3750cm-1 -----------------------------------------------------------------------------------
### Select wavenumbers smaller than 3750 cm-1
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data[c(2,1946:3699)], validation = "CV", segments = 5)
summary(pls2)
print(summary(pls2)) #28 1754

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
       subtitle="CV is 5-fold, Only for wavenumbers < 3750cm-1",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()

## Match wavenumber to absorbance used in PLS model----------------------------------------------------------------------------------

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

wavenumber <- wavenumber[c(1944:3697)] # 1 1754

## Create testdata-------------------------------------------------------------------------------

#Data frame for different loadings 
#loadings, #first column of random opus txt file)
testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                            loadings_2= pls2$loadings[,2], 
                            loadings_3 = pls2$loadings[,3], 
                            weighted_loading_1 = pls2$loading.weights[,1], 
                            weighted_loading_2 = pls2$loading.weights[,2], 
                            weighted_loading_3 = pls2$loading.weights[,3], 
                            wavenumber = wavenumber) #1754 

##Loading Plot -------------------------------------------------------------------------------------
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
       title='Loading Plot for Three Components: Full Spectrum, n = 28',
       subtitle= expression("Where wavenumber ranges from 3750 to 368" ~ cm^{-1}))+
  scale_x_reverse() + 
  theme_minimal()

--------------------------------------------------------------------------------
  
  
  
##435-480cm-1 ---------------------------------------------------

### Select wavenumbers smaller than 3750 cm-1
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data[c(2,3664:3642)], validation = "CV", segments = 5)
summary(pls2) 
print(summary(pls2)) #28 23

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
       subtitle="CV is 5-fold, Only for wavenumbers 435-480cm-1",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()

## Match wavenumber to absorbance used in PLS model----------------------------------------------------------------------------------

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

wavenumber <- wavenumber[c(3662:3640)] # 1 23
## Create Test Data-----------------------------------------------------
#Data frame for different loadings 
#loadings, #first column of random opus txt file)
testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                            loadings_2= pls2$loadings[,2], 
                            loadings_3 = pls2$loadings[,3], 
                            weighted_loading_1 = pls2$loading.weights[,1], 
                            weighted_loading_2 = pls2$loading.weights[,2], 
                            weighted_loading_3 = pls2$loading.weights[,3], 
                            wavenumber = wavenumber) #23

##Loading Plot -------------------------------------------------------------------------------------
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
       title='Loading Plot for Three Components: Partial Spectrum, n = 28',
       subtitle= expression("Where wavenumber ranges from 435 to 480" ~ cm^{-1}))+
  scale_x_reverse() + 
  theme_minimal()


##790-830cm-1---------------------------------------------------
### Select wavenumbers smaller than 3750 cm-1
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data[c(2,3480:3460)], validation = "CV", segments = 5)
summary(pls2) 
print(summary(pls2)) #28 21

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
       subtitle="CV is 5-fold, Only for wavenumbers 435-480cm-1",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()
## Match wavenumber to absorbance used in PLS model----------------------------------------------------------------------------------

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

wavenumber <- wavenumber[c(3478:3458)] # 1 21
## Create Test Data-----------------------------------------------------
#Data frame for different loadings 
#loadings, #first column of random opus txt file)
testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                            loadings_2= pls2$loadings[,2], 
                            loadings_3 = pls2$loadings[,3], 
                            weighted_loading_1 = pls2$loading.weights[,1], 
                            weighted_loading_2 = pls2$loading.weights[,2], 
                            weighted_loading_3 = pls2$loading.weights[,3], 
                            wavenumber = wavenumber) #21
##Loading Plot -------------------------------------------------------------------------------------
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
       title='Loading Plot for Three Components: Full Spectrum, n = 28',
       subtitle= expression("Where wavenumber ranges from 790 to 830" ~ cm^{-1}))+
  scale_x_reverse() + 
  theme_minimal()


##1050-1280cm-1---------------------------------------------------
### Select wavenumbers smaller than 3750 cm-1
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data[c(2,3345:3227)], validation = "CV", segments = 5)
summary(pls2) 
print(summary(pls2)) #28 119

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
       subtitle="CV is 5-fold, Only for wavenumbers 1050-1280cm-1",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()
## Match wavenumber to absorbance used in PLS model----------------------------------------------------------------------------------

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

wavenumber <- wavenumber[c(3343:3225)] # 1 21
## Create Test Data-----------------------------------------------------
#Data frame for different loadings 
#loadings, #first column of random opus txt file)
testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                            loadings_2= pls2$loadings[,2], 
                            loadings_3 = pls2$loadings[,3], 
                            weighted_loading_1 = pls2$loading.weights[,1], 
                            weighted_loading_2 = pls2$loading.weights[,2], 
                            weighted_loading_3 = pls2$loading.weights[,3], 
                            wavenumber = wavenumber) #119
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
       title='Loading Plot for Three Components: Full Spectrum, n = 28',
       subtitle= expression("Where wavenumber ranges from 1050 to 1280" ~ cm^{-1}))+
  scale_x_reverse() + 
  theme_minimal()



##435-480, 790-830, 1050-1280
##1050-1280cm-1---------------------------------------------------
### Select wavenumbers smaller than 3750 cm-1
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data[c(2,1946:3699,3664:3642,3345:3227)], validation = "CV", segments = 5)
summary(pls2) 
print(summary(pls2)) #28 119

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
       subtitle="CV is 5-fold, Only for wavenumbers 1050-1280cm-1",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()

###Truncated data----------------------------------------------
##Run Model-----------------
### Select wavenumbers smaller than 3750 cm-1
pls2 <- plsr(BSiPercent~., ncomp = 10, data=data_trunc, validation = "CV", segments = 5)
summary(pls2)
print(summary(pls2)) #28 163

plot(RMSEP(pls2))

## save root mean squared error of prediction to object res
res2 <- RMSEP(pls2)

#Dataframe with cross validation and number of components
Five_fold_CV <- cbind.data.frame(cv = res2$val[1,,], ncomps = 0:10)

#Number of components graph 
ggplot(Five_fold_CV, aes(ncomps, cv)) +
  geom_line() +
  labs(title = "Cross-validated Root Mean Squared Error of Prediction (RMSEP) Curve", 
       subtitle="CV is 5-fold, for three intervals: 435-480, 790-830, 1050-1280cm-1",
       y = "RMSEP", 
       x = "Number of Components") +
  scale_x_continuous(breaks = c(1:10)) + 
  theme_minimal()


##Match Wavenumber to absorbance values------------
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

wavenumber <- wavenumber[c(3640:3662,3458:3478,3225:3343)] # 1 163
## Create testdata-------------------------------------------------------------------------------

#Data frame for different loadings 
#loadings, #first column of random opus txt file)
testdata<- cbind.data.frame(loadings_1 = pls2$loadings[,1], 
                            loadings_2= pls2$loadings[,2], 
                            loadings_3 = pls2$loadings[,3], 
                            weighted_loading_1 = pls2$loading.weights[,1], 
                            weighted_loading_2 = pls2$loading.weights[,2], 
                            weighted_loading_3 = pls2$loading.weights[,3], 
                            wavenumber = wavenumber) #163

##Loading Plot -------------------------------------------------------------------------------------
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
       title='Loading Plot for Three Components: Partial Spectrum, n = 28',
       subtitle= expression("for 3 intervals: 435-480, 790-830, 1050-1280" ~ cm^{-1}))+
  scale_x_reverse() + 
  theme_minimal()

