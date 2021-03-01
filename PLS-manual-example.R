#load packages 
library(pls)
library(tidyverse)

#load example data sets
#1: Yarn
#2: Olive Oil
#3: Gasoline 

data("yarn")
data("oliveoil")
data("gasoline")

#Examing gasoline data set
str(gasoline)
glimpse(gasoline)
View(gasoline)

#Dividing gasoline data into training and testing sets
gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

#Examining training and testing data set 
str(gasTest)
glimpse(gasTest)
View(gasTest)

str(gasTrain)
glimpse(gasTrain)
View(gasTrain)

#Fitting a PLSR model 
gas1 <- plsr(octane~NIR, ncomp = 10, data=gasTrain, validation = "LOO")

summary(gas1)

##Validation Results are Root Mean Squared Error of Prediction (RMSEP) 

##There are two cross-validation estimates: CV is ordinary CV estimate and adjCV is bias-corrected CV estimate

#Plotting RMSEPs 
plot(RMSEP(gas1), legendpos = 'topright')

##This plot estimated RMSEPs as functions of the number of components. Two components seem to be enough. So we've determined the number of components. 

#Inspecting different aspects of the fit (prediction plot, other plots) 

#Prediction plot 
plot(gas1, ncomp = 2, asp = 1, line = TRUE)

##This figure shows the cross-validated predictions with two components versus measured values. There is no curvature. 

#Scores plot 
plot(gas1, plottype = "scores", comps = 1:3)

##This gives pairwise plot of the score values for the three first components. Score plots are used to look for patterns, groups, or outliers. In this example, there is no indication of grouping or outliers. The numbers in parentheses are relative amount of X variance explained by each component 

#Explained variance 
explvar(gas1)

#Loading plot 
plot(gas1, "loadings", comps = 1:2, legenpos = 'topleft', labels = "numbers", xlab = "nm") 

abline(h=0)

##This plot is used for interpretation purposes, like looking for known spectral peaks or profiles

#Predicting Response values of new observations 
prediction <- predict(gas1, ncomp = 2, newdata = gasTest)

print(prediction)

#Calculate test set RMSEP 
RMSEP(gas1, newdata= gasTest)

##For two components we get 0.244 which is quite close to the cross-validated estimate above (0.297) 

#Data Frames 
##To create a data frame, one can use the data.frame function: if v1, v2 and v3 are factors or numeric vectors, mydata <- data.frame(y = v1, a = v2, b = v3) will result in a data frame with variables named y, a and b.

##PLSR and PCR are often used with a matrix as the single predictor term (especially when one is working with spectroscopic data). Also, multiresponse models require a matrix as the response term. If Z is a matrix, it has to be protected by the ‘protect function’ I() in calls to data.frame: mydata <- data.frame(..., Z = I(Z)). Otherwise, it will be split into separate variables for each column, and there will be no variable called Z in the data frame, so we cannot use Z in the formula. One can also add the matrix to an existing data frame: > mydata <- data.frame(...) > mydata$Z <- Z

##This will also prevent Z from being split into separate variables. Finally, one can use cbind to combine vectors and matrices into matrices on the fly in the formula. This is most useful for the response, e.g., cbind(y1, y2) ~ X.

##Variables in a data frame can be accessed with the $ operator, e.g., mydata$y. However, the pls functions access the variables automatically, so the user should never use $ in formulas.
