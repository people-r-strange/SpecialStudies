library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)

#Load data with wet chem bsi % and absorbance values
actual_bsi <- read_csv("csvFiles/resolvedSampleNames-2.csv")
dim(actual_bsi) # 28 3699
names(actual_bsi)
#Isolate actual BSi percent from wet chem lab data
actual_bsi_wetchem <- actual_bsi %>%
  select(dataset, BSiPercent)
dim(actual_bsi_wetchem) #28  2

#Load in data 
#data <- read.csv("csvFiles/dataForProofOfConceptNice.csv")
data <- read_csv("csvFiles/resolvedSampleNames-2.csv")
dim(data) ## 28 3699
## 28 datasets, 1 sample ID, 1 actual BSi, 3697 absorbances

## All Spectrum
### Select wavenumbers smaller than 3750 cm-1
pls2_AS <- plsr(BSiPercent~., ncomp = 10, data=data, validation = "CV", segments = 5)
summary(pls2_AS)
print(summary(pls2_AS)) #28 3724 

##Load data without wet chem
data_Predict <- read_csv("csvFiles/OhneWetChem.csv")
dim(data) ## 28 3698

##Predict BSi Using pls2 model
predicted_bsi <- as.data.frame(predict(pls2_AS, ncomp = 1:10, newdata = data_Predict))
dim(predicted_bsi) #28 10

#select model with 3 components
predicted_bsi_3 <- as.data.frame(predicted_bsi[,c(3)])
#Rename wet_chem_data columns 
names(predicted_bsi_3 )[1] <- "BSiPercent_Predicted"

#Combine actual and predicted BSi wetchem data
BSi <- cbind(actual_bsi_wetchem, predicted_bsi_3)
dim(BSi) #28  3

#reformat into long so we can graph 
BSi_Long <- BSi %>%
  select(dataset, BSiPercent, BSiPercent_Predicted)%>%
  gather(key = "variable", value = "value", -dataset) 

dim(BSi_Long) #56  3

##GRAPH-----------------------------------------------

#Model Accuracy
ggplot(BSi_Long, aes(x = dataset, y = value, fill=variable))+
  geom_col(position= position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Values",
                    breaks=c("BSiPercent", "BSiPercent_Predicted"),
                    labels=c("Actual", "Predicted")) +
  labs(
    y = "BSi Percentage",
    x = "Sample ID",
    title= expression("Model Accuracy: Interval 435-480" ~ cm^{-1}),
    subtitle= "For 28 Samples",
    colour = "variabl") + 
  theme(legend.position = c(0.125, 0.8),
        axis.text.x  = element_text(angle = 90)) 

#Calculating Residuals: Difference between actual and predicted
BSi$Difference <- (BSi$BSiPercent_Predicted - BSi$BSiPercent)

#Table of differences for each sample 
Difference <- BSi %>% 
  select(dataset, Difference)

#Function to round digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

Difference <- round_df(Difference, 2)

Abs <- (abs(Difference$Difference))

mean(Abs) 
median(Abs)

#Visually Represent difference 
Difference %>% 
  mutate(highlight_flag = ifelse(Difference >= '0', T, F)) %>% 
  ggplot (aes(x = dataset, y = Difference)) + 
  geom_col(aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('red', 'green'), name = "Overfitting") +
  geom_text( data = Difference, aes(label = Difference), fontface ="bold", size = 2.5, vjust = 0) +
  labs(
    y = "Difference in Percentage",
    x = "Sample ID",
    title=expression("Residuals: Interval 435-480" ~ cm^{-1}),
    subtitle= "For 28 Samples",
    colour = "variabl") + 
  theme(legend.position = c(0.1, 0.85),
        axis.text.x  = element_text(angle = 90)) 


