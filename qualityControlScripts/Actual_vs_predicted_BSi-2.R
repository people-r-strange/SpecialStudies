library(pls)
library(tidyverse)
library(scales)
library(plotly)
library(readr)
library(dplyr)
library(Metrics)
library(moderndive)
library(ggplot2)

#Load data
actual_bsi <- read_csv("csvFiles/resolvedSampleNames-2.csv")
dim(actual_bsi) # 28 3699
names(actual_bsi)

pls2 <- plsr(BSiPercent~., ncomp = 10, data=actual_bsi, validation = "CV", segments = 5)

pls2$fitted.values

#Convert predicted BSi percents into data frame
predicted_bsi <- as.data.frame(pls2$fitted.values)
dim(predicted_bsi) # 28 10

#select model with 3 components
predicted_bsi_3 <- as.data.frame(predicted_bsi[,c(3)])

#Rename wet_chem_data columns 
names(predicted_bsi_3 )[1] <- "BSiPercent_Predicted"

#Isolate actual BSi percent from wet chem lab data
actual_bsi_wetchem <- actual_bsi %>%
  select(dataset, BSiPercent)
dim(actual_bsi_wetchem) # 28 2

BSi <- cbind(actual_bsi_wetchem, predicted_bsi_3)

BSi_Long <- BSi %>%
  select(dataset, BSiPercent, BSiPercent_Predicted)%>%
  gather(key = "variable", value = "value", -dataset)

##GRAPH ------------------------------------------------------------------------------
ggplot(BSi_Long, aes(x = dataset, y = value, fill=variable))+
  geom_col(position= position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Values",
                    breaks=c("BSiPercent", "BSiPercent_Predicted"),
                    labels=c("Actual", "Predicted")) +
  labs(
    y = "BSi Percentage",
    x = "Sample ID",
    title="Full Spectrum Model Accuracy, n = 28",
    subtitle= "Wet Chemical Digestion vs. Calibrated Model Prediction",
    colour = "variabl") + 
  theme(legend.position = c(0.15, 0.9),
        axis.text.x  = element_text(angle = 90)) 

#Difference between actual and predicted
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

#Visually Represent difference 
Difference %>% 
  mutate(highlight_flag = ifelse(Difference >= '0', T, F)) %>% 
  ggplot (aes(x = dataset, y = Difference)) + 
  geom_col(aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('red', 'green'), name = "Overfitting") +
  geom_text( data = Difference, aes(label = Difference), fontface ="bold", size = 2.5, vjust = 0) +
  labs(
    y = "Difference in Percentage ",
    x = "Sample ID",
    title="Full Spectrum Model Accuracy, n = 28",
    subtitle= "Visually representing the difference between the predicted and actual BSi Percentages",
    colour = "variabl") + 
  theme(legend.position = c(0.15, 0.9),
        axis.text.x  = element_text(angle = 90)) 
