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
actual_bsi <- read_csv("dataForProofOfConceptNice.csv")
dim(actual_bsi) # 39 3698
names(actual_bsi)

#Run pls // calibration model
pls2 <- plsr(BSiPercent~., ncomp = 10, data=actual_bsi, validation = "CV", segments = 5)
dim(pls2)

pls2$fitted.values

#Convert predicted BSi percents into data frame
predicted_bsi <- as.data.frame(pls2$fitted.values)
dim(predicted_bsi) # 39 10

#select model with 3 components
predicted_bsi_3 <- predicted_bsi[,c(3)]

write_csv(predicted_bsi_3, "predicted_bsi_3comp.csv")

#Isolate actual BSi percent from wet chem lab data
actual_bsi_wetchem <- actual_bsi %>%
  select(BSiPercent)
dim(actual_bsi_wetchem) # 39  1

write_csv(actual_bsi_wetchem,"actual_bsi.csv")

#Load in manually created csv file 
BSi <- read_csv("Actual_Predicted_BSi.csv")

#Reformat into Long 
BSi_Long <- BSi %>%
  select(Sample, Actual_Bsi, `BSiPercent.3 comps`)%>%
  gather(key = "variable", value = "value", -Sample)


#Graph actual vs. comp 1
ggplot(BSi_Long, aes(x = Sample, y = value, fill=variable))+
  geom_col(position= position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Values",
                    breaks=c("Actual_Bsi", "BSiPercent.3 comps"),
                    labels=c("Actual", "Predicted")) +
  labs(
    y = "BSi Percentage",
    x = "Sample ID",
    title="Model Accuracy",
    subtitle= "Wet Chemical Digestion vs. Calibrated Model Prediction",
    colour = "variabl") + 
  theme(legend.position = c(0.15, 0.9),
        axis.text.x  = element_text(angle = 90)) 

ggplot(BSi_Long, aes(x = Sample, y = value, fill=variable))+
  geom_col(position= position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Values",
                    breaks=c("Actual_Bsi", "BSiPercent.3 comps"),
                    labels=c("Actual", "Predicted")) +
  labs(
    y = "BSi Percentage",
    x = "Sample ID",
    title="Full Spectrum Model Accuracy",
    subtitle= "Wet Chemical Digestion vs. Calibrated Model Prediction",
    colour = "variabl") + 
  theme(legend.position = c(0.15, 0.9),
        axis.text.x  = element_text(angle = 90)) 

# Line graph
ggplot(BSi, aes(x=Sample)) + 
  geom_line (aes(y = Actual_Bsi, group=1, colour = "Wet Chemical Digestion")) +
  geom_line (aes(y = `BSiPercent.3 comps`, group=1, color = "PLS Prediction")) + 
  
  scale_colour_manual("", 
                      breaks = c("Wet Chemical Digestion", "PLS Prediction"),
                      values = c("grey","orange")) +
labs(
  y = "BSi Percentage",
  x = "Sample ID",
  title="Full Spectrum Model Accuracy",
  subtitle= "Checking whether rank and numerical values are aligned",
  colour = "variabl") + 
  theme(legend.position = c(0.15, 0.9),
        axis.text.x  = element_text(angle = 90)) 

#Difference between actual and predicted
BSi$Difference <- (BSi$`BSiPercent.3 comps` - BSi$Actual_Bsi)

#Table of differences for each sample 
Difference <- BSi %>% 
  select(Sample, Difference)

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
ggplot (aes(x = Sample, y = Difference)) + 
  geom_col(aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('red', 'green'), name = "Overfitting") +
  geom_text( data = Difference, aes(label = Difference), fontface ="bold", size = 2.5, vjust = 0) +
  labs(
    y = "Difference in Percentage ",
    x = "Sample ID",
    title="Full Spectrum Model Accuracy",
    subtitle= "Visually representing the difference between the predicted and actual BSi Percentages",
    colour = "variabl") + 
  theme(legend.position = c(0.15, 0.9),
        axis.text.x  = element_text(angle = 90)) 


#Facet_grid
ggplot(BSi_Long,
       aes(x = Sample,
           y = value, 
           group = variable,
           fill = variable)) + 
  theme_minimal() +
  geom_col() +
  labs(y="", 
       x="Sample",
       title="Model Efficacy",
       subtitle= "Actual vs. Predicted BSi Percentages") +
  theme(
    axis.text.x  = element_text(angle = 90),
    legend.position = "right") + 
  facet_grid(rows = vars(variable), scales = "free")

#Facet_wrap
ggplot(BSi_Long,
       aes(x = Sample,
           y = value, 
           group = variable,
           fill = variable)) + 
  theme_minimal() +
  geom_col() +
  labs(y="", 
       x="Sample",
       title="Model Efficacy",
       subtitle= "Actual vs. Predicted BSi Percentages") +
  theme(
    axis.text.x  = element_text(angle = 90),
    legend.position = "right") + 
  facet_wrap(~variable) 

#Linear Model 
my_mod <- lm(Actual_Bsi~ `BSiPercent.3 comps`, BSi)    

residual <- as.data.frame(get_regression_points(my_mod))

res<- residual %>%
  select(residual)

#Long Residual 
Residual_Long <- residual %>%
  select(ID, Actual_Bsi, `BSiPercent.3 comps`)%>%
  gather(key = "variable", value = "value", -ID)

res_long <- cbind(res, Residual_Long)

#Graph Residuals
ggplot(res_long,
              aes(x = ID,
                  y = value, 
                  group = variable,
                  fill = variable)) +
  geom_col(position = position_dodge()) + 
  geom_line(y = residual ) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c("#666666", "#258e70", "#00dfff"), 
                    name="Values",
                    breaks=c("Actual_Bsi", "BSiPercent.3 comps", "residual"),
                    labels=c("Actual", "Predicted", "Residual Error")) +
  labs(
    y = "BSi Percentage",
    x = "Sample ID",
    title="Model Accuracy",
    subtitle= "Wet Chemical Digestion vs. Calibrated Model Prediction",
    colour = "variable") + 
  theme(legend.position = c(0.15, 0.9))

