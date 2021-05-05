library(pls)
library(tidyverse)
library(scales)
library(plotly)

#Load in data 
data <- read.csv("testdata.csv")

#Select Wavenumber >4000
small_wavenumber <- data %>%
  filter(wavenumber <= 4000)

#Loading plot for three components where wavenumber > 4000
ggplot(small_wavenumber, aes(x=wavenumber)) + 
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
       title='Loading Plot for Three Components: Partial Spectrum',
       subtitle = expression("where wavenumber is smaller than or equal to 4000" ~ cm^{-1})) +
  scale_x_reverse() + 
  theme_minimal()
