# C. Muhs
# 10/24/17
# Animate Portal speed-flow plots

library(tidyverse)
library(lubridate)
library(ggplot2)

# devtools::install_github("dgrtwo/gganimate")
library(gganimate)

#############################
# Load data
load(file = "data//eclipse_data.Rdata")

#############################
# Add a field for sequential ordering of time and day plots
mydata$seq_id = 1:nrow(mydata)

nb_data <- mydata %>%
  filter(dir == "NB")
#############################
# plot speed-flow relationship
p = ggplot(data = mydata, aes(tot_vol, avg_spd)) 
p + geom_point() + facet_grid(dir ~ day_of_wk)

#############################
# animated plot
p <- ggplot(data = nb_data, aes(tot_vol, avg_spd, frame = hour_of_day)) +
  geom_point(size = 3, colour = "steelblue4", alpha = 0.9) +
  xlab("Volume, vph") + ylab("Speed, mph")
  
# p
# Notice we added frame = hour_of_day and saved the plot as p. 

# We then display it as an animation with the gg_animate function:
setwd("outputs//")
gganimate(p, interval = 0.2, "output.gif")
