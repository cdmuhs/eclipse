# C. Muhs
# 10/18/17
# Plot Portal speed-volume charts before & during eclipse

#############################
# Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)

#############################
# Read week of eclipse data. This is dowlnoaded from Portal --> Segemnts --> Explore --> Configure volume & speed plot --> "Download all data" 
sb = read_csv("C://temp//all_data_I5_SB_at_Wilsonville_Rd_2R399_to_SB_I5_08-20-2017_to_08-23-2017_data.csv")
nb = read_csv("C://temp//all_data_I5_NB_at_Wilsonville_Rd_2R396_to_NB_I5_08-20-2017_to_08-23-2017_data.csv")

nb = nb[, 1:10] # remove "lane_r1_vol" column

sb$dir = "SB"
nb$dir = "NB"

mydata = rbind(sb, nb)

# Read the previous 6 mondays of data, i.e., before the eclipse ("_b" = before)
sb_b = read_csv("C://temp//all_data_I5_SB_at_Wilsonville_Rd_2R399_to_SB_I5_07-10-2017_to_08-14-2017_data.csv")
nb_b = read_csv("C://temp//all_data_I5_NB_at_Wilsonville_Rd_2R396_to_NB_I5_07-10-2017_to_08-14-2017_data.csv")
nb_b = nb_b[, 1:10] # remove "lane_r1_vol" column
sb_b$dir = "SB"
nb_b$dir = "NB"
mydata_b = rbind(sb_b, nb_b)

#############################
# Average all lanes. This is not a great function but it works for this purpose
mymean = function(v1, v2, v3){
    newvar = ((v1 + v2 + v3)/(nargs()))
    return(newvar)
}

mydata$avg_spd = mymean(mydata$lane_1_spd, mydata$lane_2_spd, mydata$lane_3_spd)
mydata$avg_vol = mymean(mydata$lane_1_vol, mydata$lane_2_vol, mydata$lane_3_vol)
mydata$tot_vol = (mydata$lane_1_vol + mydata$lane_2_vol + mydata$lane_3_vol) * 12

mydata_b$avg_spd = mymean(mydata_b$lane_1_spd, mydata_b$lane_2_spd, mydata_b$lane_3_spd)
mydata_b$avg_vol = mymean(mydata_b$lane_1_vol, mydata_b$lane_2_vol, mydata_b$lane_3_vol)
mydata_b$tot_vol = (mydata_b$lane_1_vol + mydata_b$lane_2_vol + mydata_b$lane_3_vol) * 12

#############################
# Configure time field
mydata$time_pst = with_tz(mydata$starttime, tzone = "America/Los_Angeles") # Shift UTC to PST
mydata$day_of_wk = wday(mydata$time_pst, label = TRUE) # Make day of week variable
mydata$hour_of_day = hour(mydata$time_pst) # Make hour of day variable

mydata_b$time_pst = with_tz(mydata_b$starttime, tzone = "America/Los_Angeles") # Shift UTC to PST
mydata_b$day_of_wk = wday(mydata_b$time_pst, label = TRUE) # Make day of week variable
mydata_b$hour_of_day = hour(mydata_b$time_pst) # Make hour of day variable

#############################
# Filter data

# Monday of the eclipse
monday_eclipse_data = mydata %>%
    filter(day_of_wk == "Mon") %>%
    filter(dir == "NB")

# Mondays before the eclipse
typ_monday_data = mydata_b %>%
    filter(day_of_wk == "Mon") %>%
    filter(time_pst < as.Date("2017-08-01 00:00:00") & time_pst > as.Date("2017-07-30 00:00:00")) %>%
    filter(dir == "NB")

#############################
# Combine data for one plot
monday_eclipse_data$status = "NB, Eclipse Monday"
typ_monday_data$status = "NB, Typical Monday"
mydata_plot = rbind(monday_eclipse_data, typ_monday_data)


#############################
# plot speed-flow relationship

setwd("T://Muhs R scripts//Portal//outputs")

p = ggplot(data = mydata_plot, aes(tot_vol, avg_spd)) 
p + 
    geom_point(colour = "steelblue4", alpha = 0.7) + 
    facet_wrap(~status) +
    xlab("Volume, vph") + ylab("Speed, mph") +
    scale_x_continuous(limits = c(0, 6000)) +
    scale_y_continuous(limits = c(0, 75))
ggsave(file = paste(paste(Sys.Date(), "speed_flow_eclipse_vs_typical", sep = "_"), ".jpg", sep = ""), 
       width = 6.5, height = 3, units = 'in')

# Plot by each lane
# Re-organize data from wide to long
data_wide <- mydata_plot %>%
    select(time_pst, ends_with("spd"), ends_with("vol"), status)

data_long <- data_wide %>%
    gather(var_speed, speed, lane_1_spd:avg_spd) %>%
    gather(var_vol, volume, lane_1_vol:tot_vol) %>%
    filter((var_speed == "lane_1_spd" & var_vol == "lane_1_vol") |
               (var_speed == "lane_2_spd" & var_vol == "lane_2_vol") |
               (var_speed == "lane_3_spd" & var_vol == "lane_3_vol") |
               (var_speed == "avg_spd" & var_vol == "tot_vol"))

# Add variable for which lane this is.
data_long$lane[data_long$var_speed == "lane_1_spd" & data_long$var_vol == "lane_1_vol"] <- "Lane 1"
data_long$lane[data_long$var_speed == "lane_2_spd" & data_long$var_vol == "lane_2_vol"] <- "Lane 2"
data_long$lane[data_long$var_speed == "lane_3_spd" & data_long$var_vol == "lane_3_vol"] <- "Lane 3"
data_long$lane[data_long$var_speed == "avg_spd" & data_long$var_vol == "tot_vol"] <- "All lanes"

ggplot(data = data_long, aes(volume, speed)) +
    geom_point(colour = "firebrick4", alpha = 0.7) + 
    facet_grid(status ~ lane, scales = "free_x") +
    xlab("Volume, vph") + ylab("Speed, mph")


# p_b = ggplot(data = typ_monday_data, aes(tot_vol, avg_spd)) 
# p_b +
#     geom_point(colour = "palegreen4", alpha = 0.7) + 
#     # facet_wrap(~dir) +
#     xlab("Volume, vph") + ylab("Speed, mph") + 
#     scale_x_continuous(limits = c(0, 6000)) +
#     scale_y_continuous(limits = c(0, 75))
# ggsave(file = paste(paste(Sys.Date(), "speed_flow_typical_monday", sep = "_"), ".jpg", sep = ""), 
#        width = 6.5, height = 3, units = 'in')


#############################
# Export combined data
save(mydata, file = "C://temp//eclipse_data.Rdata")
