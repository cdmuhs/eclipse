# C. Muhs
# 8/21/17
# Prepare Portal data for Sundays

library(tidyverse)
library(lubridate)

#############################
# Read data. This is dowlnoaded from Portal --> Segemnts --> Explore --> Configure volume & speed plote --> "Download all data" 
# mydata = read_csv("C://temp//all_data_I5_SB_at_Wilsonville_Rd_2R399_to_SB_I5_06-05-2017_to_08-18-2017_data.csv")
# mydata = read_csv("C://temp//all_data_I5_SB_at_Wilsonville_Rd_2R399_to_SB_I5_08-20-2017_to_08-20-2017_data.csv")
# mydata = read_csv("C://temp//all_data_I5_SB_at_Wilsonville_Rd_2R399_to_SB_I5_06-09-2017_to_08-11-2017_data.csv")
mydata = read_csv("C://temp//all_data_I5_SB_at_Wilsonville_Rd_2R399_to_SB_I5_08-18-2017_to_08-18-2017_data.csv")



#############################
# Average all lanes. This is a bad function but hey it works
mymean = function(v1, v2, v3){
  newvar = ((v1 + v2 + v3)/3)
  return(newvar)
}

mydata$avg_spd = mymean(mydata$lane_1_spd, mydata$lane_2_spd, mydata$lane_3_spd)
mydata$avg_vol = mymean(mydata$lane_1_vol, mydata$lane_2_vol, mydata$lane_3_vol)

#############################
# Configure time field
mydata$time_pst = with_tz(mydata$starttime, tzone = "America/Los_Angeles") # Shift UTC to PST
mydata$day_of_wk = wday(mydata$time_pst, label = TRUE) # Make day of week variable
mydata$hour_of_day = hour(mydata$time_pst) # Make hour of day variable

#############################
# Filter for day of week of interest
# mydata.day = mydata %>%
#   filter(day_of_wk == "Sun")

mydata.day = mydata %>%
  filter(day_of_wk == "Fri")
#############################
# Aggregate data by hour
mydata.summary = mydata.day %>%
  group_by(hour_of_day) %>%
  summarize(mean(avg_spd),
            mean(avg_vol))

#############################
# Export aggregated data
write_csv(mydata.summary,
          path = "C://temp//summary.csv")
