# Chloe Fugle (chloe.m.fugle.23@dartmouth.edu)
# Shoals Marine Laboratory Internship, 7/18/2022
# Examine the effect of weather data on Roseate Tern fledging success

# replace these with the path to your data on your computer
# note: if you copy-paste the path on a Windows computer, it will have "\" in 
#       the path - please replace all "\" with "/" for R to interpret it correctly
TEMPERATURE_DATA = "C:/Users/sapph/OneDrive/Documents/2021-2022/Shoals/NARACOOS Weather Data.xlsx"

# import packages
library(readxl)
library(dplyr)
library(ggplot2)

################################################################################
# get number of heatwaves occuring in each year
# heatwaves are defined as 3 or more successive days where the high and low temperature
# exceeds the 95% of high/low temperatures between 2017-2021

# import data from "NARACOOS Weather Data.xlsx"
temp_data <- read_excel(TEMPERATURE_DATA)
colnames(temp_data) = c("Date", "Year", "Month", "Day", "High", "Low")

# make histogram of high and low temperatures for all years
temp_data$High = as.numeric(temp_data$High)
temp_data$Low = as.numeric(temp_data$Low)
high_hist = hist(temp_data$High)
low_hist = hist(temp_data$Low)

# get 5th and 95th percentiles of high and low temperature data
high_percentiles = quantile(temp_data$High, probs=c(0.1, 0.9), na.rm=TRUE)
low_percentiles = quantile(temp_data$Low, probs=c(0.1, 0.9), na.rm=TRUE)

# get days of each year below 10th percentile high and low temps and above 90th
# percentile high and low temps
extreme_heat = data.frame()
extreme_cold = data.frame()
for(i in 2017:2021){
  temp_data_seg = temp_data[temp_data$Year == i,]
  temp_data_seg = temp_data_seg[complete.cases(temp_data_seg),]
  heat_data_ext = temp_data_seg[temp_data_seg$High >= high_percentiles[2] &
                                  temp_data_seg$Low >= low_percentiles[2],]
  cold_data_ext = temp_data_seg[temp_data_seg$High <= high_percentiles[1] &
                                  temp_data_seg$Low <= low_percentiles[1],]
  extreme_heat = rbind(extreme_heat, heat_data_ext)
  extreme_cold = rbind(extreme_cold, cold_data_ext)
}
