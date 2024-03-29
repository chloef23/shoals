# Chloe Fugle
# Shoals Marine Laboratory Internship, 7/18/2022
# Examine the effect of weather data on Roseate Tern fledging success

# replace these with the path to your data on your computer
# note: if you copy-paste the path on a Windows computer, it will have "\" in 
#       the path - please replace all "\" with "/" for R to interpret it correctly
TEMPERATURE_DATA = "NARACOOS Weather Data.xlsx"

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

# get longest consecutive hot and cold spell for each year
hot_spell_by_year = vector(mode="list", length=0)
cold_spell_by_year = vector(mode="list", length=0)
no_hot_days = vector(mode="list", length=0)
no_cold_days = vector(mode="list", length=0)

for(i in 2017:2021){
  seg_ext_heat = extreme_heat[extreme_heat$Year == i,]
  seg_ext_cold = extreme_cold[extreme_cold$Year == i,]
  max_hot_spell = 0
  max_cold_spell = 0
  
  no_hot_days = c(no_hot_days, nrow(seg_ext_heat))
  no_cold_days = c(no_cold_days, nrow(seg_ext_cold))
  
  # if number of hot days 1 or 0, that is the longest spell
  if(nrow(seg_ext_heat) < 2){
    max_hot_spell = nrow(seg_ext_heat)
  }
  # else, get number of consecutive hot days
  else{
    prev_date = c(seg_ext_heat$Month[1], seg_ext_heat$Day[1])  # remember month and day of previous line
    counter = 1
    for(j in 2:nrow(seg_ext_heat)){
      if(seg_ext_heat$Month[j] != prev_date[1]){  # month changed between rows
        prev_date = c(seg_ext_heat$Month[j], seg_ext_heat$Day[j])
        counter = 1
      }
      else if(seg_ext_heat$Day[j] == (prev_date[2] + 1)){   # next row is next day
        counter = counter + 1
        if(counter > max_hot_spell){
          max_hot_spell = counter
        }
        prev_date = c(seg_ext_heat$Month[j], seg_ext_heat$Day[j])
      }
      else{   # next row is not next day
        counter = 1
        prev_date = c(seg_ext_heat$Month[j], seg_ext_heat$Day[j])
      }
    }
  }
  # add length of heat spell for that year to list
  hot_spell_by_year = c(hot_spell_by_year, max_hot_spell)
  
  # if number of cold days 1 or 0, that is the longest spell
  if(nrow(seg_ext_cold) < 2){
    max_cold_spell = nrow(seg_ext_cold)
  }
  # else, get number of consecutive hot days
  else{
    prev_date = c(seg_ext_cold$Month[1], seg_ext_cold$Day[1])  # remember month and day of previous line
    counter = 1
    for(j in 2:nrow(seg_ext_cold)){
      if(seg_ext_cold$Month[j] != prev_date[1]){  # month changed between rows
        prev_date = c(seg_ext_cold$Month[j], seg_ext_cold$Day[j])
        counter = 1
      }
      else if(seg_ext_cold$Day[j] == (prev_date[2] + 1)){   # next row is next day
        counter = counter + 1
        if(counter > max_cold_spell){
          max_cold_spell = counter
        }
        prev_date = c(seg_ext_cold$Month[j], seg_ext_cold$Day[j])
      }
      else{   # next row is not next day
        counter = 1
        prev_date = c(seg_ext_cold$Month[j], seg_ext_cold$Day[j])
      }
    }
  }
  # add length of heat spell for that year to list
  cold_spell_by_year = c(cold_spell_by_year, max_cold_spell)
}

################################################################################
# plot extreme heat and cold against percent fledged

# get mean percent fledged by year generated by productivity.R script
prod_by_year = readRDS(file="Mean_Productivity_by_Year")

# make data frame for graph
year = c(2017, 2018, 2019, 2020, 2021)
graph_df = as.data.frame(cbind(year, hot_spell_by_year, cold_spell_by_year, 
                               no_hot_days, no_cold_days, prod_by_year))
graph_df$hot_spell_by_year = as.numeric(graph_df$hot_spell_by_year)
graph_df$cold_spell_by_year = as.numeric(graph_df$cold_spell_by_year)
graph_df$no_cold_days = as.numeric(graph_df$no_cold_days)
graph_df$no_hot_days = as.numeric(graph_df$no_hot_days)
graph_df$prod_by_year = as.numeric(graph_df$prod_by_year)

# plot length of longest hot spell against percent fledged
graph_df %>%                                           
  ggplot(aes(x=hot_spell_by_year,y=prod_by_year)) +            
  geom_point() +                                     
  labs(x="Length of Longest Hot Spell",y="% Fledged")

# plot length of longest hot spell against percent fledged
graph_df %>%                                           
  ggplot(aes(x=cold_spell_by_year,y=prod_by_year)) +            
  geom_point() +                                     
  labs(x="Length of Longest Cold Spell",y="% Fledged")

# plot number of extreme heat days against percent fledged
graph_df %>%                                           
  ggplot(aes(x=no_hot_days,y=prod_by_year)) +            
  geom_point() +                                     
  labs(x="Number of Extreme Heat Days",y="% Fledged")

# plot number of extreme cold days against percent fledged
graph_df %>%                                           
  ggplot(aes(x=no_cold_days,y=prod_by_year)) +            
  geom_point() +                                     
  labs(x="Number of Extreme Cold Days",y="% Fledged")