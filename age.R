# Chloe Fugle
# Shoals Marine Laboratory Internship, 7/4/2022
# Get mean age of Roseate Terns by neighborhood

# make list of Roseate Tern field readable of chicks by year
# make list of resights and their neighborhood by year
# get age of resights based on year born
    # if not in Seavey data, look in big dataset
# get median age of adult terns by neighborhood

# replace these with the path to your data on your computer
# note: if you copy-paste the path on a Windows computer, it will have "\" in 
#       the path - please replace all "\" with "/" for R to interpret it correctly
PRODUCTIVITY_DATA = "C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx"
RESIGHT_DATA = "C:/Users/sapph/Downloads/ROST resight raw data 2016-2022.xlsx"
ALL_BANDING_DATA = "C:/Users/sapph/Downloads/ROST all PFR and MFR records from BBL proofed 6-21-21 out for review.xlsx"

# import packages
library(readxl)
library(dplyr)
library(ggplot2)

################################################################################
# make list of Roseate Tern field readable bands of chicks by year

# import data from "ROST productivity raw data 2016-2021.xlsx"
prod_2017 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2017")
prod_2018 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2018")
prod_2019 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2019")
prod_2020 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2020")
prod_2021 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2021")

# function to get first three char from string
extract_pfr = function(x, output){
  return(substring(x, 0, 3))
}

# for each year, extract all PFRs on chicks banded that year
# note code is different for each year because data is in different format
pfr_2017 = prod_2017[c("Plot/Area...1", "PFR")]
pfr_2017 = pfr_2017[pfr_2017$PFR != "-" & !is.na(pfr_2017$PFR),]
pfr_2017$PFR = lapply(pfr_2017$PFR, extract_pfr)
pfr_2017$Year = 2017
colnames(pfr_2017) = c("Neighborhood", "PFR", "Year")

pfr_2018 = prod_2018[c("Island/area", "PFR ID")]
pfr_2018 = pfr_2018[!is.na(pfr_2018$"PFR ID"),]
pfr_2018$"PFR ID" = lapply(pfr_2018$"PFR ID", extract_pfr)
pfr_2018$Year = 2018
colnames(pfr_2018) = c("Neighborhood", "PFR", "Year")

pfr_2019 = prod_2019[c("Plot/Area", "PFR ID")]
pfr_2019 = pfr_2019[!is.na(pfr_2019$"PFR ID"),]
pfr_2019$"PFR ID" = lapply(pfr_2019$"PFR ID", extract_pfr)
pfr_2019$Year = 2019
colnames(pfr_2019) = c("Neighborhood", "PFR", "Year")

pfr_2020 = prod_2020[c("Plot/Area", "PFR ID")]
pfr_2020 = pfr_2020[!is.na(pfr_2020$"PFR ID"),]
pfr_2020$"PFR ID" = lapply(pfr_2020$"PFR ID", extract_pfr)
pfr_2020$Year = 2020
colnames(pfr_2020) = c("Neighborhood", "PFR", "Year")

pfr_2021 = prod_2021[c("Plot/Area", "PFR ID")]
pfr_2021 = pfr_2021[!is.na(pfr_2021$"PFR ID"),]
pfr_2021$"PFR ID" = lapply(pfr_2021$"PFR ID", extract_pfr)
pfr_2021$Year = 2021
colnames(pfr_2021) = c("Neighborhood", "PFR", "Year")

# combine all pfr ids into one data frame
banded_pfr_all = as.data.frame(rbind(pfr_2017, pfr_2018, pfr_2019, pfr_2020, pfr_2021))
colnames(banded_pfr_all) = c("Natal Neighborhood", "PFR", "Year Born")
banded_pfr_all <- banded_pfr_all %>% relocate("Year Born")

# function to extract numbers from string
# input: x - string
# output: x_numbers - list of the single-digit integers contained in the string
extract_nums = function(x, output){
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))
  return(x_numbers)
}

# function to collapse list of integers into string
# input: x - list of numbers
# output: x_string - string of numbers
collapse_nums = function(x, output){
  x_string = paste(unlist(x),collapse="")
  return(x_string)
}

# extract neighborhood number from string
neigh_num <- lapply(banded_pfr_all[,2], extract_nums)
neigh_num_str <- lapply(neigh_num, collapse_nums)
banded_pfr_all$Natal_Neighborhood_Number <- neigh_num_str

# remove rows with uncertain neighborhood (have multiple or no neighborhoods listed)
banded_pfr_all_seg <- subset(banded_pfr_all, nchar(Natal_Neighborhood_Number) == 1)

################################################################################
# make list of resights and their neighborhood by year

# import data from "ROST resight raw data 2016-2022.xlsx"
resi_2017 <- read_excel(RESIGHT_DATA, sheet="Resights 2017")
resi_2018 <- read_excel(RESIGHT_DATA, sheet="Resights 2018")
resi_2019 <- read_excel(RESIGHT_DATA, sheet="Resights 2019")
resi_2020 <- read_excel(RESIGHT_DATA, sheet="Resights 2020")
resi_2021 <- read_excel(RESIGHT_DATA, sheet="Resights 2021")

# for each year, extract all resight PFRs and the neighborhood they were sighted at
# note code is different for each year because data is in different format
resipfr_2017 = resi_2017[resi_2017$"Aux type" == "PFR",]
resipfr_2017 = resipfr_2017[c("Specific Location", "Aux code")]
resipfr_2017$Year = 2017

resipfr_2018 = resi_2018[resi_2018$"Aux type" == "PFR",]
resipfr_2018 = resipfr_2018[c("Specific Location", "Aux code")]
resipfr_2018$Year = 2018

resipfr_2019 = resi_2019[resi_2019$"Aux type" == "PFR",]
resipfr_2019 = resipfr_2019[c("Specific Location", "Aux code")]
resipfr_2019$Year = 2019

resipfr_2020 = resi_2020[resi_2020$"Aux type" == "PFR",]
resipfr_2020 = resipfr_2020[c("Specific Location", "Aux code")]
resipfr_2020$Year = 2020

resipfr_2021 = resi_2021[resi_2021$"Aux type" == "PFR",]
resipfr_2021 = resipfr_2021[c("Specific Location", "Aux code")]
resipfr_2021$Year = 2021

resipfr_2022 = resi_2022[resi_2022$"Aux type" == "PFR",]
resipfr_2022 = resipfr_2022[c("Specific Location", "Aux code")]
resipfr_2022$Year = 2022

# combine all pfr ids into one data frame
resipfr_all = as.data.frame(rbind(resipfr_2017, resipfr_2018, resipfr_2019, 
                                  resipfr_2020, resipfr_2021, resipfr_2022))
colnames(resipfr_all) = c("Neighborhood", "PFR", "Year")
resipfr_all <- resipfr_all %>% relocate("Year")

# function to extract numbers from string
# input: x - string
# output: x_numbers - list of the single-digit integers contained in the string
extract_nums = function(x, output){
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))
  return(x_numbers)
}

# function to collapse list of integers into string
# input: x - list of numbers
# output: x_string - string of numbers
collapse_nums = function(x, output){
  x_string = paste(unlist(x),collapse="")
  return(x_string)
}

# extract neighborhood number from string
neigh_num <- lapply(resipfr_all[,2], extract_nums)
neigh_num_str <- lapply(neigh_num, collapse_nums)
resipfr_all$Neighborhood_Number <- neigh_num_str

# remove rows with uncertain neighborhood (have multiple or no neighborhoods listed)
resipfr_all_seg <- subset(resipfr_all, nchar(Neighborhood_Number) == 1)

################################################################################
# get age of resights based on year born (if born locally)
resipfr_all_seg$Birth_Year = NA
resipfr_all_seg$Natal_Neighborhood = NA
resipfr_all_seg$Born_Locally = FALSE
for(i in 1:length(resipfr_all_seg$PFR)){
  present <- match(resipfr_all_seg$PFR[i], banded_pfr_all_seg$PFR)
  if(!is.na(present)){
    resipfr_all_seg$Birth_Year[i] = banded_pfr_all_seg$`Year Born`[present]
    resipfr_all_seg$Natal_Neighborhood[i] = 
      banded_pfr_all_seg$Natal_Neighborhood_Number[present]
    resipfr_all_seg$Born_Locally[i] = TRUE
  }
}

################################################################################
# get age of resights based on year born (if born elsewhere)
# this code will take longer to execute since the data set is large

# extract age, year banded, PFR and code
all_banding_data <- read_excel(ALL_BANDING_DATA)
immigrant_resi = all_banding_data[c("AGE_CODE", "Banding_year", "MFR or PFR", "Code")]
