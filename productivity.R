# Chloe Fugle
# Shoals Marine Laboratory Internship, 7/3/2022
# Get productivity distribution of Roseate Terns from 2017-2021

PRODUCTIVITY_DATA = "C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx"

# import packages
library(readxl)
library(dplyr)

# import data from "ROST productivity raw data 2016-2021.xlsx"
prod_2017 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2017")
prod_2018 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2018")
prod_2019 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2019")
prod_2020 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2020")
prod_2021 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2021")

# get neighborhood and fledged/dead data
# note: code is specific to each year because the data is recorded differently
fledged_2017 = prod_2017[c("Plot/Area...1", "Notes", "Final Disposition")]
fledged_2017['Year'] = 2017
fledged_2017 <- fledged_2017 %>% relocate("Year")
colnames(fledged_2017) = c("Year", "Neighborhood", "Notes", "Fledged")

fledged_2018 = prod_2018[c("Island/area")]
fledged_2018['Year'] = 2018
fledged_2018 <- fledged_2018 %>% relocate("Year")
fledged_2018['Notes'] = NA
fledged_2018['Fledged'] = prod_2018[,23]
colnames(fledged_2018) = c("Year", "Neighborhood", "Notes", "Fledged")

fledged_2019 = prod_2019[c("Plot/Area")]
fledged_2019['Year'] = 2019
fledged_2019 <- fledged_2019 %>% relocate("Year")
fledged_2019['Notes'] = NA
fledged_2019['Fledged'] = prod_2019[,26]
colnames(fledged_2019) = c("Year", "Neighborhood", "Notes", "Fledged")

fledged_2020 = prod_2020[c("Plot/Area", "Notes")]
fledged_2020['Year'] = 2020
fledged_2020 <- fledged_2020 %>% relocate("Year")
fledged_2020['Fledged'] = prod_2020[,24]
colnames(fledged_2020) = c("Year", "Neighborhood", "Notes", "Fledged")

fledged_2021 = prod_2021[c("Plot/Area")]
fledged_2021['Year'] = 2021
fledged_2021 <- fledged_2021 %>% relocate("Year")
fledged_2021['Notes'] = NA
fledged_2021['Fledged'] = prod_2021[,23]
colnames(fledged_2021) = c("Year", "Neighborhood", "Notes", "Fledged")

# combine year dataframes into one
fledged_all <- rbind(fledged_2017, fledged_2018, fledged_2019, fledged_2020, fledged_2021)
