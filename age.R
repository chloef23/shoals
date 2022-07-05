# Chloe Fugle
# Shoals Marine Laboratory Internship, 7/4/2022
# Get mean age of Roseate Terns by neighborhood

# make list of Roseate Tern field readable of chicks by year
# make list of resights and their neighborhood by year
# get age of resights based on year born
    # if not in Seavey data, look in big dataset
# get median age of adult terns by neighborhood

PRODUCTIVITY_DATA = "C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx"
RESIGHT_DATA = "C:/Users/sapph/Downloads/ROST resight raw data 2016-2022.xlsx"

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

# function to get first three car from string
extract_pfr = function(x, output){
  return(substring(x, 0, 3))
}

# for each year, get all PFRs on chicks banded that year
pfr_2017 = prod_2017$PFR[prod_2017$PFR != "-" & !is.na(prod_2017$PFR)]
pfr_2017 = lapply(pfr_2017, extract_pfr)
year = as.list(rep(2017, length(pfr_2017)))
pfr_2017 = cbind(pfr_2017, year)

pfr_2018 = prod_2018$"PFR ID"[!is.na(prod_2018$"PFR ID")]
year = as.list(rep(2018, length(pfr_2018)))
pfr_2018 = cbind(pfr_2018, year)

pfr_2019 = prod_2019$"PFR ID"[!is.na(prod_2019$"PFR ID")]
year = as.list(rep(2019, length(pfr_2019)))
pfr_2019 = cbind(pfr_2019, year)

pfr_2020 = prod_2020$"PFR ID"[!is.na(prod_2020$"PFR ID")]
pfr_2020 = lapply(pfr_2020, extract_pfr)
year = as.list(rep(2020, length(pfr_2020)))
pfr_2020 = cbind(pfr_2020, year)

pfr_2021 = prod_2021$"PFR ID"[!is.na(prod_2021$"PFR ID")]
pfr_2021 = lapply(pfr_2021, extract_pfr)
year = as.list(rep(2021, length(pfr_2021)))
pfr_2021 = cbind(pfr_2021, year)

# combine all pfr ids into one data frame
banded_pfr_all = as.data.frame(rbind(pfr_2017, pfr_2018, pfr_2019, pfr_2020, pfr_2021))
colnames(banded_pfr_all) = c("PFR", "Year")
banded_pfr_all <- banded_pfr_all %>% relocate("Year")

################################################################################
# make list of resights and their neighborhood by year