# Chloe Fugle (chloe.m.fugle.23@dartmouth.edu)
# Shoals Marine Laboratory Internship, 7/20/2022
# Get distribution of number of nests per neighborhood and the relationship
# of nest number and productivity of the neighborhood

# replace this with the path to your data on your computer
# note: if you copy-paste the path on a Windows computer, it will have "\" in 
#       the path - please replace all "\" with "/" for R to interpret it correctly
PRODUCTIVITY_DATA = "C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx"

# import packages
library(readxl)
library(dplyr)
library(ggplot2)

################################################################################
# import data from "ROST productivity raw data 2016-2021.xlsx"
prod_2017 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2017")
prod_2018 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2018")
prod_2019 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2019")
prod_2020 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2020")
prod_2021 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2021")

