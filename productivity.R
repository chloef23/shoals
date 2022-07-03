# Chloe Fugle
# Shoals Marine Laboratory Internship, 7/3/2022
# Get productivity distribution of Roseate Terns from 2017-2021

# import packages
library(readxl)
library(dplyr)

# import data from "ROST productivity raw data 2016-2021.xlsx"
prod_2017 <- read_excel("C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx",
                        sheet="Productivity 2017")
prod_2018 <- read_excel("C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx",
                        sheet="Productivity 2018")
prod_2019 <- read_excel("C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx",
                        sheet="Productivity 2019")
prod_2020 <- read_excel("C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx",
                        sheet="Productivity 2020")
prod_2021 <- read_excel("C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx",
                        sheet="Productivity 2021")

# get neighborhood and fledged/dead data
fledged_2017 = prod_2017[c("Plot/Area...1", "Notes", "Final Disposition")]
fledged_2017['Year'] = 2017
fledged_2017 %>% relocate("Year")
