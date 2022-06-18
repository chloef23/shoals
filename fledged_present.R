# Chloe Fugle (chloe.m.fugle.23@dartmouth.edu), 6/18/22
# Shoals Marine Laboratory internship
# purpose:
# all data from Gulls of Appledore Database, primary contact:
#          Mary Elizabeth Everett (MElizabethEverett@gmail.com)

# import packages
library(readxl)
library(dplyr)

# read data from excel spreadsheet into dataframe
# excel spreadsheet contains gull data sorted for
#       field readable tag (male or female), number fledged,
#       latitude, and longitude is not null
df <- read_excel("~/2021-2022/Shoals/Data/field_readable.xlsx")
View(df)

# sort dataframe by species
herg = df %>% filter(grepl('HERG', species))
gbbg = df %>% filter(grepl('GBBG', species))
