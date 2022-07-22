# Chloe Fugle (chloe.m.fugle.23@dartmouth.edu)
# Shoals Marine Laboratory Internship, 7/4/2022
# Get mean age of Roseate Terns by neighborhood

# replace these with the path to your data on your computer
# note: if you copy-paste the path on a Windows computer, it will have "\" in 
#       the path - please replace all "\" with "/" for R to interpret it correctly
PRODUCTIVITY_DATA = "C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx"
RESIGHT_DATA = "C:/Users/sapph/Downloads/ROST resight raw data 2016-2022.xlsx"
ALL_BANDING_DATA = "C:/Users/sapph/Downloads/ROST all PFR and MFR records from BBL proofed 6-21-21 out for review.xlsx"

# path for excel file of all White and Seavey Island resights and their age and birth location
EXPORT_EXCEL = "C:/Users/sapph/OneDrive/Documents/R/Shoals//ROST_Resight_Data.xlsx"

# import packages
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

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
# note: code is different for each year because data is in different format
# note: chicks are always banded with PFRs, MFRs are not relevant here
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
resi_2022 <- read_excel(RESIGHT_DATA, sheet="Resights 2022")

# for each year, extract all resight PFRs and the neighborhood they were sighted at
# note code is different for each year because data is in different format
resipfr_2017 = resi_2017[c("Specific Location", "Aux Color", "Aux type", "Aux code")]
resipfr_2017$Year = 2017

resipfr_2018 = resi_2018[c("Specific Location", "Aux Color", "Aux type", "Aux code")]
resipfr_2018$Year = 2018

resipfr_2019 = resi_2019[c("Specific Location", "Aux Color", "Aux type", "Aux code")]
resipfr_2019$Year = 2019

resipfr_2020 = resi_2020[c("Specific Location", "Aux Color", "Aux type", "Aux code")]
resipfr_2020$Year = 2020

resipfr_2021 = resi_2021[c("Specific Location", "Aux Color", "Aux type", "Aux code")]
resipfr_2021$Year = 2021

resipfr_2022 = resi_2022[c("Specific Location", "Aux Color", "Aux type", "Aux code")]
resipfr_2022$Year = 2022

# combine all pfr/mfr ids into one data frame
resipfr_all = as.data.frame(rbind(resipfr_2017, resipfr_2018, resipfr_2019, 
                                  resipfr_2020, resipfr_2021, resipfr_2022))
colnames(resipfr_all) = c("Neighborhood", "Color", "PFR or MFR", "Code", "Year")
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

# remove all rows with uncertain PFR (length of PFR != 3 char)
resi_pfr_only = resipfr_all_seg[resipfr_all_seg$`PFR or MFR` == "PFR",]
resi_pfr_only <- subset(resi_pfr_only, nchar(Code) == 3)

# remove all rows with uncertain MFR (length of MFR != 4 or MFR contains "_")
resi_mfr_only = resipfr_all_seg[resipfr_all_seg$`PFR or MFR` == "MFR",]
resi_mfr_only <- subset(resi_mfr_only, nchar(Code) == 4)
resi_mfr_only <- resi_mfr_only[!grepl("_", resi_mfr_only$Code),]

# combine PFR and MFR back into one dataframe
resipfr_all_seg = as.data.frame(rbind(resi_mfr_only, resi_pfr_only))

# lowercase band color
resipfr_all_seg$Color <- lapply(resipfr_all_seg$Color, tolower)

################################################################################
# get age of resights based on year born (if born locally)
# note: not looking for MFRs because chicks banded since 2017 only have PFRs
resipfr_all_seg$Birth_Year = NA
resipfr_all_seg$Natal_Neighborhood = NA
resipfr_all_seg$Natal_Known = FALSE
for(i in 1:length(resipfr_all_seg$Code)){  # for all resights, see if born on Seavey
  print(resipfr_all_seg$Code[i])
  present <- match(resipfr_all_seg$Code[i], banded_pfr_all_seg$PFR)
  print(present)
  if(!is.na(present)){  # if born and banded on Seavey, get neighborhood information
    resipfr_all_seg$Birth_Year[i] = banded_pfr_all_seg$`Year Born`[present]
    resipfr_all_seg$Natal_Neighborhood[i] = 
      banded_pfr_all_seg$Natal_Neighborhood_Number[present]
    resipfr_all_seg$Natal_Known[i] = TRUE
  }
}

################################################################################
# get age of resights based on year born (if born elsewhere)
# this code will take longer to execute since the data set is large

# function to get first four char from string
extract_year = function(x, output){
  return(substring(x, 0, 4))
}

# extract age, year banded, PFR and code
all_banding_data <- read_excel(ALL_BANDING_DATA)
immigrant_resi = all_banding_data[c("AGE_CODE", "Banding_location", "Banding_state",
                                    "Banding_date", "MFR or PFR","Color", "Code")]
immigrant_resi = immigrant_resi[immigrant_resi$AGE_CODE == 4,]    # hatched that year
immigrant_resi$Banding_date = lapply(immigrant_resi$Banding_date, extract_year)
immigrant_resi$Color <- lapply(immigrant_resi$Color, tolower)
immigrant_resi_seg = immigrant_resi[c("Banding_location", "Banding_state", "Banding_date",
                                      "MFR or PFR", "Color", "Code")]
colnames(immigrant_resi_seg) = c("Birth_Colony", "Birth_State", "Birth_Year","Band_Type", 
                                 "Band_Color", "Code")

# assign correct birth colony and state to local birds
resipfr_all_seg$Birth_Colony = NA
resipfr_all_seg$Birth_State = NA

resipfr_all_seg$Birth_Colony[resipfr_all_seg$Natal_Known == 'TRUE'] <- "White and Seavey Islands"
resipfr_all_seg$Birth_State[resipfr_all_seg$Natal_Known == 'TRUE'] <- "New Hampshire"

# match PFRs and MFRs to tern database for all terns not already matched
pfr_only = immigrant_resi_seg[immigrant_resi_seg$Band_Type == "PFR",]
mfr_only = immigrant_resi_seg[immigrant_resi_seg$Band_Type == "MFR",]
for(i in 1:nrow(resipfr_all_seg)){
  if(!resipfr_all_seg$Natal_Known[i]){  # if tern not already matched
    if(resipfr_all_seg$`PFR or MFR`[i] == "PFR"){
      present <- which(pfr_only$Code %in% resipfr_all_seg$Code[i])
      if(length(present) != 0){ # if bird banded and recorded
         for(j in present){ # for all indicies that match the band number, check color
           if(as.character(resipfr_all_seg$Color[i]) ==
              as.character(pfr_only$Band_Color[j])){
             resipfr_all_seg$Birth_Year[i] = pfr_only$Birth_Year[j]
             resipfr_all_seg$Birth_Colony[i] = pfr_only$Birth_Colony[j]
             resipfr_all_seg$Birth_State[i] = pfr_only$Birth_State[j]
           }
         }
      }
    }
    else if(resipfr_all_seg$`PFR or MFR`[i] == "MFR"){
      present <- which(mfr_only$Code %in% resipfr_all_seg$Code[i])
      if(length(present) == 1){ # if bird banded and recorded
        resipfr_all_seg$Birth_Year[i] = mfr_only$Birth_Year[present]
        resipfr_all_seg$Birth_Colony[i] = mfr_only$Birth_Colony[present]
        resipfr_all_seg$Birth_State[i] = mfr_only$Birth_State[present]
      }
    }
  }
}

# export dataframe of all resights and their birth year, location
library("writexl")

for_export = resipfr_all_seg[c("Year", "Neighborhood", "Color", "PFR or MFR", 
                               "Code","Birth_Year","Birth_Colony", "Birth_State")]
for_export$Color = as.character(for_export$Color)
for_export$Birth_Year = as.numeric(for_export$Birth_Year)
write_xlsx(for_export,EXPORT_EXCEL)

# clean up dataframe
resights_all = resipfr_all_seg[c("Year", "Code", "Neighborhood_Number", "Birth_Year",
                                 "Natal_Neighborhood", "Birth_Colony", "Birth_State")]
resights_all = resights_all[!is.na(resights_all$Birth_Year),]
resights_all = unique(resights_all)

################################################################################
# create graphs of data

# get local terns that did and did not return to their natal neighborhood
resights_all$Returned_Natal = NA
resights_all$Neighborhood_Number = as.numeric(resights_all$Neighborhood_Number)

for(i in 1:length(resights_all$Returned_Natal)){
  if(!is.na(resights_all$Natal_Neighborhood[i])){ # if the tern was born locally
    if(resights_all$Natal_Neighborhood[i] == resights_all$Neighborhood_Number[i]){
      resights_all$Returned_Natal[i] = TRUE   # tern returned to natal neighborhood
    }
    else{
      resights_all$Returned_Natal[i] = FALSE  # tern did not return to natal neighborhood
    }
  }
}

# get age of terns
resights_all$Birth_Year = as.numeric(resights_all$Birth_Year)
for(i in 1:nrow(resights_all)){
  resights_all$Tern_Age[i] = resights_all$Year[i] - resights_all$Birth_Year[i]
}

# create histogram of age distribution
hist(resights_all$Tern_Age)

# exclude terns that were resighted in the same year they were born
resights_all = resights_all[resights_all$Tern_Age != 0,]

# create histogram of resight distribution by neighborhood
plot(resights_all$Neighborhood_Number)

# export year, neighborhood, and ROST code for each resighted tern
for_export = resights_all[c("Year", "Code", "Neighborhood_Number")]
saveRDS(for_export, file="Resights by Neighborhood and Year")

# perform one-way ANOVA modeling tern age as a function of neighborhood
anova = aov(Tern_Age ~ Neighborhood_Number, data = resights_all)
plot(anova)   # check for homoscedasticity
tukey = TukeyHSD(anova)   # perform Tukey HSD Pst-Hoc test

# plot tern age by neighborhood
resights_all$Neighborhood_Number = as.factor(resights_all$Neighborhood_Number)
resights_all %>%                                           
  ggplot(aes(x=Neighborhood_Number,y=Tern_Age)) +            
  geom_boxplot() +                                     
  labs(x="Neigborhood",y="Tern Age")

# plot birth place of terns by neighborhood
resights_all = as.data.frame(lapply(resights_all, unlist))
resights_all$Birth_State = as.factor(resights_all$Birth_State)
resights_all$Neighborhood_Number = as.factor(resights_all$Neighborhood_Number)

resights_percent <- resights_all %>%
  count(Birth_Colony, Neighborhood_Number) %>%
  group_by(Neighborhood_Number) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  ungroup()

resights_percent %>%                                           
  ggplot(aes(x = Neighborhood_Number, y = percent, fill = Birth_Colony)) +            
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette="Set3")  +
  labs(x="Neigborhood",y="Percent of Terns")

# plot returning terns by neighborhood
# TODO: this double counts birds that return to same neighborhood every year
only_natal = resights_all[!is.na(resights_all$Natal_Neighborhood),]
only_natal$Natal_Neighborhood = as.numeric(only_natal$Natal_Neighborhood)
only_natal$Natal_Neighborhood = as.factor(only_natal$Natal_Neighborhood)
only_natal %>%                                           
  ggplot(aes(Returned_Natal)) +
  geom_bar() + 
  facet_grid(. ~Natal_Neighborhood) +                                    
  labs(x="Neigborhood",y="Number of Natal Terns Returning")
