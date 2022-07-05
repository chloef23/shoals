# Chloe Fugle
# Shoals Marine Laboratory Internship, 7/3/2022
# Get productivity distribution of Roseate Terns from 2017-2021

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

# extract neighborhood and fledged/dead data from raw data
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

########################################################################
# combine year dataframes into one and process the data
fledged_all <- rbind(fledged_2017, fledged_2018, fledged_2019, fledged_2020, fledged_2021)

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
neigh_num <- apply(fledged_all[,2], 1, extract_nums)
neigh_num_str <- lapply(neigh_num, collapse_nums)
fledged_all$Neighborhood_Number <- neigh_num_str

# remove rows with uncertain neighborhood (have multiple neighborhoods listed)
fledged_all_seg <- subset(fledged_all, nchar(Neighborhood_Number) == 1)

# remove any rows where Notes say "exclude from productivity"
fledged_all_seg <- subset(fledged_all_seg, Notes != "exclude from productivity"
                          | is.na(Notes))

# function that returns 0 if the string matches "dead" or "abandoned", NA if
#   string matches "missing", and 1 if string matches "fledged" and the first two
#   conditions aren't met
# input: x - string
# output: x_num - value is 0, 1, or NA
check_alive = function(x, output){
  x_num = NA
  if(missing(x) | is.na(x) | x == ""){
    return(NA)
  }
  
  x <- tolower(x)
  if(grepl("dead", x) | grepl("abandoned", x) | grepl("predated", x) |
     grepl("abandonded", x)){
    x_num = 0
  }
  else if(grepl("missing", x)){
    x_num = NA
  }
  else if(grepl("fledged", x)){
    x_num = 1
  }
}

# assign 1 for fledged chicks, 0 for dead chicks or abandoned eggs, and NA
# for missing chicks
alive <- apply(fledged_all_seg[,4], 1, check_alive)
alive <- lapply(alive, as.numeric)
fledged_all_seg$Fledged_Bool <- alive

################################################################################
# make scatter plot of mean productivity by neighborhood by year

# get mean productivity by year
year_means_list = list()
for(i in 2017:2021){
  # get mean of Fledged_Bool for all rows where Year == i
  temp_mean = mean(as.numeric(fledged_all_seg$Fledged_Bool
                              [fledged_all_seg$Year == i]), na.rm = TRUE)
  year_means_list = c(year_means_list, temp_mean)
}

# get mean productivity by neighborhood
neigh_means_list = list()
for(i in 1:9){
  # get mean of Fledged_Bool for all rows where Neighborhood == i
  temp_mean = mean(as.numeric(fledged_all_seg$Fledged_Bool
                              [fledged_all_seg$Neighborhood_Number == i]), na.rm = TRUE)
  neigh_means_list = c(neigh_means_list, temp_mean)
}

# get mean productivity by neighborhood by year
neigh_year_means_list = list()
year_list = list()
neigh_list = list()
for(i in 2017:2021){
  for(j in 1:9){
    temp_year = 
    temp_mean = mean(as.numeric(fledged_all_seg$Fledged_Bool
                                [fledged_all_seg$Year == i & 
                                    fledged_all_seg$Neighborhood_Number == j]), na.rm = TRUE)
    neigh_year_means_list = c(neigh_year_means_list, temp_mean)
    year_list = c(year_list, i)
    neigh_list = c(neigh_list, j)
  }
}

# combine year, neighborhood, means to form data frame
graph_df = as.data.frame(cbind(Year = year_list, Neighborhood = neigh_list, 
                               Mean = neigh_year_means_list))
graph_df$Neighborhood <- as.numeric(graph_df$Neighborhood)
graph_df$Mean <- as.numeric(graph_df$Mean)
graph_df %>%                                           
  ggplot(aes(x=Neighborhood,y=Mean, color=Year)) +            
  geom_point() +                                     
  labs(x="Neigborhood",y="% Fledged",
       fill="Year")
