# Chloe Fugle (chloe.m.fugle.23@dartmouth.edu)
# Shoals Marine Laboratory Internship, 7/20/2022
# Compare productivity and percent fledged as measures of ROST fledsing success

# replace this with the path to your data on your computer
# note: if you copy-paste the path on a Windows computer, it will have "\" in 
#       the path - please replace all "\" with "/" for R to interpret it correctly
PRODUCTIVITY_DATA = "C:/Users/sapph/Downloads/ROST productivity raw data 2016-2021.xlsx"

# import packages
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

################################################################################
# import data from "ROST productivity raw data 2016-2021.xlsx"
prod_2017 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2017")
prod_2018 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2018")
prod_2019 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2019")
prod_2020 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2020")
prod_2021 <- read_excel(PRODUCTIVITY_DATA, sheet="Productivity 2021")

# extract neighborhood and nest data from raw data
# note: code is specific to each year because the data is recorded differently
comp_2017 = prod_2017[c("Plot/Area...1", "Nest Number", "Final Disposition")]
comp_2017['Year'] = 2017
colnames(comp_2017) = c("Neighborhood", "Nest_Number", "Fledged", "Year")

comp_2018 = prod_2018[c("Island/area", "Nest Number")]
comp_2018['Fledged'] = prod_2018[,23]
comp_2018['Year'] = 2018
colnames(comp_2018) = c("Neighborhood", "Nest_Number", "Fledged", "Year")

comp_2019 = prod_2019[c("Plot/Area", "Nest Number")]
comp_2019['Fledged'] = prod_2019[,26]
comp_2019['Year'] = 2019
colnames(comp_2019) = c("Neighborhood", "Nest_Number", "Fledged", "Year")

comp_2020 = prod_2020[c("Plot/Area", "Nest Number")]
comp_2020['Fledged'] = prod_2020[,24]
comp_2020['Year'] = 2020
colnames(comp_2020) = c("Neighborhood", "Nest_Number", "Fledged", "Year")

comp_2021 = prod_2021[c("Plot/Area", "Nest Number")]
comp_2021['Fledged'] = prod_2021[,23]
comp_2021['Year'] = 2021
colnames(comp_2021) = c("Neighborhood", "Nest_Number", "Fledged", "Year")

########################################################################
# combine year dataframes into one and process the data

comp_all <- rbind(comp_2017, comp_2018, comp_2019, comp_2020, comp_2021)

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
neigh_num <- apply(comp_all[,1], 1, extract_nums)
neigh_num_str <- lapply(neigh_num, collapse_nums)
comp_all$Neighborhood_Number <- neigh_num_str

# remove rows with uncertain neighborhood (have multiple or no neighborhoods listed)
comp_all <- subset(comp_all, nchar(Neighborhood_Number) == 1)

# extract nest number from string
nest_num <- apply(comp_all[,2], 1, extract_nums)
nest_num_str <- lapply(nest_num, collapse_nums)
comp_all$Nest_Only_Number <- nest_num_str

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
  else{
    x_num = NA
  }
}

# assign 1 for fledged chicks, 0 for dead chicks or abandoned eggs, and NA
# for missing chicks
alive <- apply(comp_all[,3], 1, check_alive)
alive <- lapply(alive, as.numeric)
comp_all$Fledged_Bool <- alive

# clean up data frame
comp_all_seg = comp_all[c("Nest_Only_Number", "Year", "Neighborhood_Number", "Fledged_Bool")]
comp_all_seg <- comp_all_seg %>% relocate("Neighborhood_Number")
comp_all_seg <- comp_all_seg %>% relocate("Year")
colnames(comp_all_seg) = c("Year", "Neighborhood", "Nest", "Fledged")

################################################################################
# Calculate productivity (# fledged/nest)
year = vector(mode = "list", length = 0)
neighborhood = vector(mode = "list", length = 0)
productivity = vector(mode = "list", length = 0)

for(i in 2017:2021){
  for(j in 1:9){
    nests_seg = comp_all_seg[comp_all_seg$Year == i &
                               comp_all_seg$Neighborhood == j,]
    if(nrow(nests_seg) == 0){
      # add to data frame
      year = c(year, i)
      neighborhood = c(neighborhood, j)
      productivity = c(productivity, NA)
      next
    }
    nests_seg = as.data.frame(lapply(nests_seg, unlist))
    remove = nests_seg[which(is.na(nests_seg$Fledged)),"Nest"]  # list of nests that contain an NA
    if(length(remove) > 0){
      remove = as.list(remove)
      nests_seg = subset(nests_seg, !(Nest %in% remove))   # remove nests with NAs
    }
    no_nests = length(unique(nests_seg$Nest)) # get number of nests in neighborhood that year
    total_fledged = sum(nests_seg$Fledged)
    
    # add to data frame
    year = c(year, i)
    neighborhood = c(neighborhood, j)
    productivity = c(productivity, total_fledged/no_nests)
  }
}

productivity_df = as.data.frame(cbind(year, neighborhood, productivity))
productivity_df = as.data.frame(lapply(productivity_df, unlist))
colnames(productivity_df) = c("Year", "Neighborhood", "Productivity")

################################################################################
# plot productivity against percent fledged

# get percent fledged by year by neighborhood generated by productivity.R script
prod_by_neighyear = readRDS(file="Mean_Productivity_by_Year_by_Neighborhood")
productivity_df$Percent_Fledged = prod_by_neighyear

# plot percent fledged against number of nests in a neighborhood
productivity_df$Percent_Fledged = as.numeric(productivity_df$Percent_Fledged, na.rm = TRUE)
productivity_df %>%                                           
  ggplot(aes(x=Productivity,y=Percent_Fledged)) +            
  geom_point() + 
  geom_abline(slope=1,intercept=0, color="red4")  +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.05, 1.65)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.05, 1.05))  +
  labs(x="Productivity (Fledged/Nest)",y="% Fledged (No. Fledged/No. Eggs)")

################################################################################
# get productivity by year
year = vector(mode = "list", length = 0)
productivity = vector(mode = "list", length = 0)

for(i in 2017:2021){
  year_seg = comp_all_seg[comp_all_seg$Year == i,]
  if(nrow(year_seg) == 0){
    # add to data frame
    year = c(year, i)
    productivity = c(productivity, NA)
    next
  }
  year_seg = as.data.frame(lapply(year_seg, unlist))
  remove = year_seg[which(is.na(year_seg$Fledged)),"Nest"]  # list of nests that contain an NA
  if(length(remove) > 0){
    remove = as.list(remove)
    year_seg = subset(year_seg, !(Nest %in% remove))   # remove nests with NAs
  }
  no_nests = length(unique(year_seg$Nest)) # get number of nests in neighborhood that year
  total_fledged = sum(year_seg$Fledged)
  
  # add to data frame
  year = c(year, i)
  neighborhood = c(neighborhood, j)
  productivity = c(productivity, total_fledged/no_nests)
}

year_productivity_df = as.data.frame(cbind(year, productivity))
year_productivity_df = as.data.frame(lapply(year_productivity_df, unlist))
colnames(year_productivity_df) = c("Year", "Productivity")
