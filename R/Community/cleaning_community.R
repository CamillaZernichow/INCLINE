#### PLANT COMMUNITY DATA CLEANING IN INCLINE ####

#### Loading libraries ####
library(osfr)
library(pipebind)
library(tidyverse)
library(lubridate)
library(turfmapper)
library(dataDownloader)

#osf_auth(token = "Your personal OSF token")

#Communitydata
get_file(node = "zhk3m",
         file = "INCLINE_community_2018_2019_2021_2022_korrekturlest.csv",
         path = "data",
         remote_path = "RawData/Community")

#Metadata
get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")


#### Reading in data ####

#Sibbaldia procumbens
community_data <- read.csv("data\\INCLINE_community_2018_2019_2021_2022_korrekturlest.csv",
                               sep = ";")
meta_data <- read.csv("data\\INCLINE_metadata.csv", sep = ";")


#### Fixing mistakes in the dataset ####

##### Cleaning variables in dataset #####
# By renaming columns after the same standard, it is easier to use them, and we therefor don't need to think about their unique names to use them.

#community data
community_data <- community_data |>
  rename(Cer_sag_cf = Cer.sag_cf, Cer_sp = Cer._sp, Fes_sp = Fes.sp., Nid_seedling = Nid.seedling, block = Block, measure = Measure, site = Site, treatment = Treatment)|> #Changed wrong types and capital letters to small letters. 
  mutate(across(Ach_mil:Nid_seedling, .fns = as.character))|>
  mutate(plotID = paste0(substr(site, 1,3), "_", block, "_", plot))|>
  select(-treatment,-X)

#meta data
meta_data <- meta_data |>
  select(plotID, OTC, treatment) #selecting relevant variables

##### Combining community data and meta data #####
# The first year the data was collected, we didn't register the treatment. Therefor by combining the community data and meta data, we will get the missing information. We are also putting in plotID as a new variable that is easier to work with than separated block and plot information. 

community_data <- community_data |>
  left_join(meta_data, by = "plotID")


#### Turfmapper ####
# To continuing cleaning the large dataset, one of our group members have made a turfmap where we can analyse the yearly changes for each specie in each plot. All the colored subplots represent the observations, and the green color changes based on the species cover for each year. 

#Making data ready for turfmapper
community_data_longer <- community_data |>
  pivot_longer(Ach_mil:Nid_seedling)|>
  rename(species = name)

#Making new variables were different signs in the dataset gives the same value based on which categorie it goes under. 
community_data_longer <- community_data_longer |>
  mutate(
    presence = str_detect(value, "(?i)[1234odsjf]"),
    fertile = str_detect(value, "(?i)[f]"),
    dominance = str_detect(value, "(?i)[234d]"),
    juvenile = str_detect(value, "(?i)[j]"),
    seedling = str_detect(value, "(?i)[s]")
  )|>
  mutate(dominance = case_when(dominance == "TRUE" ~ value)) |>
  mutate(presence = case_when(presence == "TRUE" ~ 1))

#Making a new variable called cover
cover_column <- community_data_longer|>
  filter(measure == "cover" )|>
  select(block, plot, site, year, species, value) |>
  rename(cover = value)|>
  filter(!cover == "")|>
  filter(!cover == " ") 

#Combining the new column "cover" and the elongated community data
community_data_longer <-  community_data_longer |>
  left_join(cover_column, by = c("plot", "block", "site", "year", "species"))|>
  mutate(subPlot = case_when(year > 2021 & subPlot == 9 ~ "1",
                             TRUE ~ subPlot)) |>
  mutate(measure = case_when(year > 2021 & subPlot == 9 ~ "plot",
                             TRUE ~ measure)) |>
  filter(subPlot != "cover") |>
  filter(subPlot != "plot")|>
  mutate(cover = as.integer(cover))

#Plotting the turfmapps
grid <- make_grid(ncol = 7, nrow = 5 ) #Making the grid

community_data_longer |>
  filter(plotID == "Skj_4_1")|> #Put in the plotID of the plot you want to investigate.
  mutate(subPlot = as.integer(subPlot)) |>
  filter(presence != "0")|> 
  bind(x,
       make_turf_plot(
         data = x,
         year = year, species = species, cover = cover, subturf = subPlot,
         title = glue::glue("{x$plotID} Treatment: {x$OTC} {x$treatment}"), 
         grid_long = grid
       ))
