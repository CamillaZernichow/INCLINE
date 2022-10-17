#### PLANT COMMUNITY DATA CLEANING IN INCLINE ####

#### Loading libraries ####
library(tidyverse)
library(dataDownloader)
library(lubridate)
library(osfr)

#osf_auth(token = "personal excess token")

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


#### Cleaning data ####


