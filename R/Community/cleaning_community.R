
##################################################
#### PLANT COMMUNITY DATA CLEANING IN INCLINE ####
##################################################

#### Loading libraries ####
library(tidyverse)
library(dataDownloader)
library(lubridate)

#Flowering data
get_file(node = "zhk3m",
         file = "INCLINE_flowering_2021.csv",
         path = "Raw_data",
         remote_path = "RawData/Flowering")

#Sibbaldia procumbens demography
get_file(node = "zhk3m",
         file = "INCLINE_demography_Sib_pro.csv",
         path = "Cleaned_demography",
         remote_path = "Demography")



#### Reading in data ####

#Sibbaldia procumbens
sib_pro_demography <- read.csv("Cleaned_demography/INCLINE_demography_Sib_pro.csv", header = TRUE, sep = ",", dec = ".") #first row is headers

#Veronica alpina
ver_alp_demography <- read.csv("Cleaned_demography/INCLINE_demography_Ver_alp.csv", header = TRUE, sep = ",", dec = ".") #first row is headers