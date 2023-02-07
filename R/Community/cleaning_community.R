#### PLANT COMMUNITY DATA CLEANING IN INCLINE ####

##### Loading libraries #####
library(osfr)
library(pipebind)
library(tidyverse)
library(lubridate)
library(turfmapper)
library(dataDownloader)
library(dplyr)
library("vegan")

#osf_auth(token = "")#Your personal OSF token

#Community_data
get_file(node = "zhk3m",
         file = "INCLINE_community_2018_2019_2021_2022_korrekturlest.csv",
         path = "data",
         remote_path = "RawData/Community")

#Meta_data
get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")


##### Reading in data #####
#Community data
community_data_download <- read_delim("data\\INCLINE_community_2018_2019_2021_2022_korrekturlest.csv", col_types = cols(.default = col_character()))
#Meta data
meta_data_download <- read_delim("data\\INCLINE_metadata.csv")


#### Fixing mistakes in the dataset ####
##### Cleaning variables in dataset #####
#By renaming columns after the same standard, it is easier to use them, and we therefor don't need to think about their unique names when using them.

#community data
community_data <- community_data_download |>
  rename(Cer_sag_cf = "Cer/sag_cf", Cer_sp = "Cer _sp", Fes_sp = Fes.sp., Vac_myr_cf = Var_myr_cf, Nid_seedling = "Nid seedling", block = Block, measure = Measure, site = Site, treatment = Treatment, weather = Weather)|> #Changed wrong types and capital letters to small letters. 
  mutate(plotID = paste0(substr(site, 1,3), "_", block, "_", plot))|>#Making a new column called plotID
  select(-treatment,-...226)#Removing unnecessary columns

#meta data
meta_data <- meta_data_download|>
  select(plotID, OTC, treatment) #selecting relevant variables from the meta data

##### Combining community data and meta data #####
# The first year the data was collected, we didn't register the treatment. Therefor by combining the community data and meta data, we will get the missing information. We are also putting in plotID as a new variable that is easier to work with than separated block and plot information. 

community_data <- community_data |>
  left_join(meta_data, by = "plotID")


##### Turfmapper #####
# To continuing cleaning the large dataset, one of our group members have made a turfmap where we can analyse the yearly changes for each specie in each plot. All the colored subplots represent the observations, and the green color changes based on the species cover for each year. The turfmapper plotting code can be found in the community_turfmapper.R script

#Making data ready for turfmapper by widening the data
community_data_longer <- community_data |>
  pivot_longer(Ach_mil:Nid_seedling,values_drop_na = TRUE)|>
  rename(species = name)

#Making new variables were different signs in the dataset gives the same value based on which category it goes under. 
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
  filter(!cover == " ")|>
  mutate(cover = ifelse(cover == 0.1, 1, cover))|>
  mutate(cover = ifelse(cover == 0.5, 1, cover))|>
  mutate(cover = ifelse(cover == 0 & species == "Hyp_mac" & site == "Ulvehaugen" & year == 2019 & block == 7 & plot == 3, 1, cover))

#Combining the new column "cover" and the elongated community data
community_data_longer <-  community_data_longer |>
  left_join(cover_column, by = c("plot", "block", "site", "year", "species"))|>
  mutate(subPlot = case_when(year == 2022 & subPlot == 9 ~ "whole_plot",
                             TRUE ~ subPlot)) |> #For using in turmapper, "whole_plot" needs to be a number from 1-35. Easiest when using 1.
  mutate(measure = case_when(year == 2022 & subPlot == 9 ~ "plot",
                             TRUE ~ measure)) |>
  filter(subPlot != "cover")|>
  filter(subPlot != "plot")|>
  mutate(cover = as.integer(cover))

#Removing unnecessary tables
rm(cover_column)



#### Cleaning mistakes from dataset ####
#Several typing mistakes occurred when making the dataset. Therefor, we standardise again the rest of typing mistakes for the different species so its easier to work on when cleaning the data.
#General coding for all plots
community_clean <- community_data_longer |>
  mutate(species = ifelse(species == "Tri_eur", "Lys_eur", species))|>
  mutate(species = ifelse (species == "Antennaria_sp", "Ant_sp", species))|>
  mutate(species = ifelse (species == "Epilobium_sp", "Epi_sp", species))|>
  mutate(species = ifelse (species == "Juncus_sp", "Jun_sp", species))|>
  mutate(species = ifelse (species == "Ranunculus", "Ran_sp", species))|>
  mutate(species = ifelse(species %in% c("Eup_sp", "Eup_str"), "Eup_wet", species))|>
  mutate(species = ifelse (species == "Vio_riv", "Vio_can", species))|>
  mutate(species = ifelse(species == "Emp_her", "Emp_nig", species))|>
  mutate(species = ifelse(species == "Hup_sel", "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Gen_ana", "Gen_ama", species))|>
  mutate(species = ifelse(species == "Lyc_lyc", "Sel_sel", species))
#Renamed Tri_eur to Lys_eur, Antenoria_sp to Ant_sp, Eup_sp and Eup_str to Eup_wet, and Vio_riv to Vio Can, Emp_her to Emp_nig, Hup_sel to Hyp_sel, Gen_ana to Gen_ama. Also shortened the long species names to the same standard as the other.

#Codes for each specific change in each plot#
#Using two different codes with different variants of the two codes:

# - mutate(species = ifelse(species == "the variable" & plotID == "the name of the plot", "the new name", species)) #The main code that rename and merges the information from two columns together in one. 

# - mutate(cover = ifelse(species == "the variable" & plotID == "the name of the plot" & year == the year(s) you want the cover to apply for, the new cover, cover)) #You can also change "species" before "ifelse", and after "the new name" to change the cover for the plot. Here you need "year ==" to specify which year you want to change, if not, all covers for all year for the specific specie will be the same.

# - By putting in c("variable1", "variable2"...) beside "species ==", you can merge several columns together. You can also do the same for years, if there is several years you want the change to apply for. 

# - You can also put in "year ==" in when merging columns. Good to use when you have unknown species of cf that you want to split.

#####Skjellingahaugen#####

community_clean <- community_clean |>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_1_1" & year %in% c(2018, 2021), "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_1" & year == 2018, 4, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_1" & year == 2021,5, cover ))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Skj_1_1" & year %in% c(2021, 2022), "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_1_1" & year == 2021, 2, cover), cover = ifelse(species == "Car_big" & plotID == "Skj_1_1" & year == 2022,6, cover ))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_1_1", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Skj_1_1", "Pin_vul", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_1_1", "Ran_acr", species)) |>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Skj_1_1", "Sib_pro", species))|>
  mutate(cover = ifelse(species == "Sib_pro" & plotID == "Skj_1_1" & year == 2018, 4, cover)) |> #Sjekk turfmapper
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_nor") & plotID == "Skj_1_3" & year == 2018, "Car_big", species)) |> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_1_3" & year == 2018, 3, cover))|>
  mutate(species = ifelse(species %in% c("Agr_mer","Phl_alp") & plotID == "Skj_1_3", "Agr_cap", species))|>
  mutate(cover = ifelse(species =="Agr_cap" & plotID == "Skj_1_3" & year == 2022, 5, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_3" & year == 2018, 4, cover)) |>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_3" & year == 2019, 3, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_3" & year == 2021,6,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf","Agr_mer") & plotID == "Skj_1_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_4" & year == 2021, 25, cover))|>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_1_4", "Car_fla", species)) |> 
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_1_4" & year == 2021, "Leo_aut", species)) |>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_1_4" & year == 2022, "Fes_rub", species))|>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_1_5", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_5" & year == 2018, 12, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_5" & year == 2021, 13, cover))|>
  mutate(species = ifelse(species == "Pot_ere" & plotID == "Skj_1_5" & year == 2022, "Pot_cra", species))|>
  mutate(species = ifelse(species == "Car_atr" & plotID == "Skj_2_1" & year == 2018, "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_2_1" & year == 2018, 7, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_2_1" & year == 2018, "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_2_1" & year == 2018, 1, cover))|> #Was a proposal on if the Tar_sp is Hie_pil, however, non Hie_pil have been found earlier.
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_2_2", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_2_2" & year == 2021, 5, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Skj_2_2" & year == 2018, "Car_big", species)) |>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_2_2" & year == 2018, 6, cover))|>
  mutate(species = ifelse(species == "Gen_sp" & plotID == "Skj_2_2" & year == 2018, "Gen_niv", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_2_2" & year == 2022, "Ran_acr", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_2_3", "Epi_ana", species))|> 
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_2_3" & year == 2018,1,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_2_5" & year == 2022, "Alc_sp", species)) |>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_2_5" & year == 2018, "Cer_cer", species)) |>
  mutate(species = ifelse(species %in% c("Epi_ana_cf", "Epilobium_sp") & plotID == "Skj_2_5" & year %in% c(2018,2022), "Epi_ana", species)) |>
  mutate(species = ifelse(species == "Equ_sp" & plotID == "Skj_2_5" & year %in% c(2019, 2021), "Equ_arv", species)) |> 
  mutate(cover = ifelse(species == "Equ_arv" & plotID == "Skj_2_5" & year == 2021, 3, cover)) |>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Skj_2_6" & year == 2021, "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_2_6" & year == 2021, 3, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_2_6" & year == 2022, "Alc_sp", species)) |>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_2_6" & year == 2022, "Car_vag", species)) |>
  mutate(species = ifelse(species == "Poa_pra" & plotID == "Skj_2_6" & year == 2022, "Poa_alp", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_2_6" & year == 2021, "Ran_acr", species)) |> 
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Skj_2_6" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Ver_off_cf" & plotID == "Skj_2_6" & year == 2021, "Ver_off", species)) |>
  mutate(cover = ifelse(species == "Ver_off" & plotID == "Skj_2_6" & year == 2021, 2, cover)) |>
  mutate(species = ifelse(species == "Ach_mil" & plotID == "Skj_3_1", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Skj_3_1" & year == 2021, "Car_pal", species))|>
  mutate(cover = ifelse(species == "Car_pal" & plotID == "Skj_3_1" & year == 2021, 5, cover))|>
  mutate(species = ifelse(species == "Cer_cer" & plotID == "Skj_3_1" & year == 2018, "Cer_fon", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_3_1" & year == 2018, "Epi_ana", species))|> 
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_3_1" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species == "Geu_riv" & plotID == "Skj_3_1" & year == 2022, "Gen_niv", species))|>
  mutate(species = ifelse(species %in% c("Ach_mil", "Agr_cap_cf") & plotID == "Skj_3_3", "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_3_3" & year == 2018, 3, cover))|>
  mutate(species = ifelse(species == "Arc_urv" & plotID == "Skj_3_3" & year == 2022, "Bis_viv", species))|>
  mutate(cover = ifelse(species == "Bis_viv" & plotID == "Skj_3_3" & year == 2022, 10, cover))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Skj_3_3" & year == 2019, "Car_pil", species))|>
  mutate(species = ifelse(species == "Gen_cam_cf" & plotID == "Skj_3_3" & year == 2021, "Gen_niv", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Skj_3_3" & year == 2019, "Luz_mul", species))|>
  mutate(species = ifelse(species == "Sau_alp_cf" & plotID == "Skj_3_3" & year == 2022, "Sau_alp", species))|>
  mutate(cover = ifelse(species == "Sau_alp" & plotID == "Skj_3_3" & year == 2022, 10, cover))|>
  mutate(species = ifelse(species == "Bar_alp_cf" & plotID =="Skj_3_4", "Bar_alp", species))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Skj_3_4" & year == 2021, "Oma_sup", species))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_3_4" & year == 2019, "Rum_ace", species))|>
  mutate(species = ifelse(species %in% c("Nid_orchid", "Orchid") & plotID == "Skj_3_4", "Coel_vir", species))|>
  mutate(cover = ifelse(species == "Coel_vir" & plotID == "Skj_3_4" & year == 2018, 4, cover))|>
  mutate(species = ifelse(species == "Car_nor" & plotID == "Skj_3_6" & year == 2019, "Car_cap", species)) |>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Skj_3_6" & year == 2021, "Epi_ana", species)) |>
  mutate(species = ifelse(species == "Orchid" & plotID == "Skj_3_6", "Coel_vir", species)) |> 
  mutate(cover = ifelse(species == "Coel_vir" & plotID == "Skj_3_6" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_3_6" & year == 2019, "Rum_ace", species))|>
  mutate(species = ifelse(species == "Vac_myr_cf" & plotID == "Skj_3_6", "Vac_myr", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_4_1" & year == 2018, "Epi_ana", species)) |>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_4_1" & year == 2018, 1, cover))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_fla_CF") & plotID == "Skj_4_2" & year == 2018, "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_4_2" & year == 2018, 5, cover))|>
  mutate(species = ifelse(species == "Cer_fon" & plotID == "Skj_4_2" & year == 2021, "Cer_cer", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_4_2" & year == 2018, "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Skj_4_2" & year == 2018, 3, cover))|>
  mutate(species = ifelse(species == "Vio_bif" & plotID == "Skj_4_2" & year == 2018, "Vio_pal", species))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Skj_4_2" & year == 2018, 12, cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Skj_4_3" & year == 2019, "Agr_cap", species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Skj_4_3" & year == 2019, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_4_3" & year == 2019, "Ran_acr", species))|>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_sp") & plotID == "Skj_4_4" & year %in% c(2018,2019), "Car_big", species))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_4_4" & year == 2019, "Rum_ace", species)) |>
  mutate(species= ifelse(species == "Car_big_cf" & plotID == "Skj_4_5" & year == 2018, "Car_big", species)) |> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_4_5" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Skj_4_5" & year == 2019, "Car_cap", species)) |> 
  mutate(cover = ifelse(species == "Car_cap" & plotID == "Skj_4_5" & year == 2018,1,cover))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Skj_4_5" & year == 2019, "Oma_sup", species)) |>
  mutate(cover = ifelse(species == "Oma_sup" & plotID == "Skj_4_5" & year == 2018, 3, cover))|>
  mutate(cover = ifelse(species == "Cam_rot" & plotID == "Skj_4_5" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Rum_ace" & plotID == "Skj_4_5" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Sag_sag" & plotID == "Skj_4_5" & year == 2018, 1, cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Skj_5_1" & year == 2021, "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_1" & year == 2021, 7, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_5_1" & year == 2022, "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_fla" & plotID == "Skj_5_1", "Car_big", species))|> 
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_5_1", "Fes_rub", species)) |>
  mutate(species = ifelse(species == "Hyp_mac" & plotID == "Skj_5_1" & year == 2021, "Nid_seedling", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Skj_5_1" & year == 2019, "Sib_pro", species)) |>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Skj_5_1" & year == 2021, "Ver_alp", species)) |>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Skj_5_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species %in% c("Car_fla", "Car_fla_CF") & plotID == "Skj_5_2" & year %in% c(2018,2021), "Car_vag", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Skj_5_2" & year == 2019, "Ave_fle", species))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Skj_5_2" & year == 2021, "Jun_tri", species)) |>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_5_2" & year == 2022, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_5_3" & year == 2022, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Skj_5_4" & year == 2022, "Car_pil", species))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Skj_5_4" & year == 2021, 1, cover))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Skj_5_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species %in% c("Agr_mer_CF", "Agr_mer") & plotID == "Skj_5_5" & year %in% c(2018,2021), "Agr_cap", species))|> 
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_5" & year == 2018, 3, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_5" & year == 2021, 8, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Skj_5_5" & year == 2022, "Hie_pil", species))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Skj_5_5" & year == 2018, "Jun_tri", species))|>
    mutate(cover = ifelse(species == "Jun_tri" & plotID == "Skj_5_5" & year == 2018, 5, cover))|>
  mutate(species = ifelse(species %in% c("Phl_alp", "Agr_mer") & plotID == "Skj_5_6", "Agr_cap", species)) |>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_6" & year == 2018, 7, cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_5_6" & year == 2021, 11, cover))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_6_1", "Cer_cer", species))|>
  mutate(species = ifelse(species %in% c("Hie_pil", "Hie_sp") & plotID == "Skj_6_2" & year %in% c(2018,2019,2022), "Hie_alp", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Skj_6_2" & year == 2021, "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_6_2" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_6_2" & year == 2018, "Car_vag", species))|>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_6_3" & year == 2021, "Car_big", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_6_3" & year == 2018, "Cer_cer", species)) |>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Skj_6_3", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_6_4", "Car_vag", species))|> #Sjekk om overlapp
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Skj_6_4" & year == 2021, "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_6_4" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_6_6" & year == 2018, "Car_vag", species))|> 
  mutate(species = ifelse(species == "Sel_sp" & plotID == "Skj_6_6" & year == 2019, "Sel_sel", species))|>
  mutate(cover = ifelse(species == "Sel_sel" & plotID == "Skj_6_6" & year == 2019,1,cover))|>
  mutate(cover = ifelse(species == "Leo_aut" & plotID == "Skj_6_6" & year == 2019, 3, cover))|>
  mutate(species = ifelse(species == "Car_lep" & plotID == "Skj_7_1" & year == 2021, "Car_big", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Skj_7_1" & year == 2019, "Car_cap", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_7_1" & year == 2019, "Car_vag", species))|> 
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Skj_7_1" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Skj_7_1" & year == 2019, "Cer_fon", species)) |> 
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Skj_7_1" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Jun_tri" & plotID == "Skj_7_1" & year == 2019, "Ave_fle", species))|>
  mutate(species = ifelse(species == "jamne" & plotID == "Skj_7_1" & year == 2019, "Sel_sel", species))|>
  mutate(cover = ifelse(species == "Sel_sel" & plotID == "Skj_7_1" & year == 2019,3,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap", "Agr_mer_CF") & plotID == "Skj_7_2" & year %in% c(2019,2022), "Ant_odo", species))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Skj_7_2" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Skj_7_2" & year == 2022, 4, cover))|>
  mutate(species = ifelse(species == "Car_cap" & plotID == "Skj_7_5" & year == 2021, "Car_big", species))

#####Gudmeddalen#####

community_clean <- community_clean|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_2" & year == 2019, "Car_big", species))|>
  mutate(species = ifelse(species == "Vio_pal_cf" & plotID == "Gud_1_3", "Vio_pal", species))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Gud_1_3" & year == 2019, 4 ,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_4", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Bet_sp" & plotID == "Gud_1_4", "Bet_nan", species)) |>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_5", "Agr_cap", species)) |>
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_1_5", "Car_fla", species))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_6", "Agr_cap", species))|>
  mutate(cover = ifelse (species == "Ast_alp" & plotID == "Gud_1_6" & year == 2022, 3, cover ))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Gud_1_6", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_fla" & plotID == "Gud_1_6" & year == 2019, "Car_big", species))|>
 # mutate(cover = ifelse(species == "Ave_fle" & plotID == "Gud_1_6" & year == 2021, 3,cover))|> Not sure why I converted this one
  mutate(species = ifelse(species =="Ave_fle" & plotID == "Gud_1_6" & year == 2019 & subPlot %in% c(7,14,19,20,26,28,30,21,32), "Fes_rub", species))|> 
  mutate(species = ifelse(species =="Ave_fle" & plotID == "Gud_1_6" & year == 2022 & subPlot %in% c(19,26), "Fes_rub", species))|> 
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_2_2", "Car_fla", species))|> #Sjekk
  mutate(species = ifelse(species == "Car_pil" & plotID == "Gud_2_2", "Car_pal", species))|>
  mutate(species = ifelse(species == "Rum_ace_cf" & plotID == "Gud_2_2", "Rum_ace", species))|>
  mutate(species = ifelse(species == "Vio_tri_cf" & plotID == "Gud_2_3", "Vio_tri", species))|>
  mutate(species = ifelse(species == "Ver_ser_cf" & plotID == "Gud_2_4", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Gud_2_4" & year == 2019, 5, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_3_2", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_3_2" & year %in% c(2018,2019), 5, cover))|>
  mutate(species = ifelse(species %in% c("Pyr_sp", "Pyr_sp_IKKE_rotundifolia") & plotID == "Gud_3_2", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Gud_3_2", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Gud_3_2" & year %in% c(2018,2019,2022), 2, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Gud_3_3", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Gud_3_3" & year %in% c(2018,2019), 2, cover))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Gud_3_3", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Gud_3_3", "Ran_acr", species))|>
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Gud_3_3" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Gud_3_3" & year == 2021, 3, cover))|>
  mutate(species = ifelse(species == "Vio_pal" & plotID == "Gud_3_3", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Gud_3_3" & year == 2019, 11, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Gud_3_5", "Hie_alp", species)) |>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Gud_3_5", "Unknown", species))|>
  mutate(cover = ifelse (species == "Ave_fle" & plotID == "Gud_3_5" & year == 2019,2,cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_3_5" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Gud_3_6", "Hie_alp", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_6" & year == 2019, "Car_big", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_6" & year == 2021 & subPlot %in% c(4,5,6,28,33), "Car_pil", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_3_6" & year == 2021 & subPlot == 29, "Car_pil", species))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_4_1", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Pot_ere_cf" & plotID == "Gud_4_1" & year == 2021, "Pot_ere", species))|>
  mutate(species = ifelse(species =="Car_big" & plotID == "Gud_4_1" & year == 2019, "Car_fla", species))|>
  mutate(cover = ifelse(species == "Car_fla" & plotID == "Gud_4_1" & year == 2019, 11, cover))|>
  mutate(species = ifelse(species =="Car_big" & plotID == "Gud_4_1" & year == 2022, "Car_vag", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Gud_4_1" & year == 2019, "Car_nor", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Gud_4_1" & year == 2021, 7, cover))|>
  mutate(cover = ifelse(species == "Ast_alp" & plotID == "Gud_4_1" & year == 2019, 1, cover))|>
  mutate(cover = ifelse(species == "Ast_alp" & plotID == "Gud_4_3", 3, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_4_3", "Car_big", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Gud_4_3","Car_nor", species ))|>
  mutate(species = ifelse(species == "Ant_odo" & plotID == "Gud_4_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Gud_4_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Alc_alp_cf" & plotID == "Gud_4_4", "Alc_alp", species))|>
  mutate(cover = ifelse(species == "Alc_alp" & plotID == "Gud_4_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Gud_4_4", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Gud_4_4" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Gud_4_6", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Gud_4_6" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Par_pal_cf" & plotID == "Gud_4_6", "Par_pal", species))|>
  mutate(species = ifelse(species == "Pot_cra" & plotID == "Gud_4_6", "Pot_ere", species))|>
  mutate(cover = ifelse(species == "Pot_ere" & plotID == "Gud_4_6" & year == 2019, 9, cover))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Tri_ces") & plotID == "Gud_5_1", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Car_atr" & plotID == "Gud_5_1", "Car_sax", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_5_2" & year == 2019, "Car_sax", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Gud_5_2", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Nid_juvenile" & plotID == "Gud_5_2", "Nid_seedling", species))|>
  mutate(species = ifelse(species %in% c("Nid_orchid", "Orchid") & plotID == "Gud_5_2", "Coel_vir", species))|>
  mutate(species = ifelse(species == "Lyc_alp" & plotID == "Gud_5_4" & year == 2018, "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Gud_5_4", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_5_5", "Car_fla", species))|>
  mutate(cover = ifelse(species == "Car_fla" & plotID == "Gud_5_5" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Gud_5_5", "Hie_pil", species))|>
  mutate(species = ifelse(species %in% c("Lyc_alp","Sel_sel") & plotID == "Gud_5_5", "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Par_pal_cf" & plotID == "Gud_5_5", "Par_pal", species))|>
  mutate(species = ifelse(species %in% c("Pot_cra", "Pot_ere_cf") & plotID == "Gud_5_5", "Pot_ere", species))|>
  mutate(cover = ifelse(species == "Pot_ere" & plotID == "Gud_5_5" & year == 2019, 9, cover))|>
  mutate(species = ifelse(species %in% c("Pyr_sp", "Pyr_sp_cf") & plotID == "Gud_5_5", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Dip_alp" & plotID == "Gud_5_5", "Hyp_sel", species))|>
  mutate(species = ifelse(species == "Ave_fle_cf" & plotID == "Gud_5_6", "Ave_fle", species))|> 
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Gud_5_6" & year == 2018,5,cover))|>
  mutate(species = ifelse(species == "Ave_fle" & plotID == "Gud_6_1", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Hie_pil" & plotID == "Gud_6_1", "Sol_vir", species))|>
  mutate(species = ifelse(species == "Vio_bif" & plotID == "Gud_6_1", "Vio_pal", species))|>
  mutate(cover = ifelse(species =="Vio_pal" & plotID == "Gud_6_1" & year == 2022, 3,cover))|>
  mutate(species = ifelse(species == "Pyr_sp_IKKE_rotundifolia" & plotID == "Gud_6_2", "Pyr_sp",species))|>
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_6_3", "Car_fla", species))|>
  mutate(species = ifelse(species == "Vio_can_cf" & plotID == "Gud_6_3", "Vio_can", species))|>
  mutate(cover = ifelse(species == "Sil_aca" & plotID == "Gud_6_3" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Gud_6_4", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Gud_6_6", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Hie_pil_cf" & plotID == "Gud_6_6", "Hie_pil", species))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Gud_7_1", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_7_1" & year == 2018,3,cover))|>
  mutate(species = ifelse(species == "Car_pal_cf" & plotID == "Gud_7_1", "Car_pal", species ))|>
  mutate(cover = ifelse(species == "Eup_wet" & plotID == "Gud_7_1" & year == 2019, 5, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Gud_7_2", "Car_big", species))|>
  mutate(species = ifelse(species == "Vio_can" & plotID == "Gud_7_2", "Vio_pal", species))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Gud_7_2" & year == 2019, 3, cover))|>
  mutate(species = ifelse(species == "Ver_cha_cf" & plotID == "Gud_7_2", "Ver_cha", species))|> #Sjekk!
  mutate(cover = ifelse(species =="Vac_myr" & plotID == "Gud_7_2" & year == 2022, 9,cover))|>
  mutate(species = ifelse(species == "Ast_alp_cf" & plotID == "Gud_7_3", "Ast_alp", species))|> 
  mutate(cover = ifelse(species == "Ast_alp" & plotID == "Gud_7_3" & year == 2019, 21,cover))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_7_3" & year == 2018, "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Gud_7_3" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species == "Car_fla_CF" & plotID == "Gud_7_4", "Car_fla", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Gud_7_4","Car_pal", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Gud_7_4" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Pyr_sp_IKKE_rotundifolia" & plotID == "Gud_7_4", "Pyr_sp",species))|>
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Gud_7_4" & year == 2018 & is.na(cover),0,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Gud_7_6", "Agr_cap", species))|>
  mutate(cover = ifelse(species =="Agr_cap" & plotID == "Gud_7_6" & year == 2019, 7, cover))|>
  mutate(species = ifelse(species %in% c("Fes_rub","Fes_rub_cf_kanskje_Ave_fle") & plotID == "Gud_7_6" & year %in% c(2018, 2019), "Fes_rub", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Gud_7_6", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Gud_7_6", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Gud_7_6", "Car_cap", species))|>
  mutate(cover = ifelse(species == "Car_cap" & plotID == "Gud_7_6" & year %in% c(2021,2022),1 ,cover))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Gud_7_6" & year == 2019, 6, cover))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Gud_7_6" & year == 2019, 1,cover))

#####Lavisdalen#####

community_clean <- community_clean |>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_1_1", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_1_1", "Car_sp", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_1_1", "Car_nor", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_1", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Lav_1_1" & year == 2019, 4,cover))|>
  mutate(species = ifelse(species == "Fes_ovi_cf" & plotID == "Lav_1_1", "Fes_ovi", species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_1_1" & year == 2019, 3,cover))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_1_1", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_1_1" & year == 2019, 1,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_1_2", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_2", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Lav_1_2" & year == 2018,1,cover))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Lav_1_2" & year == 2019, 4,cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_1_2", "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_1_2" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species == "Fes_ovi_cf" & plotID == "Lav_1_2", "Fes_ovi", species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_1_2" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species == "Luz_spi_cf" & plotID == "Lav_1_2", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_1_2", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_1_2" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_1_3", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_3" & year == 2019, 7,cover))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_3" & year == 2021, 2,cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_1_3", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_1_3" & year == 2019, 1,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_1_3", "Car_nor", species))|>
  mutate(cover = ifelse(species == "Bis_viv" & plotID == "Lav_1_3" & year == 2022, 4,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_1_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_4" & year == 2021, 2,cover))|>
  mutate(species = ifelse(species == "Ant_odo_cf" & plotID == "Lav_1_4", "Ant_odo", species))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Lav_1_4" & year == 2019, 5,cover))|>
  mutate(species = ifelse(species %in% c("Cer_alp_cf", "Cer_alp") & plotID == "Lav_1_4", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID =="Lav_1_4" & year == 2019, 3,cover))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_4", "Cer_cer", species))|>
  mutate(species = ifelse(species == "Eri_uni_cf" & plotID == "Lav_1_4", "Eri_uni", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_1_4", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_1_4" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_1_6", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID =="Lav_1_6" & year == 2021, 2,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_1_6", "Car_nor", species))|> 
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_1_6", "Cer_cer", species))|> 
  mutate(species = ifelse(species == "Cer_alp" & plotID =="Lav_1_6", "Cer_fon",species))|>
  mutate(species = ifelse(species == "Fes_ovi_cf" & plotID =="Lav_1_6", "Fes_ovi",species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_1_6" & year == 2019, 1,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_1", "Car_nor", species))|> 
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_2_2", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_2", "Car_nor", species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_2_2",2, cover))|>
  mutate(species = ifelse(species == "Car_sp_den_lyse" & plotID == "Lav_2_2", "Car_pil", species))|>
  mutate(cover = ifelse(species == "Car_pil" & plotID == "Lav_2_2" & year == 2019 ,3, cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Lav_2_2", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_2_2",4, cover))|>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Lav_2_3", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Alc_alp_cf" & plotID == "Lav_2_3", "Alc_alp",species))|>
  mutate(cover = ifelse(species == "Alc_alp" & plotID == "Lav_2_3" & year == 2019,13,cover))|>
  mutate(species = ifelse(species == "Ave_fle_cf" & plotID == "Lav_2_3", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Lav_2_3" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Car_pil" & plotID == "Lav_2_3", "Car_pal", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_3", "Car_nor", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_2_3", "Poa_pra", species))|>
  mutate(species = ifelse(species %in% c("Ranunculus", "Ran_acr") & plotID == "Lav_2_3", "Ran_pyg", species))|>
  mutate(species = ifelse(species %in% c("Vac_myr", "Ver_alp_cf") & plotID == "Lav_2_3", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_2_3" & year == 2019,3,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_2_4", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_4", "Car_nor", species))|>
  mutate(species = ifelse(species == "Poa_pra_cf" & plotID == "Lav_2_4", "Poa_pra", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_2_4", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ran_sp" & plotID == "Lav_2_4", "Ran_acr", species))|>
  mutate(species = ifelse (species == "Ver_alp_cf" & plotID == "Lav_2_4", "Ver_alp", species))|> 
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_2_4" & year == 2019,3,cover))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Lav_2_5" & year == 2022, "Agr_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_2_5" & year == 2019, "Car_nor", species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_2_5" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_2_5" & year == 2019, "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_2_5" & year == 2019,1,cover))|>
  mutate(species = ifelse(species == "Poa_alp_cf" & plotID == "Lav_2_5" & year == 2019, "Poa_alp", species))|>
  mutate(cover = ifelse(species == "Poa_alp" & plotID == "Lav_2_5" & year == 2019,2,cover))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Lav_2_5" & year == 2021, 5,cover))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_2_5", "Pyr_min", species))|>
  mutate(species = ifelse (species == "Ver_alp_cf" & plotID == "Lav_2_5", "Ver_alp", species))|> 
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_2_5" & year == 2019,2,cover))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Lav_2_6", "Oma_sup", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_2_6", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Lav_2_6", "Ran_acr", species))|>
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Lav_2_6" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Vac_myr" & plotID == "Lav_2_6", "Sal_her", species))|>
  mutate(cover = ifelse(species == "Sal_her" & plotID == "Lav_2_6" & year == 2021, 5, cover ))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Lav_3_1", "Ave_fle", species))|>
  mutate(species = ifelse(species %in% c("Agr_cap","Agr_mer_CF") & plotID == "Lav_3_1", "Agr_mer", species))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_3_1" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_3_1" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_3_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Lav_3_1", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Lav_3_1" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_3_1", "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_3_1" & year == 2019, 3, cover))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Lav_3_1", "Ran_acr", species))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Lav_3_1", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Lav_3_1" & year == 2021,2,cover))|>
  mutate(species = ifelse(species == "Car_nor" & plotID == "Lav_3_3" & year == 2021, "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_3_3" & year == 2021, 6, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Lav_3_3", "Epi_ana", species))|>
  mutate(cover = ifelse(species == "Epi_ana" & plotID == "Lav_3_3" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Lav_3_3", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Lav_3_3" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Oxy_dig" & plotID == "Lav_3_3" & year == 2019,2 , cover))|>
  mutate(species = ifelse(species == "Tar_sp_cf" & plotID == "Lav_3_3", "Tar_sp", species))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Lav_3_3" & year == 2019,7 , cover))|>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Lav_3_3", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Lav_3_3" & year == 2019, 2, cover)) |>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Lav_3_3", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Lav_3_4", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Lav_3_4" & year == 2018, 2, cover))|>
  mutate(species = ifelse(species %in% c("Car_sp","Car_vag", "Car_vag_CF") & plotID == "Lav_3_4","Car_big", species ))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_3_4" & year == 2019, 6, cover))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Lav_3_5", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Lav_3_5" & year == 2019, 7, cover))|>
  mutate(cover = ifelse(species == "Rum_ace" & plotID == "Lav_3_5" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_3_6", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_3_6" & year == 2021,2,cover))|>
  mutate(species = ifelse(species %in% c("Car_sp","Car_nor_cf") & plotID == "Lav_3_6", "Car_nor", species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_3_6" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_3_6" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species =="Fes_viv" & plotID == "Lav_3_6", "Fes_ovi", species))|>
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Lav_3_6" & year == 2021, 4, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_4_1", "Car_nor", species))|> # Må sjekkes nøye i 2023!!!
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Lav_4_1", "Car_vag", species))|>
  mutate(species = ifelse(species == "Gen_cam_cf" & plotID == "Lav_4_1", "Gen_cam", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Lav_4_1", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Sal_sp" & plotID == "Lav_4_1", "Sal_lan", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_4_3","Car_nor",species))|>
  mutate(species = ifelse(species =="Des_ces" & plotID == "Lav_4_2","Des_alp",species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Lav_4_4", "Car_cap", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_4_4", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Lav_4_5", "Car_cap", species))|>
  mutate(species = ifelse(species == "Fes_ovi" & plotID == "Lav_4_5", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Lav_4_5", "Pyr_sp", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_5_2", "Car_big", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_5_2", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_5_2"& year == 2021, 10, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_5_2", "Car_nor", species))|>
  mutate(species = ifelse(species %in% c("Car_nig_cf", "Car_nor_cf") & plotID == "Lav_5_3", "Car_nor",species))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_5_3" & year == 2021,2,cover))|>
  mutate(cover = ifelse(species == "Car_nor" & plotID == "Lav_5_3" & year == 2022,1,cover))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_5_3", "Car_pil", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Lav_5_5", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_5_5" & year == 2021,2,cover))|>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_nor_cf") & plotID == "Lav_5_5", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_5_5" & year == 2019,4,cover))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_5_5" & year == 2021,4,cover))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Lav_5_5", "Cer_fon", species))|>
  mutate(species = ifelse(species %in% c("Hie_alp", "Hie_sp") & plotID == "Lav_5_5", "Hie_pil",species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Lav_5_5", "Poa_pra", species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Lav_5_5", "Pyr_sp", species))|>
  mutate(species = ifelse(species %in% c("Agr_mer", "Agr_cap_cf") & plotID == "Lav_5_6", "Agr_cap", species))|>
  mutate(cover = ifelse (species == "Agr_cap" & plotID == "Lav_5_6" & year == 2021,2,cover))|>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_nor_cf")& plotID == "Lav_5_6", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Lav_5_6" & year == 2019,5,cover))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Lav_5_6", "Cer_fon",species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Lav_5_6", "Pyr_sp", species))|>
  mutate(species = ifelse(species == "Sal_sp" & plotID == "Lav_5_5", "Sal_lan", species))|>
  mutate(species = ifelse(species == "Agr_cap" & plotID == "Lav_6_2", "Agr_mer", species))|>
  mutate(cover = ifelse(species == "Agr_mer" & plotID == "Lav_6_2" & year == 2018,2,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Lav_6_2", "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_cap" & plotID == "Lav_6_2", "Car_nor", species ))|>
  mutate(species = ifelse(species == "Agr_cap" & plotID == "Lav_6_3" & year == 2022, "Agr_mer", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_6_3", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_sp_den_lyse" & plotID == "Lav_6_3", "Car_sp", species))|>
  mutate(species = ifelse(species == "Car_sp_den_lyse" & plotID == "Lav_6_5", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_6_5" & year == 2022, 7,cover))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Lav_6_5", "Jun_tri", species))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Lav_6_5", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Lav_6_5" & year == 2022, 1,cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_6_6", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Lav_6_6", "Car_pil", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Lav_7_1", "Car_nor", species))|>
  mutate(species = ifelse(species %in% c("Fes_viv_cf", "Fes_ovi") & plotID == "Lav_7_1", "Fes_viv", species))|>
  mutate(cover = ifelse(species == "Fes_viv" & plotID == "Lav_7_1" & year == 2019, 4, cover))|>
  mutate(cover = ifelse(species == "Fes_viv" & plotID == "Lav_7_1" & year == 2022, 2, cover))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Lav_7_1", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Lav_7_1", "Ran_acr", species))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Lav_7_2", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Lav_7_2", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Lav_7_2", 3, cover))|>
  mutate(species = ifelse(species == "Fes_ovi" & plotID == "Lav_7_2", "Fes_viv", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Lav_7_2", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Lav_7_2", "Poa_pra", species))|>
  mutate(cover = ifelse(species == "Poa_pra" & plotID == "Lav_7_2" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Ave_fle" & plotID == "Lav_7_3", "Fes_ovi", species))|>
  mutate(species = ifelse(species == "Des_ces" & plotID == "Lav_7_3", "Des_alp", species))|>
  mutate(cover = ifelse(species =="Des_alp" & plotID == "Lav_7_3" & year == 2021, 6,cover))|>
  mutate(species = ifelse(species == "Fes_viv" & plotID == "Lav_7_3", "Fes_ovi", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Lav_7_2", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Lav_7_2", "Poa_pra", species))
  
#####Ulvehaugen#####

community_clean <- community_clean |>
  mutate(species = ifelse(species == "Vio_sp" & plotID == "Ulv_1_1", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_1_1" & year == 2021, 7, cover))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_1_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Ave_fle_cf" & plotID == "Ulv_1_3", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Ulv_1_3", "Car_vag", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Ulv_1_3", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Ulv_1_3", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_1_3" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Tri_sp" & plotID == "Ulv_1_3", "Tri_rep", species))|>
  mutate(cover = ifelse(species == "Tha_alp" & plotID == "Ulv_1_3" & year == 2021, 1, cover))|>
  mutate(cover = ifelse(species == "Vac_vit" & plotID == "Ulv_1_3" & year == 2021, 1, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_1_4", "Alc_sp", species))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Ulv_1_4" & year == 2019,4,cover))|>
  mutate(species = ifelse(species == "Cer_sag_cf" & plotID == "Ulv_1_4", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Ulv_1_4" & year == 2019,4,cover))|>
  mutate(species = ifelse(species == "Oma_nor" & plotID == "Ulv_1_4", "Oma_sup", species))|>
  mutate(species = ifelse(species == "Poa_pra" & plotID == "Ulv_1_4", "Poa_alp", species))|>
  mutate(species = ifelse(species == "Unknown" & plotID == "Ulv_1_4", "Des_ces", species))|>
  mutate(species = ifelse(species == "Cer_alp_cf" & plotID == "Ulv_1_5", "Cer_cer", species))|>
  mutate(cover = ifelse(species == "Cer_cer" & plotID == "Ulv_1_5" & year == 2019, 6,cover))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_1_5", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_2_1", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Vio_pal" & plotID == "Ulv_2_1", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_2_1" & year == 2019, 6, cover))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Ulv_2_2", "Car_pil", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_2_2", "Leo_aut", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Ulv_2_2" & year == 2022,1,cover ))|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Ulv_2_3", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Ulv_2_3" & year == 2019,19,cover))|>
  mutate(cover = ifelse(species == "Alc_sp" & plotID == "Ulv_2_3" & year == 2022,2,cover))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_2_3", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Ulv_2_4", "Ave_fle", species))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Ulv_2_4" & year == 2022, 2,cover))|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Ulv_2_4", "Oma_sup", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_2_4" & year == 2019, 2,cover))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Ulv_2_4", "Sib_pro", species))|>
  mutate(cover = ifelse(species == "Leo_aut" & plotID == "Ulv_2_4" & year == 2018, 1,cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_2_4" & year == 2022,3,cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_2_5", "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Ulv_2_5", "Car_cap", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_2_5", "Leo_aut", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_2_5" & year == 2022,2, cover))|>
  mutate(species = ifelse(species %in% c("Lyc_sp", "Sel_sp") & plotID == "Ulv_2_5", "Sel_sel", species))|>
  mutate(species = ifelse(species == "Ver_cha" & plotID == "Ulv_2_5", "Ver_alp", species))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Ulv_2_5" & year == 2019, 2, cover))|>
  mutate(cover = ifelse(species == "Sib_pro" & plotID == "Ulv_2_5" & year == 2019, 5,cover))|>
  mutate(cover = ifelse(species =="Alc_sp" & plotID == "Ulv_2_5" & year == 2021, 30, cover))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Ulv_3_1", "Epi_ana", species))|> 
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Ulv_3_1", "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_3_1" & year == 2018,4,cover))|>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Ulv_3_2", "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Ulv_3_2" & year == 2019, 14, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_3_2", "Alc_sp", species))|>
  mutate(cover = ifelse(species == "Des_ces" & plotID == "Ulv_3_2" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Unknown" & plotID == "Ulv_3_2", "Coel_vir", species))|>
  mutate(species = ifelse(species == "Epi_nor" & plotID == "Ulv_3_2", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Ulv_3_2", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_3_2", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Epi_nor" & plotID == "Ulv_3_3", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Ver_cha" & plotID == "Ulv_3_3", "Ver_alp", species))|>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Ulv_3_3", "Ver_alp", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Ulv_3_4", "Ave_fle", species))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Ulv_3_4", "Pyr_min", species))|>
  mutate(species = ifelse(species == "Epi_nor" & plotID == "Ulv_3_4", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Epi_sp" & plotID == "Ulv_3_5", "Epi_ana", species))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Ulv_3_5", "Hie_pil",species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Ulv_3_5", "Leo_aut", species))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Ulv_4_1", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_4_1" & year == 2019, 4, cover))|>
  mutate(species = ifelse(species == "Car_sp_2" & plotID == "Ulv_4_1", "Car_sp", species))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Ulv_4_1", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Poa_alp" & plotID == "Ulv_4_1" & year == 2019 ,1,cover))|>
  mutate(cover = ifelse(species == "Sal_her" & plotID == "Ulv_4_1" & year == 2019 ,1,cover))|>
  mutate(species = ifelse(species == "Vio_sp" & plotID == "Ulv_4_1", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_4_1" & year == 2021, 5,cover))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Ulv_4_3", "Car_big", species))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_4_3" & year == 2018, 17,cover))|>
  mutate(species = ifelse(species %in% c("Gen_sp", "Gen_ana") & plotID == "Ulv_4_3", "Gen_ama", species))|>
  mutate(species = ifelse(species == "Pot_ere" & plotID == "Ulv_4_3", "Pot_cra", species))|>
  mutate(species = ifelse(species == "Poa_alp" & plotID == "Ulv_4_3", "Poa_pra", species))|>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID == "Ulv_4_4", "Car_pil", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Ulv_4_4", "Sib_pro", species))|>
  mutate(species = ifelse(species == "Vio_pal" & plotID == "Ulv_4_4", "Vio_bif", species))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_4_4" & year == 2022, 6,cover))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Ulv_5_1", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Ulv_5_1" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species %in% c("Unknown", "Eri_uni_cf") & plotID == "Ulv_5_1", "Eri_uni", species))|>
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Ulv_5_1", "Hie_alp", species))|>
  mutate(species = ifelse(species == "Rum_ace_cf" & plotID == "Ulv_5_1", "Rum_ace", species))|>
  mutate(cover = ifelse(species == "Rum_ace" & plotID == "Ulv_5_1" & year == 2021, 3, cover))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Ulv_5_1" & year == 2021, 2, cover))|>
  mutate(cover = ifelse(species == "Cam_rot" & plotID == "Ulv_5_1" & year == 2019, 2, cover))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Ulv_5_3", "Rum_ace", species))|>
  mutate(species = ifelse(species %in% c("Car_sp", "Car_big") & plotID == "Ulv_5_3", "Car_vag", species))|>
  mutate(cover = ifelse(species == "Car_vag" & plotID == "Ulv_5_3" & year == 2018,8, cover))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Ulv_5_3", "Rum_ace", species))|>
  mutate(species = ifelse(species == "Car_big" & plotID == "Ulv_5_4", "Car_vag", species))|>
  mutate(species = ifelse(species == "Ant_sp" & plotID == "Ulv_5_4", "Ant_dio", species))|>
  mutate(species = ifelse(species == "Jun_sp" & plotID == "Ulv_5_4", "Jun_tri", species))|>
  mutate(cover = ifelse(species == "Jun_tri" & plotID == "Ulv_5_4" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Pyr_sp" & plotID == "Ulv_5_4", "Pyr_min", species))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Ulv_5_4" & year == 2019, 1,cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_5_5" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Ulv_5_5", "Car_nor", species))|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Ulv_5_5", "Car_vag", species))|>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Ulv_5_5", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Ulv_5_5", "Luz_mul", species))|>
  mutate(species = ifelse(species == "Pyr_min" & plotID == "Ulv_5_5", "Pyr_sp", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Ulv_5_5", "Sib_pro", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_6_1", "Leo_aut", species))|>
  mutate(species = ifelse(species =="Alc_alp_cf" & plotID == "Ulv_6_1", "Alc_alp", species))|>
  mutate(cover = ifelse(species =="Alc_alp" & plotID == "Ulv_6_1" & year == 2021, 1,cover))|>
  mutate(species = ifelse(species == "Car_sp_smal" & plotID == "Ulv_6_2", "Car_cap", species))|>
  mutate(species = ifelse(species == "Phl_alp_cf" & plotID == "Ulv_6_2", "Phl_alp", species))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_6_2" & year == 2021, 2, cover))|>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Ulv_6_4", "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_sp_smal" & plotID == "Ulv_6_4", "Car_sp", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Ulv_6_4", "Car_nor", species))|>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Ulv_6_5", "Leo_aut", species))|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Ulv_6_5", "Car_nor", species))|>
  mutate(species = ifelse(species == "Agr_cap" & plotID == "Ulv_6_6", "Agr_mer", species))|>
  mutate(cover = ifelse(species =="Agr_mer" & plotID == "Ulv_6_6" & year == 2022,16,cover))|>
  mutate(species = ifelse(species == "Car_sp_smal" & plotID == "Ulv_6_6", "Car_big", species))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle") & plotID == "Ulv_7_2", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Ulv_7_2", "Car_big", species))|>
  mutate(species = ifelse(species == "Cer_alp" & plotID == "Ulv_7_2", "Cer_fon", species))|>
  mutate(cover = ifelse(species == "Car_pil" & plotID == "Ulv_7_2" & year == 2019, 1, cover))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Ulv_7_2" & year == 2021, 3, cover))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle") & plotID == "Ulv_7_3", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Alc_sp" & plotID == "Ulv_7_3", "Alc_alp", species))|>
  mutate(species = ifelse(species == "Hyp_mac" & plotID == "Ulv_7_3", "Hyp_sp", species))|>
  mutate(cover = ifelse(species == "Ver_off" & plotID == "Ulv_7_2" & year == 2019 & is.na(cover),0, cover))|>
  mutate(cover = ifelse(species =="Vio_can" & plotID == "Ulv_7_2" & year == 2019, 1, cover))|>
  mutate(species = ifelse(species %in% c("Ave_fle", "Fes_rub_cf_kanskje_Ave_fle", "Fes_ovi") & plotID == "Ulv_7_4", "Fes_rub", species))|>
  mutate(species = ifelse(species == "Sil_aca_cf" & plotID == "Ulv_7_4", "Sil_aca", species))|>
  mutate(cover = ifelse(species == "Tha_alp" & plotID == "Ulv_7_4" & year == 2019, 1, cover))|>
  mutate(cover = ifelse(species == "Ach_mil" & plotID == "Ulv_7_4" & year == 2019 & is.na(cover),0, cover))


####Ulv_7_3 2018####
#The Ulv_7_3 2018 plot lacks cover. To use the data, we have decided to give it approximatly the same cover as the year after

community_clean <- community_clean|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Alc_alp" & plotID == "Ulv_7_3" & year == 2018, 10, cover))|>
  mutate(cover = ifelse(species == "Ant_odo" & plotID == "Ulv_7_3" & year == 2018, 3, cover))|>
  mutate(cover = ifelse(species == "Ave_fle" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Bis_viv" & plotID == "Ulv_7_3" & year == 2018, 15, cover))|>
  mutate(cover = ifelse(species == "Car_big" & plotID == "Ulv_7_3" & year == 2018,4 , cover))|>
  mutate(cover = ifelse(species == "Car_pal" & plotID == "Ulv_7_3" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Cer_fon" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Des_ces" & plotID == "Ulv_7_3" & year == 2018, 6, cover))|>
  mutate(cover = ifelse(species == "Eup_wet" & plotID == "Ulv_7_3" & year == 2018, 2, cover))|> #Sjekk om heter Eup_wet
  mutate(cover = ifelse(species == "Fes_ovi" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Fes_rub" & plotID == "Ulv_7_3" & year == 2018 & is.na(cover), 0, cover))|>
  mutate(cover = ifelse(species == "Leo_aut" & plotID == "Ulv_7_3" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Luz_mul" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Phl_alp" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Pyr_sp" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|> #Sjekk om riktig navn
  mutate(cover = ifelse(species == "Ran_acr" & plotID == "Ulv_7_3" & year == 2018, 9, cover))|>
  mutate(cover = ifelse(species == "Sal_her" & plotID == "Ulv_7_3" & year == 2018, 5, cover))|>
  mutate(cover = ifelse(species == "Sel_sel" & plotID == "Ulv_7_3" & year == 2018, 3, cover))|>
  mutate(cover = ifelse(species == "Sib_pro" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Tar_sp" & plotID == "Ulv_7_3" & year == 2018, 2, cover))|>
  mutate(cover = ifelse(species == "Ver_alp" & plotID == "Ulv_7_3" & year == 2018, 1, cover))|>
  mutate(cover = ifelse(species == "Vio_bif" & plotID == "Ulv_7_3" & year == 2018, 15, cover))|>
  mutate(cover = ifelse(species == "Vio_pal" & plotID == "Ulv_7_3" & year == 2018, 1, cover))
