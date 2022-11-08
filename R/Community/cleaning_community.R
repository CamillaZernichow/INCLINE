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
community_data <- read_delim("data\\INCLINE_community_2018_2019_2021_2022_korrekturlest.csv", delim = ";")
meta_data <- read_delim("data\\INCLINE_metadata.csv", delim = ";")


#### Fixing mistakes in the dataset ####

##### Cleaning variables in dataset #####
# By renaming columns after the same standard, it is easier to use them, and we therefor don't need to think about their unique names to use them.

#community data
community_data <- community_data |>
  rename(Cer_sag_cf = "Cer/sag_cf", Cer_sp = "Cer _sp", Fes_sp = Fes.sp., Vac_myr_cf = Var_myr_cf, Nid_seedling = "Nid seedling", block = Block, measure = Measure, site = Site, treatment = Treatment)|> #Changed wrong types and capital letters to small letters. 
  mutate(across(Ach_mil:Nid_seedling, .fns = as.character))|>
  mutate(plotID = paste0(substr(site, 1,3), "_", block, "_", plot))|>
  select(-treatment,-...226)

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
  pivot_longer(Ach_mil:Nid_seedling, values_drop_na = TRUE)|>
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

prove |>
  filter(plotID == "Skj_1_1")|> #Put in the plotID of the plot you want to investigate.
  mutate(subPlot = as.integer(subPlot)) |>
  filter(presence != "0")|> 
  bind(x,
       make_turf_plot(
         data = x,
         year = year, species = species, cover = cover, subturf = subPlot,
         title = glue::glue("{x$plotID} Treatment: {x$OTC} {x$treatment}"), 
         grid_long = grid
       ))

#Removing unnecessary tables
rm(cover_column)



#### Cleaning mistakes from dataset ####


#General coding for all plots#
community_clean <- community_data_longer |>
  mutate(species = ifelse(species == "Tri_eur", "Lys_eur", species))|>
  mutate(species = ifelse (species == "Antenoria_sp", "Ant_sp", species))|>
  mutate(species = ifelse(species %in% c("Eup_sp", "Eup_str"), "Eup_wet", species))|>
  mutate(species = ifelse (species == "Vio_riv", "Vio_can", species))
#Renamed Tri_eur to Lys_eur, Antenoria_sp to Ant_sp, Eup_sp and Eup_str to Eup_wet, and Vio_riv to Vio Can


#Codes for each specific change in each plot#

####Skjellingahaugen####

#Skj_1_1: Merged Agr_mer and Agr_cap, Car_big_cf and Car_big, Sib_pro_cf and Sib_pro. Summarised cover for Agr_cap and Agr_mer, and fixed cover for Car_big and Sib_pro
prove <- community_clean |>
  mutate(species = ifelse(species == "Agr_mer" & plotID == "Skj_1_1" & year %in% c(2018, 2021), "Agr_cap", species))|>
  mutate(cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_1" & year == 2018, 4, cover), cover = ifelse(species == "Agr_cap" & plotID == "Skj_1_1" & year == 2021,5, cover ))|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_sp") & plotID == "Skj_1_1" & year %in% c(2021, 2022), "Car_big", species))|> 
  mutate(cover = ifelse(species == "Car_big" & plotID == "Skj_1_1" & year == 2021, 2, cover), cover = ifelse(species == "Car_big" & plotID == "Skj_1_1" & year == 2022,6, cover ))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_1_1" & year == 2021, "Ran_acr", species)) |>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Skj_1_1" & year == 2018, "Sib_pro", species))|>
  mutate(cover = ifelse(species == "Sib_pro" & plotID == "Skj_1_1", 4, cover))
#Filter unknown


#Skj_1_3
#Slå sammen Car_nor og Car_big_cf til Car_big 
#Slå sammen Phl_alp og Agr_cap 
prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_nor") & plotID == "Skj_1_3" & year == 2018, "Car_big", species)) |> #Summarise and fix cover
  mutate(species = ifelse(species == "Phl_alp" & plotID == "Skj_1_3" & year == 2022, "Agr_cap", species)) #summarise cover
#Sjekk rådata på Vio_pal 2019 Skj_1_3


#Skj_1_4
#Slå sammen Agr_mer og Agr_cap_cf til Agr_cap
#Slå sammen Car_vag og Car_fla #2022 enda ikke fikset!
#Slå sammen Leu_aut_cf til Leo_aut
#Usikker på hvor fes_rub_cf skal
prove <- community_clean|>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_1_4" & year == 2019, "Car_fla", species)) |> #summarise cover
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_1_4" & year == 2021, "Leo_aut", species)) |>
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_1_4" & year == 2022, "Fes_rub", species))
#filtrere unknown
#Skriv kommentar på Agr_mer i artikkel


#Skj_1_5
#Hyp_mac: Rådata sier plassering 24, men transplant info sier 27
#Slå sammen Pot_ere og Pot_cra
prove <- community_clean|>
  mutate(species = ifelse(species == "Pot_ere" & plotID == "Skj_1_5" & year == 2022, "Pot_cra", species))


#Skj_2_1
#Slå sammen Car_atr og Car_big? 
#Slå sammen Epi_ana_cf og Epi_ana
prove <- community_clean|>
  mutate(species = ifelse(species == "Car_atr" & plotID == "Skj_2_1" & year == 2018, "Car_big", species))|> #Summarise cover
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_2_1" & year == 2018, "Epi_ana", species))#Summarise and fix cover
#Tror ikke vi gjør noe med Tar_sp. Forslag om å endre den til Hie_pil, men Hie_pil har ikke blitt funnet tidligere


#Skj_2_2
#Slå sammen Car_nor_cf og Car_big
#Slå sammen Gen_sp og Gen_niv
#Slå sammen Ran_acr_cf og Ran_acr
prove <- community_clean|>
  mutate(species = ifelse(species == "Car_nor_cf" & plotID == "Skj_2_2" & year == 2018, "Car_big", species)) #Summarise cover
  mutate(species = ifelse(species == "Gen_sp" & plotID == "Skj_2_2" & year == 2018, "Gen_niv", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_2_2" & year == 2022, "Ran_acr", species))


#Skj_2_5
#Slå sammen Alc_sp_cf og Alc_sp
#Slå sammen Cer_cer_cf og Cer_cer
#Slå sammen Epi_ana_cf, Epilobium_sp og Epi_ana
#Slå sammen Equ_sp og Equ_arv
#Slå sammen Lyc_lyc og Sel_Sel
prove <- community_clean |>
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_2_5" & year == 2022, "Alc_sp", species)) |>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_2_5" & year == 2018, "Cer_cer", species)) |>
  mutate(species = ifelse(species %in% c("Epi_ana_cf", "Epilobium_sp") & plotID == "Skj_2_5" & year %in% c(2018,2022), "Epi_ana", species)) |>
  mutate(species = ifelse(species == "Equ_sp" & plotID == "Skj_2_5" & year %in% c(2019, 2021), "Equ_arv", species)) |> #summarise cover
  mutate(species = ifelse(species == "Lyc_lyc" & plotID == "Skj_2_5" & year == 2019, "Sel_sel", species))


#Skj_2_6
#Slå sammen Agr_cap_cf og Agr_cap
#Slå sammen Alc_sp_cf og Alc_sp
#Slå sammen Car_vag_CF og Car_vag
#Slå sammen Poa_pra og Poa_alp
#Slå sammen Ran_acr_cf og Ran_acr
#Slå Ver_off_cf og Ver_off sammen
prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Skj_2_6" & year == 2021, "Agr_cap", species))|> #Summarise cover
  mutate(species = ifelse(species == "Alc_alp_cf" & plotID == "Skj_2_6" & year == 2022, "Alc_alp", species)) |>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_2_6" & year == 2022, "Car_vag", species)) |>
  mutate(species = ifelse(species == "Poa_pra" & plotID == "Skj_2_6" & year == 2022, "Poa_alp", species)) |>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_2_6" & year == 2021, "Ran_acr", species)) |> #Fix cover
  mutate(species = ifelse(species == "Ver_off_cf" & plotID == "Skj_2_6" & year == 2021, "Ver_off", species)) #Fix cover


#Skj_3_1
#Slå sammen Car_pil_cf og Car_pal
#Slå sammen Epi_ana_cf og Epi_ana
#Slå sammen Cer_cer og Cer_fon
#Sjekk Geu_riv
prove <- community_clean |>
  mutate(species = ifelse(species == "Car_pil_cf" & plotID = "Skj_3_1" & year == 2021, "Car_pal", species))|> #summarise cover
  mutate(species = ifelse(species == "Cer_cer" & plotID == "Skj_3_1" & year == 2018, "Cer_fon", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_3_1" & year == 2018, "Epi_ana", species))|> #Fix and summarise cover
  mutate(species = ifelse(species == "Geu_riv" & plotID == "Skj_3_1" & year == 2022, "Gen_niv", species))


#Skj_3_3
#Slå sammen Arc_urv og Bis_viv
#Slå sammen Ach_mil, Agr_cap_cf og Agr_cap
#Slå sammen Car_pal og Car_pil
#Slå sammen Gen_cam_cf og Gen_niv
#Slå sammen Luz_spi og Luz_mul
#Slå sammen Sau_alp_cf og Sau_alp
prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Ach_mil", "Agr_cap_cf") & plotID == "Skj_3_3", "Agr_cap", species))|> #Fix cover
  mutate(species = ifelse(species == "Arc_urv" & plotID == "Skj_3_3" & year == 2022, "Bis_viv", species))|>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Skj_3_3" & year == 2019, "Car_pil", species))|>
  mutate(species = ifelse(species == "Gen_cam_cf" & plotID == "Skj_3_3" & year == 2021, "Gen_niv", species))|>
  mutate(species = ifelse(species == "Luz_spi" & plotID == "Skj_3_3" & year == 2019, "Luz_mul", species))|>
  mutate(species = ifelse(species == "Sau_alp_cf" & plotID == "Skj_3_3" & year == 2022, "Sau_alp", species))#Fix cover


#Skj_3_4
#Slå sammen Nid_orchid, Orchid og Coel_vir
#slå sammen Oma_sp og Oma_sup
#Slå sammen Rum_acl og Rum_ace
prove <- community_clean|>
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Skj_3_4" & year == 2021, "Oma_sup", species))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_3_4" & year == 2019, "Rum_ace", species))|>
  mutate(species = ifelse(species %in% c("Nid_orchid", "Orchid") & plotID == "Skj_3_4", "Coel_vir", species))#Summarise cover


#Skj_3_6
#Slå sammen Car_nor og Car_cap?
#Slå sammen Rum_acl og Rum_ace <- sjekket håndskrift, skal være ace
#Slå sammen Epilobium_sp og Epi_ana
#Slå sammen Orchid og Coel_vir
#Slå sammen Vac_myr_cf og Vac_myr
prove <- community_clean|>
  mutate(species = ifelse(species == "Car_nor" & plotID == "Skj_3_6" & year == 2019, "Car_cap", species)) |>
  mutate(species = ifelse(species == "Epilobium_sp" & plotID == "Skj_3_6" & year == 2021, "Epi_ana", species)) |>
  mutate(species = ifelse(species == "Orchid" & plotID == "Skj_3_6", "Coel_vir", species)) |> #summarise cover
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_3_6" & year == 2019, "Rum_ace", species))|>
  mutate(species = ifelse(species == "Vac_myr_cf" & plotID == "Skj_3_6", "Vac_myr", species)) 


#Skj_4_1
#Slå sammen Epi_ana_cf og Epi_ana
prove <- community_clean |>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_4_1" & year == 2018, "Epi_ana", species)) 


#Skj_4_2
#Slå sammen Car_big_cf, Car_fla_CF og Car_big
#Slå sammen Cer_fon og Cer_cer
#Slå sammen Epi_ana_cf og Epi_ana
#Slå sammen Vio_bif og Vio_pal
prove <- community_clean|>
  mutate(species = ifelse(species %in% c("Car_big_cf", "Car_fla_CF") & plotID == "Skj_4_2" & year %in% c(2018), "Car_big", species))|> #Fix and summarise cover
  mutate(species = ifelse(species == "Cer_fon" & plotID == "Skj_4_2" & year == 2021, "Cer_cer", species))|>
  mutate(species = ifelse(species == "Epi_ana_cf" & plotID == "Skj_4_2" & year == 2018, "Epi_ana", species))|> #summerise and fix cover
  mutate(species = ifelse(species == "Vio_bif" & plotID == "Skj_4_2" & year == 2018, "Vio_pal", species)) #Summarise cover


#Skj_4_3
#Slå sammen Agr_cap_cf og Agr_cap
#Slå sammen Leo_sp og Leo_aut
#Slå sammen Ran_acr_cf og Ran_acr
prove <- community_clean |>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Skj_4_3" & year == 2019, "Agr_cap", species))|>
  mutate(species = ifelse(species == "Leo_sp" & plotID == "Skj_4_3" & year == 2019, "Leo_aut", species))|>
  mutate(species = ifelse(species == "Ran_acr_cf" & plotID == "Skj_4_3" & year == 2019, "Ran_acr", species))


#Skj_4_4
#Slå sammen Car_sp, Car_nor og Car_big
#Slå sammen Rum_acl og Rum_ace
prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Car_nor", "Car_sp") & plotID == "Skj_4_4" & year %in% c(2018,2019), "Car_big", species))|>
  mutate(species = ifelse(species == "Rum_acl" & plotID == "Skj_4_4" & year == 2019, "Rum_ace", species))


#Skj_4_5
#Slå sammen Car_big_cf og Car_big
#Slå sammen Oma_sp og Oma_sup
prove <- community_clean |>
  mutate(species= ifelse(species == "Car_big_cf" & plotID == "Skj_4_5" & year == 2018, "Car_big", species)) |> #Fix cover
  mutate(species = ifelse(species == "Car_sp" & plotID == "Skj_4_5" & year == 2019, "Car_cap", species)) |> #Slå sammen Car_cap og Car_sp <- Heller litt mot å slå til Car_cap? Fix cover
  mutate(species = ifelse(species == "Oma_sp" & plotID == "Skj_4_5" & year == 2019, "Oma_sup", species)) #Fix cover
#Fix cover for cam_rot
#Fix cover for Rum_ace and Sag_sag 2018

#Skj_5_1
#Slå sammen Agr_cap_cf og Agr_mer og Agr_cap
#Slå sammen Alc_sp_cf og Alc_sp
#Slå sammen Car_fla og Car_big
#Endre Fes_rub_cf til Fes_rub?
#Endre Hyp_mac til Nid_seedling? <- Var en Seedling i dataarket på hyp_mac
#Slå sammen Sib_pro_cf og Sib_pro
#Slå sammen Ver_alp_cf og Ver_alp
prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Agr_cap_cf", "Agr_mer") & plotID == "Skj_5_1" & year == 2021, "Agr_cap", species))|> #summarise cover
  mutate(species = ifelse(species == "Alc_sp_cf" & plotID == "Skj_5_1" & year == 2022, "Alc_sp", species))|>
  mutate(species = ifelse(species == "Car_fla" & plotID == "Skj_5_1" & year == 2021, "Car_big", species))|> #Summarise cover
  mutate(species = ifelse(species == "Fes_rub_cf_kanskje_Ave_fle" & plotID == "Skj_5_1", "Fes_rub", species)) |>
  mutate(species = ifelse(species == "Hyp_mac" & plotID == "Skj_5_1" & year == 2021, "Nid_seedling", species))|>
  mutate(species = ifelse(species == "Sib_pro_cf" & plotID == "Skj_5_1" & year == 2019, "Sib_pro", species)) |>
  mutate(species = ifelse(species == "Ver_alp_cf" & plotID == "Skj_5_1" & year == 2021, "Ver_alp", species)) #Fix cover


#Skj_5_2
#Slå sammen Car_fla, Car_fla_CF og Car_vag
#Slå sammen Fes_rub og Ave_fle
#Endre Jun_tri_CF til Jun_tri
#Slå sammen Leu_aut_cf og Leo_aut
prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Car_fla", "Car_fla_cf") & plotID == "Skj_5_2" & year %in% c(2018,2021), "Car_vag", species))|>
  mutate(species = ifelse(species == "Fes_rub" & plotID == "Skj_5_2" & year == 2019, "Ave_fle", species))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Skj_5_2" & year == 2021, "Jun_tri", species)) |>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_5_2" & year == 2022, "Leo_aut", species))

#Skj_5_3
#Slå sammen Leu_aut_cf og Leo_aut
prove <- community_clean |>
  mutate(species = ifelse(species == "Leu_aut_cf" & plotID == "Skj_5_3" & year == 2022, "Leo_aut", species))

#Skj_5_4
#Slå sammen Car_pal og Car_pil
prove <- community_clean |>
  mutate(species = ifelse(species == "Car_pal" & plotID == "Skj_5_4" & year == 2022, "Car_pil", species))
#Alc_sp fix cover 2021
#Ave_fle fix cover, give 1% since only found in 2019


#Skj_5_5
#Slå sammen Agr_mer_CF, Agr_mer og Agr_cap
#Slå sammen Hie_sp og Hie_pil
#Gjøre noe med hyp_mac? Stemmer med rådata
#Slå sammen Jun_tri_CF og Jun_tri

prove <- community_clean|>
  mutate(species = ifelse(species %in% c("Agr_mer_CF", "Agr_mer") & plotID == "Skj_5_5" & year %in% c(2018,2021), "Agr_cap", species))|> #Fix cover
  mutate(species = ifelse(species == "Hie_sp" & plotID == "Skj_5_5" & year == 2022, "Hie_pil", species))|>
  mutate(species = ifelse(species == "Jun_tri_CF" & plotID == "Skj_5_5" & year == 2018, "Jun_tri", species)) #Fix cover
  

#Skj_5_6
#Slå sammen Phl_alp og Agr_mer og Agr_cap

prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Phl_alp", "Agr_mer") & plotID = "Skj_5_6", "Agr_cap", species)) #summarise cover

#Skj_6_2
#Slå sammen Car_big_cf og Car_big
#Slå sammen Car_vag_cf og Car_vag
#Slå sammen Hie_pil, Hie_sp og Hie_alp
#Hyp_mac står registrert i rådataen

prove <- community_clean|>
  mutate(species = ifelse(species %in% c("Hie_pil", "Hie_sp") & plotID == "Skj_6_2" & year %in% c(2018,2019,2022), "Hie_alp", species))|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Skj_6_2" & year == 2021, "Car_big", species))|> #Fix cover
  mutate(species = ifelse(species == "Car_vag_cf" & plotID == "Skj_6_2" & year == 2018, "Car_vag", species))

#Skj_6_3
#Slå sammen Car_vag og Car_big
#Slå sammen Cer_cer_cf og Cer_cer
#Slå sammen Epilobium_sp og Epi_ana

prove <- community_clean |>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_6_3" & year == 2021, "Car_big", species))|>
  mutate(species = ifelse(species == "Cer_cer_cf" & plotID == "Skj_6_3" & year == 2018, "Cer_cer", species)) |>
  mutate(species = ifelse(species == "Epilobium_sp" & plotID == "Skj_6_3", "Epi_ana", species))

#Skj_6_4
#Slå sammen Car_big_cf og Car_big

prove <- community_clean|>
  mutate(species = ifelse(species == "Car_big_cf" & plotID == "Skj_6_4" & year == 2021, "Car_big", species))#Fix cover

#Skj_6_6
#Slå sammen Car_Vag_CF og Car_vag
#Slå sammen Sel_sp og Sel_sel

prove <- community_clean|>
  mutate(species = ifelse(species == "Car_vag" & plotID == "Skj_6_6" & year == 2018, "Car_vag", species))|> 
  mutate(species = ifelse(species == "Sel_sp" & plotID == "Skj_6_6" & year == 2019, "Sel_sel", species))
#Fix cover leo_aut 2019

#Skj_7_1
#Slå sammen Car_lep og Car_big
#Endre Car_cap_cf til Car_cap?
#Slå sammen Car_vag_CF og Car_Vag
#Slå sammen Cer_sp og Cer_fon
#Hør med V og J om Jun tri til 2019 Ave_fle, ja
#Slå sammen jamne og Sel_sel

prove <- community_clean |>
  mutate(species = ifelse(species == "Car_lep" & plotID == "Skj_7_1" & year == 2021, "Car_big", species))|>
  mutate(species = ifelse(species == "Car_cap_cf" & plotID == "Skj_7_1" & year == 2019, "Car_cap", species))|>
  mutate(species = ifelse(species == "Car_vag_CF" & plotID == "Skj_7_1" & year == 2019, "Car_vag", species))|> #Fix cover
  mutate(species = ifelse(species == "Cer_sp" & plotID == "Skj_7_1" & year == 2019, "Cer_fon", species)) |> #Fix cover
  mutate(species = ifelse(species == "Jun_tri" & plotID == "Skj_7_1" & year == 2019, "Ave_fle", species))|>
  mutate(species = ifelse(species == "jamne" & plotID == "Skj_7_1" & year == 2019, "Sel_sel", species))

#Skj_7_2
#Slå sammen Agr_cap og Agr_mer_CF til Ant_odo

prove <- community_clean |>
  mutate(species = ifelse(species %in% c("Agr_cap", "Agr_mer_cf") & plotID == "Skj_7_2" & year %in% c(2019,2022), "Ant_odo", species)) #summarise cover
#Nid_seedling fix cover


#Skj_7_5
#Fix cover Nid_seedling


####Gudmeddalen####

#Gud_1_2
prove <- community_clean|>
  mutate(species = ifelse(species == "Car_sp" & plotID == "Gud_1_2" & year == 2019, "Car_big", species))

#Gud_1_3
prove <- community_clean|>
  mutate(species = ifelse(species == "Vio_pal_cf" & plotID == "Gud_1_3", "Vio_pal", species))

#Gud_1_4
prove <- community_clean|>
  mutate(species = ifelse(species == "Agr_cap_cf" & plotID == "Gud_1_4", "Agr_cap", species))|>
  mutate(species = ifelse(species == "Bet_sp" & plotID == "Gud_1_4", "Bet_nan", species))






