#################################################
#####          Community analysis          ######
#################################################

#Libraries used for analysis
library(osfr) #To download the data from OSF we need this library to get the function osf_auth
library(tidyverse)
library(lubridate)
library(dataDownloader)
library(lme4)
library(performance)
library(vegan)
library(ggvegan)
library(gridExtra)
library(glmmTMB)
library(glmmTMBTest)

#Use your OSF token to get excess to osf. From here you can download neccesary files
#osf_auth(token = "")#Your personal OSF token

source("R/Community/cleaning_community.R")

#Just an example on how to upload the data from OSF 
#Downloading the cleaned community data from OSF
get_file(node = "zhk3m",
         file = "INCLINE_community_subplot.csv",
         path = "data",
         remote_path = "Community")

#Downloading the cleaned community data from OSF
get_file(node = "zhk3m",
         file = "INCLINE_community_species_cover.csv",
         path = "data",
         remote_path = "Community")

#Meta_data
get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")

#Reding inn the data to the script. Needs the metadata to get precipitation information.
#Cleaned community data
community_subplot_download <- read_delim("data\\INCLINE_community_subplot.csv")

#Cleaned cover data
community_cover_download <- read_delim("data\\INCLINE_community_species_cover.csv")

#Meta data
meta_data_download <- read_delim("data\\INCLINE_metadata.csv") 


#For this master thesis we want to investigate species richness and species evenness to see if the community changes when warming and new biotic interactions are implemented in a already relative stable system. The reason for this, is to see how drastic changes we can expect in the alpine due to accelerating climate warming, and to see how fast we can expact them to occure.
#We also want to investigate specifically what effect the different treatments have on the alpine plant community and are using ordinations to investegate patterns. 

#Downloading some meta data and making an environment
env <- meta_data_download|>
  select(plotID, `precipitation_2009-2019`)

#Using the cleaned datasets as base. For this code we need both the subplot information and the cover information. Therefor we need to combine the two datasets agains, community_clean_subplot_download and community_clean_cover_download

community_analysis <- community_subplot_download |>
  left_join(community_cover_download, by = c("site", "plotID", "warming", "treatment", "year", "date", "recorder", "writer", "functional_group", "species")) #Changes this to the names given them when downloading from the osf


#Making the community data ready for analysis with only subplots analysed in 2022. Remove replicas so that we only get one cover for each specie in each plotID.

the_communities <- community_analysis |>
  select(site|plotID|warming|treatment|year|species|presence|cover|subPlot)|> #Select the columns we want to use.
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|> #Selects away the transplants species as these only function as a treatment and not a part of the original community.
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"plot"))|>#Selects away the subplots that are in the frame for the data to be comparable with the 2022 data.
  select(-subPlot)|> #Removing the subplot column from the dataframe.
  mutate(year = as.numeric(year))|>
  group_by(site, year, species, warming, treatment)|>
  mutate(treat = paste0(warming, "_", treatment)) |> #making a new column called treat that combines the warming treatment and the interaction treatment. 
  filter(!treatment %in% c("R"))|> #Removing the removal treatment as this is not relevant for the master. 
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|> #Recoding the names of the treat so its easier to understand what the different treatments are. 
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  ungroup()|>
  group_by(plotID, year)|>
  filter(!duplicated(species))|>
  ungroup()

#Making the communities ready for ordination. Removing unnecessary observations, filtering out rare species, making a new column that includes year, site and plotID. 
community_ordination <- the_communities|>
  group_by(species)|>
  filter(n()>3)|>
  ungroup()|>
  mutate(plotIDyear = paste0(plotID, "_", year))|> #Making a new column that includes plotid and year
  filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|> #Sal_sp removed since its rare, but are not removed when we removes the three most rare species. 
  left_join(env, by = "plotID")|>
  rename(precip_2009_2019 = 'precipitation_2009-2019')|>
  select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year, precip_2009_2019)|>
  pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
  column_to_rownames("plotIDyear") |> #Making the column plotIDyear to rownames. 
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "N", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "E", "extant", "other"))

###################################################################
############Alt mellom her og neste kan være unødvendig############
###################################################################

#Kanskje greit å bruke hvis jeg skal lage ordinasjoner med å splitte lokasjoner

#Community wider separated in locations. Removing all the unnecessary columns. Only species and the rows left.
com_ord_skj <- community_ordination |>
  filter(site == "Skjellingahaugen")|>
  select(- c(warming, treatment, site, plotID, year, precip_2009_2019))

#Making a wide format that includes all the columns. These need to be used in the ordination with the com_ord. The same done for all locations.
com_skj <- community_ordination|>
  filter(site == "Skjellingahaugen")

# <- the_communities|>
#   filter(n()>3, .by = species)|>
#   filter(site == "Skjellingahaugen")|>
#   mutate(plotIDyear = paste0(plotID, "_", year))|>
#   filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|>
#   select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year, precip_2009_2019)|>
#   pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
#   column_to_rownames("plotIDyear")


com_ord_lav <- community_ordination |>
  filter(site == "Lavisdalen")|>
  select(- c(warming, treatment, site, treat, plotID, year, transplant))

com_lav  <- community_ordination|>
  filter(site == "Lavisdalen")
#<- the_communities|>
#   filter(n()>3, .by = species)|>
#   filter(site == "Lavisdalen")|>
#   mutate(plotIDyear = paste0(plotID, "_", year))|>
#   filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|>
#   select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year)|>
#   pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
#   column_to_rownames("plotIDyear")
  

com_ord_gud <- community_ordination |>
  filter(site == "Gudmedalen")|>
  select(- c(warming, treatment, site, treat, plotID, year, transplant))

com_gud <- community_ordination |>
  filter(site == "Gudmedalen") 


# com_gud <- the_communities|>
#   filter(n()>3, .by = species)|>
#   filter(site == "Gudmedalen")|>
#   mutate(plotIDyear = paste0(plotID, "_", year))|>
#   filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|>
#   select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year)|>
#   pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
#   column_to_rownames("plotIDyear")
  

com_ord_ulv <- community_ordination |>
  filter(site == "Ulvehaugen")|>
  select(- c(warming, treatment, site, treat, plotID, year, transplant))

com_ulv <- community_ordination |>
  filter(site == "Ulvehaugen")

# com_ulv <- the_communities|>
#   filter(n()>3, .by = species)|>
#   filter(site == "Ulvehaugen")|>
#   mutate(plotIDyear = paste0(plotID, "_", year))|>
#   filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|>
#   select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year)|>
#   pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
#   column_to_rownames("plotIDyear")


#Community wider with everything. Combining this with the community_ordination to get all the locations in the same. 
com_ord_wide <- community_ordination|>
  select(-warming, -treatment, -site, -treat, -plotID, -year, -transplant, -precip_2009_2019, -novel, -extant)

com_ord_wide_test <- the_communities |>
  group_by(species)|>
  filter(n()>3)|>
  ungroup()|>
  mutate(plotIDyear = paste0(plotID, "_", year))|> #Making a new column that includes plotid and year
  filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|> #Sal_sp removed since its rare, but are not removed when we removes the three most rare species. 
  left_join(env, by = "plotID")|>
  rename(precip_2009_2019 = 'precipitation_2009-2019')|>
  select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year, precip_2009_2019)|>
  pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "N", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "E", "extant", "other"))

####################################
######Principal response curve######
####################################
#We are going to do analysis on richness, evenness and the RDAs. Starting with making the RDA models. Want to show the it as an PRC. Also want to separate sites from each other and make a PRC for each location. 


##########################################################################
#######                 RDA ordinations to the PRC                 #######
##########################################################################

#Starting with testing the predictors
RDA_site <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ site, data = community_ordination)
RDA_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ warming, data = community_ordination)
RDA_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ precip_2009_2019, data = community_ordination)
RDA_trans <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant, data = community_ordination)
RDA_treat <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment, data = community_ordination)
RDA_time <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ year, data = community_ordination)

#The eigenvalues we gets:
#RDA_site = 9.359575
#RDA_warm = 0.02753673
#RDA_precip = 6.710349

#Making a null model to test the different predictors alone
null <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ 1 , data = community_ordination)

#Taking an ANOVA test on the model and put it up against the null model. All values can be found in an external table
anova(null, RDA_site) 
anova(null, RDA_warm) 
anova(null, RDA_precip)
anova(null,RDA_trans)
anova(null,RDA_treat)
anova(null,RDA_time)

#We choose to go further with the precipitation. The sites are explaining the most variation however the precipitation also explain a lot of the variation found at the sites. We do the same process for each variable. And test if we want to include the interaction or not

#__________Warming and precip__________#

RDA_warm_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ warming + precip_2009_2019, data = community_ordination) #Hør med Ragnhild, Ragnhild sier behold

RDA_warm_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ warming * precip_2009_2019, data = community_ordination)

anova(RDA_precip, RDA_warm_and_precip)
anova(RDA_precip, RDA_warm_over_precip)
anova(RDA_warm_and_precip, RDA_warm_over_precip)

#By adding warming, the interaction with precip have a larger effect than seperated. We therefor goes further with the interaction

#__________Transplant and precip__________#
RDA_trans_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant + precip_2009_2019, data = community_ordination) 

RDA_trans_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * precip_2009_2019, data = community_ordination)

anova(RDA_precip, RDA_trans_and_precip)
anova(RDA_precip, RDA_trans_over_precip)
anova(RDA_trans_and_precip, RDA_trans_over_precip)

#__________Transplant and warm__________#
RDA_trans_and_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant + warming, data = community_ordination) 

RDA_trans_over_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * warming, data = community_ordination)

anova(RDA_trans, RDA_trans_and_warm)
anova(RDA_trans, RDA_trans_over_warm)
anova(RDA_trans_and_warm, RDA_trans_over_warm)

#__________Including transplants__________#

RDA_transplant_adding_warm_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant + warming * precip_2009_2019, data = community_ordination)
#Eigenval = 6.737078
RDA_transplant_over_warm_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * warming * precip_2009_2019, data = community_ordination)
#Eigenval = 6.742093
RDA_transplant_over_precip_adding_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * precip_2009_2019 + warming, data = community_ordination)

RDA_transplant_over_warm_adding_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * warming + precip_2009_2019, data = community_ordination)

anova(RDA_warm_over_precip, RDA_transplant_adding_warm_over_precip) #0.019*
anova(RDA_warm_over_precip, RDA_transplant_over_warm_and_precip) #0.026*
anova(RDA_transplant_adding_warm_over_precip, RDA_transplant_over_warm_and_precip) #Ikke ta interaksjonen

anova(RDA_warm_and_precip, RDA_transplant_over_precip_adding_warm)
anova(RDA_warm_and_precip, RDA_transplant_over_warm_adding_precip)
anova(RDA_transplant_over_precip_adding_warm, RDA_transplant_over_warm_and_precip)
anova(RDA_transplant_over_warm_adding_precip, RDA_transplant_over_warm_and_precip)

#__________Treatment and precip__________#
RDA_treat_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment + precip_2009_2019, data = community_ordination) 

RDA_treat_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * precip_2009_2019, data = community_ordination)

anova(RDA_precip, RDA_treat_and_precip)
anova(RDA_precip, RDA_treat_over_precip)
anova(RDA_treat_and_precip, RDA_treat_over_precip)

#__________Treatment and warm__________#
RDA_treat_and_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment + warming, data = community_ordination) 

RDA_treat_over_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * warming, data = community_ordination)

anova(RDA_treat, RDA_treat_and_warm)
anova(RDA_treat, RDA_treat_over_warm)
anova(RDA_treat_and_warm, RDA_treat_over_warm)

#__________ Including treatment __________#
RDA_warm_and_precip_added_treatment <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment + warming * precip_2009_2019, data = community_ordination)
RDA_warm_and_precip_and_treatment <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * warming * precip_2009_2019, data = community_ordination)
RDA_warm_and_treatment_adding_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * warming + precip_2009_2019, data = community_ordination)
RDA_precip_and_treatment_adding_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * precip_2009_2019 + warming, data = community_ordination)

anova(RDA_warm_over_precip, RDA_warm_and_precip_added_treatment) #0.008**, Eigenval = 6.737166
anova(RDA_warm_over_precip, RDA_warm_and_precip_and_treatment) #0.001***, Eigenval = 6.747975
anova(RDA_warm_and_precip_added_treatment, RDA_warm_and_precip_and_treatment) #0.011*

anova(RDA_warm_and_precip, RDA_warm_and_treatment_adding_precip)
anova(RDA_warm_and_precip, RDA_precip_and_treatment_adding_warm)
anova(RDA_warm_and_treatment_adding_precip, RDA_warm_and_precip_and_treatment)
anova(RDA_precip_and_treatment_adding_warm, RDA_warm_and_precip_and_treatment)

#
RDA_warm_and_precip_added_extant <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ extant + warming * precip_2009_2019, data = community_ordination)
RDA_warm_and_precip_and_extant <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ extant * warming * precip_2009_2019, data = community_ordination)

anova(RDA_warm_over_precip, RDA_warm_and_precip_added_extant)
anova(RDA_warm_over_precip, RDA_warm_and_precip_and_extant)
anova(RDA_warm_and_precip_added_extant, RDA_warm_and_precip_and_extant)

RDA_warm_and_precip_added_novel <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ novel + warming * precip_2009_2019, data = community_ordination)
RDA_warm_and_precip_and_novel <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ novel * warming * precip_2009_2019, data = community_ordination)

anova(RDA_warm_and_precip, RDA_warm_and_precip_added_novel)
anova(RDA_warm_and_precip, RDA_warm_and_precip_and_novel)
anova(RDA_warm_and_precip_added_novel, RDA_warm_and_precip_and_novel)

#_________________PRC___________________#

community_ordination$warming <- factor(community_ordination$warming)
community_ordination$year <- factor(community_ordination$year)
community_ordination$treatment <- factor(community_ordination$treatment)
community_ordination$treat <- factor(community_ordination$treat)

RDA_transplant_over_warm_and_precip_test <- as.matrix(RDA_transplant_over_warm_and_precip)

mod <- prc(community_ordination[, -c(1:7, 86:88)], community_ordination$treatment, community_ordination$year)
mod

mod_2 <- prc(community_ordination[, -c(1:7, 86:88)], community_ordination$warming, community_ordination$year)
mod_2

summary(mod)

logabu <- colSums(community_ordination[, -c(1:7, 86:88)])
plot(mod, select = logabu > 500)
plot(mod_2, select = logabu > 500)

autoplot(mod)

mod_3 <- prc(community_ordination[, -c(1:7, 86:88)], community_ordination$treat, community_ordination$year)

autoplot(mod_3, select = logabu > 500)
plot(mod_3, select = logabu > 500)

#############################################################################
##############                 Richness models                 ##############
#############################################################################

#To make the richness models we need to make a dataframe that includes species richness

#Species richness
species_richness <- community_analysis |>
  select(year|warming|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "N", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "E", "extant", "other"))|>
  mutate(cover = as.numeric(cover))|>
  mutate(year = as.numeric(year))|>
  group_by(site, year, species, warming, treatment)|>
  mutate(treat = paste0(warming, "_", treatment)) |>
  filter(!treatment %in% c("R"))|>
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|>
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  mutate(pres = case_when(
    cover > 0  ~ 1, 
    is.na(cover) ~ 0)) |>
  ungroup()|>
  group_by(plotID, year)|>
  filter(!duplicated(species))|>
  select(-species)|>
  mutate(richness = sum(pres, na.rm = TRUE)) |>
  ungroup()|>
  select(-cover, -pres) |>
  unique()|>
  group_by(site, year, treat) |>
  mutate(treat_richness = mean(richness))|>
  ungroup()|>
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  left_join(env, by = "plotID")|>
  rename(precip_2009_2019 = 'precipitation_2009-2019')|>
  select(year, warming, treatment, site, presence, plotID, treat, richness, transplant, novel, extant, treat_richness, precip_2009_2019)

#############Richness figur####################

mod_richness_boxplot_1 <- species_richness |>
  filter(year == c(2018, 2022))|>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot() + 
  facet_grid(year ~ as.factor(precip_2009_2019)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_richness_boxplot_1

mod_richness_boxplot_2022 <- species_richness |>
  filter(year == 2022) |>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot() + 
  facet_grid(~ as.factor(precip_2009_2019)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_richness_boxplot_2022

mod_richness_boxplot_2018 <- species_richness |>
  filter(year == 2018) |>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot() + 
  facet_grid(~ as.factor(precip_2009_2019)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_richness_boxplot_2018

#___________________________________#
#Need block to make new site: Therefore takes the block info from community_data and combine it by plotID to species_richness
block_cloumn <- community_data|>
  select(plotID, block)|> 
  unique()

species_richness <- species_richness|>
  left_join(env, by = "plotID")|>
  rename("precip" = "precipitation_2009-2019") |>
  left_join(block_cloumn, by = "plotID")|>
  filter(year == 2022)|>
  mutate(new_site = paste0(substr(site, 1,3), "_", block)) |>
  mutate(precip = as.numeric(precip, na.rm = TRUE)) 
  #mutate(overdisp_column = 1:nrow(species_richness))

species_richness$precip_scaled <- scale(species_richness$precip_2009_2019)

#________Making the models________#

#Starting with making a null model so we can test the predictors alone

null_richness <- glmmTMB(richness ~ 1 + (1|site), data = species_richness, family = poisson)

richmod_precip <-  glmmTMB(richness ~ precip_scaled + (1|site), data = species_richness, family = poisson)

anova(null_richness, richmod_precip)

#Also checking warming, site, transplant and treatment separated as well
richmod_warm <- glmmTMB(richness ~ warming + (1|site), data = species_richness, family = poisson)
richmod_site <- glmmTMB(richness ~ site + (1|site), data = species_richness, family = poisson) #Do not think this is right, however its easy to remove if neccesary
richmod_trans <- glmmTMB(richness ~ transplant + (1|site), data = species_richness, family = poisson)
richmod_treat <- glmmTMB(richness ~ treatment + (1|site), data = species_richness, family = poisson)

anova(null_richness, richmod_warm)
anova(null_richness, richmod_site)
anova(null_richness, richmod_trans)
anova(null_richness, richmod_treat)

#__________Testing precip up against warming__________#

richmod_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + (1|site), data = species_richness, family = poisson)
richmod_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + (1|site), data = species_richness, family = poisson)

anova(richmod_precip, richmod_warm_and_precip)
anova(richmod_precip, richmod_warm_over_precip)
anova(richmod_warm_and_precip, richmod_warm_over_precip)

#_________Testing precip up against transplant_________#

richmod_trans_and_precip <-  glmmTMB(richness ~ precip_scaled + transplant + (1|site), data = species_richness, family = poisson)
richmod_trans_over_precip <-  glmmTMB(richness ~ precip_scaled * transplant + (1|site), data = species_richness, family = poisson)

anova(richmod_precip, richmod_trans_and_precip)
anova(richmod_precip, richmod_trans_over_precip)
anova(richmod_trans_and_precip, richmod_trans_over_precip)

#_________Testing transplant up against warming_________#

richmod_trans_and_warm <-  glmmTMB(richness ~ warming + transplant + (1|site), data = species_richness, family = poisson)
richmod_trans_over_warm <-  glmmTMB(richness ~ warming * transplant + (1|site), data = species_richness, family = poisson)

anova(richmod_warm, richmod_trans_and_warm)
anova(richmod_warm, richmod_trans_over_warm)
anova(richmod_trans_and_warm, richmod_trans_over_warm)

#_________Adding transplant__________#

#Making models adding transplant
richmod_trans_and_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + transplant + (1|site), data = species_richness, family = poisson)
richmod_trans_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + transplant + (1|site), data = species_richness, family = poisson)
richmod_trans_over_warm_adding_precip <-  glmmTMB(richness ~ precip_scaled + warming * transplant + (1|site), data = species_richness, family = poisson)
richmod_trans_over_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming * transplant + (1|site), data = species_richness, family = poisson)

anova(richmod_warm_and_precip, richmod_trans_and_warm_and_precip)
anova(richmod_warm_and_precip, richmod_trans_warm_over_precip)
anova(richmod_warm_over_precip, richmod_trans_over_warm_adding_precip)
anova(richmod_warm_over_precip, richmod_trans_over_warm_over_precip)
anova(richmod_trans_and_warm_and_precip, richmod_trans_over_warm_adding_precip)
anova(richmod_trans_and_warm_and_precip, richmod_trans_warm_over_precip)
anova(richmod_trans_over_warm_adding_precip, richmod_trans_over_warm_over_precip)
anova(richmod_trans_warm_over_precip, richmod_trans_over_warm_over_precip)

#_________Testing precip up against treatment_________#

richmod_treat_and_precip <-  glmmTMB(richness ~ precip_scaled + treatment + (1|site), data = species_richness, family = poisson)
richmod_treat_over_precip <-  glmmTMB(richness ~ precip_scaled * treatment + (1|site), data = species_richness, family = poisson)

anova(richmod_precip, richmod_treat_and_precip)
anova(richmod_precip, richmod_treat_over_precip)
anova(richmod_treat_and_precip, richmod_treat_over_precip)

#_________Testing treatment up against warming_________#

richmod_treat_and_warm <-  glmmTMB(richness ~ warming + treatment + (1|site), data = species_richness, family = poisson)
richmod_treat_over_warm <-  glmmTMB(richness ~ warming * treatment + (1|site), data = species_richness, family = poisson)

anova(richmod_warm, richmod_treat_and_warm)
anova(richmod_warm, richmod_treat_over_warm)
anova(richmod_treat_and_warm, richmod_treat_over_warm)

#__________Adding treatment__________#

#Making models adding treatment
richmod_treat_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + treatment + (1|site), data = species_richness, family = poisson)
richmod_treat_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + treatment + (1|site), data = species_richness, family = poisson)
richmod_treat_over_warm_adding_precip <-  glmmTMB(richness ~ precip_scaled + warming * treatment + (1|site), data = species_richness, family = poisson)
richmod_treat_over_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming * treatment + (1|site), data = species_richness, family = poisson)

anova(richmod_warm_and_precip, richmod_treat_warm_and_precip)
anova(richmod_warm_and_precip, richmod_treat_warm_over_precip)
anova(richmod_warm_over_precip, richmod_treat_over_warm_adding_precip)
anova(richmod_warm_over_precip, richmod_treat_over_warm_over_precip)
anova(richmod_treat_warm_and_precip, richmod_treat_over_warm_adding_precip)
anova(richmod_treat_warm_and_precip, richmod_treat_warm_over_precip)
anova(richmod_treat_over_warm_adding_precip, richmod_treat_over_warm_over_precip)
anova(richmod_treat_warm_over_precip, richmod_treat_over_warm_over_precip)


#__________Adding novel and extant__________#

richmod_novel_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + novel + (1|site), data = species_richness, family = poisson)
richmod_treat_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + novel + (1|site), data = species_richness, family = poisson)
richmod_treat_over_warm_adding_precip <-  glmmTMB(richness ~ precip_scaled + warming * novel + (1|site), data = species_richness, family = poisson)
richmod_treat_over_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming * novel + (1|site), data = species_richness, family = poisson)

anova(richmod_warm_and_precip, richmod_novel_warm_and_precip)
anova(richmod_warm_and_precip, richmod_novel_warm_over_precip)
anova(richmod_warm_over_precip, richmod_novel_over_warm_adding_precip)
anova(richmod_warm_over_precip, richmod_novel_over_warm_over_precip)

richmod_extant_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + extant + (1|site), data = species_richness, family = poisson)
richmod_extant_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + extant + (1|site), data = species_richness, family = poisson)
richmod_extant_over_warm_adding_precip <-  glmmTMB(richness ~ precip_scaled + warming * extant + (1|site), data = species_richness, family = poisson)
richmod_extant_over_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming * extant + (1|site), data = species_richness, family = poisson)

anova(richmod_warm_and_precip, richmod_extant_warm_and_precip)
anova(richmod_warm_and_precip, richmod_extant_warm_over_precip)
anova(richmod_warm_over_precip, richmod_extant_over_warm_adding_precip)
anova(richmod_warm_over_precip, richmod_extant_over_warm_over_precip)

##############################################################
##########                 Evenness                 ##########
##############################################################

#Need block to make new site: Therefore takes the block info from community_data and combine it by plotID to species_richness

species_evenness <- community_analysis |>
  select(year|warming|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "N", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "E", "extant", "other"))|>
  mutate(cover = as.numeric(cover))|>
  mutate(year = as.numeric(year))|>
  filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|>
  group_by(site, year, species, warming, treatment)|>
  mutate(treat = paste0(warming, "_", treatment)) |>
  filter(!treatment %in% c("R"))|>
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|>
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  ungroup()|>
  group_by(plotID, year)|>
  filter(!duplicated(species)) |>
  mutate(plotIDyear = paste0(plotID, "_", year))

species_evenness <- species_evenness|>
  left_join(env, by = "plotID")|>
  rename("precip" = "precipitation_2009-2019") |>
  left_join(block_cloumn, by = "plotID")

#Only this is neccesary?#
evenness <- eventstar(com_ord_wide)

comp_data <- merge(evenness,community_ordination, by='row.names',all=TRUE)|>
  select(-c(Hstar,Dstar,qstar))

species_evenness_2018 <- comp_data|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(Estar))|>
  ungroup()|>
  filter(year == 2018) |>
  rename(evenness = "Estar" ) |>
  rename(precip = "precip_2009_2019")

species_evenness_2019 <- comp_data|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(Estar))|>
  ungroup()|>
  filter(year == 2019) |>
  rename(evenness = "Estar" ) |>
  rename(precip = "precip_2009_2019")

species_evenness_2021 <- comp_data|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(Estar))|>
  ungroup()|>
  filter(year == 2021) |>
  rename(evenness = "Estar" ) |>
  rename(precip = "precip_2009_2019")


species_evenness_2022 <- comp_data|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(Estar))|>
  ungroup()|>
  filter(year == 2022) |>
  rename(evenness = "Estar" ) |>
  rename(precip = "precip_2009_2019")

comp_data_2 <- comp_data_test|>
  select("Estar", "plotIDyear", "year", "warming", "treatment", "site", "transplant", "novel", "extant", "treat", "precip_2009_2019")

species_evenness_general <- species_evenness|>
  left_join(comp_data_2, by = c("plotIDyear", "year", "warming", "treatment", "site", "transplant", "novel", "extant", "treat")) |>
  select(-c("species", "cover"))|>
  unique()


#Standardise the precip variable
species_evenness$precip_scaled <- scale(species_evenness$precip)


#########################
#####Evenness models#####
#########################

#_______________________________#
#Starting with comparing the predictors with a null model
null_evenness <- glmmTMB(evenness ~ 1 + (1|site), family = beta_family(link = "logit"), data = species_evenness)

evenmod_precip <- glmmTMB(evenness ~ precip_scaled + (1|site),family = beta_family(link = "logit"), data = species_evenness)
evenmod_warm <- glmmTMB(evenness ~ warming + (1|site), family = beta_family(link = "logit"),data = species_evenness)
evenmod_site <- glmmTMB(evenness ~ site + (1|site),family = beta_family(link = "logit"), data = species_evenness) #Do not think this is right, however its easy to remove if neccesary
evenmod_trans <- glmmTMB(evenness ~ transplant + (1|site), family = beta_family(link = "logit"),data = species_evenness)
evenmod_treat <- glmmTMB(evenness ~ treatment + (1|site), family = beta_family(link = "logit"),data = species_evenness)

anova(null_evenness, evenmod_precip)
anova(null_evenness, evenmod_warm)
anova(null_evenness, evenmod_site)
anova(null_evenness, evenmod_trans)
anova(null_evenness, evenmod_treat)

#__________Testing precip up against warming__________#

evenmod_warm_and_precip <-  glmmTMB(evenness ~ precip_scaled + warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_precip, evenmod_warm_and_precip)
anova(evenmod_precip, evenmod_warm_over_precip)
anova(evenmod_warm_and_precip, evenmod_warm_over_precip)

#_________Testing transplant up against precip_________#
evenmod_trans_and_precip <-  glmmTMB(evenness ~ precip_scaled + transplant + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_trans_over_precip <-  glmmTMB(evenness ~ precip_scaled * transplant + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_precip, evenmod_trans_and_precip)
anova(evenmod_precip, evenmod_trans_over_precip)
anova(evenmod_trans_and_precip, evenmod_trans_over_precip)

#_________Testing transplant up against warming________#
evenmod_warm_and_trans <-  glmmTMB(evenness ~ transplant + warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_warm_over_trans <-  glmmTMB(evenness ~ transplant * warming + (1|site), family = beta_family(link = "logit"),data = species_evenness)

anova(evenmod_warm, evenmod_warm_and_trans)
anova(evenmod_warm, evenmod_warm_over_trans)
anova(evenmod_warm_and_trans, evenmod_warm_over_trans)

#__________Adding transplant__________#
#Making models adding transplant
evenmod_trans_warm_and_precip <- glmmTMB(evenness ~ precip_scaled + warming + transplant + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_trans_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming + transplant + (1|site), family = beta_family(link = "logit"),data = species_evenness)
evenmod_trans_over_warm_adding_precip <-  glmmTMB(evenness ~ precip_scaled + warming * transplant + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_trans_over_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming * transplant + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm_and_precip, evenmod_trans_warm_and_precip)
anova(evenmod_warm_and_precip, evenmod_trans_over_warm_adding_precip)
anova(evenmod_warm_and_precip, evenmod_trans_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_trans_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_trans_over_warm_over_precip)
anova(evenmod_trans_warm_and_precip, evenmod_trans_warm_over_precip)
anova(evenmod_trans_warm_and_precip, evenmod_trans_over_warm_adding_precip)
anova(evenmod_trans_warm_over_precip, evenmod_trans_over_warm_over_precip)
anova(evenmod_trans_over_warm_adding_precip, evenmod_trans_over_warm_over_precip)

#__________Testing precip up against treatment___________#
evenmod_treat_and_precip <-  glmmTMB(evenness ~ precip_scaled + treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_treat_over_precip <-  glmmTMB(evenness ~ precip_scaled * treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_precip, evenmod_treat_and_precip)
anova(evenmod_precip, evenmod_treat_over_precip)
anova(evenmod_treat_and_precip, evenmod_treat_over_precip)

#__________Testing warming up against treatment___________#
evenmod_warm_and_treat <-  glmmTMB(evenness ~ treatment + warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_warm_over_treat <-  glmmTMB(evenness ~ treatment * warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm, evenmod_warm_and_treat)
anova(evenmod_warm, evenmod_warm_over_treat)
anova(evenmod_warm_and_treat, evenmod_warm_over_treat)

#__________Adding treatment__________#
#Making models adding treatment
evenmod_treat_warm_and_precip <- glmmTMB(evenness ~ precip_scaled + warming + treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_treat_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming + treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_treat_over_warm_adding_precip <-  glmmTMB(evenness ~ precip_scaled + warming * treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_treat_over_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming * treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_treat_times_precip_adding_warm <-  glmmTMB(evenness ~ precip_scaled * treatment + warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm_and_precip, evenmod_treat_warm_and_precip)
anova(evenmod_warm_and_precip, evenmod_treat_over_warm_adding_precip)
anova(evenmod_warm_and_precip, evenmod_treat_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_treat_over_warm_adding_precip)
anova(evenmod_warm_over_precip, evenmod_treat_over_warm_over_precip)
anova(evenmod_treat_warm_and_precip, evenmod_treat_over_warm_adding_precip)
anova(evenmod_treat_warm_and_precip, evenmod_treat_warm_over_precip)
anova(evenmod_treat_over_warm_adding_precip, evenmod_treat_over_warm_over_precip)
anova(evenmod_treat_warm_over_precip, evenmod_treat_over_warm_over_precip)

anova(evenmod_treat_warm_and_precip,evenmod_treat_times_precip_adding_warm)
anova(evenmod_treat_warm_and_precip, evenmod_treat_over_warm_over_precip)

#__________Adding novel and extant__________#
evenmod_novel_warm_and_precip <-  glmmTMB(evenness ~ precip_scaled + warming + novel + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_novel_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming + novel + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_novel_over_warm_adding_precip <-  glmmTMB(evenness ~ precip_scaled + warming * novel + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_novel_over_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming * novel + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm_and_precip, evenmod_novel_warm_and_precip)
anova(evenmod_warm_and_precip, evenmod_novel_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_novel_over_warm_adding_precip)
anova(evenmod_warm_over_precip, evenmod_novel_over_warm_over_precip)

evenmod_extant_warm_and_precip <-  glmmTMB(evenness ~ precip_scaled + warming + extant + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_extant_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming + extant + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_extant_over_warm_adding_precip <-  glmmTMB(evenness ~ precip_scaled + warming * extant + (1|site), family = beta_family(link = "logit"), data = species_evenness)
evenmod_extant_over_warm_over_precip <-  glmmTMB(evenness ~ precip_scaled * warming * extant + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm_and_precip, evenmod_extant_warm_and_precip)
anova(evenmod_warm_and_precip, evenmod_extant_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_extant_over_warm_adding_precip)
anova(evenmod_warm_over_precip, evenmod_extant_over_warm_over_precip)

#############Evenness figur####################

mod_evenness_boxplot_1 <- species_evenness |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_1

mod_evenness_boxplot_2 <- species_evenness |>
  ggplot(aes(x = treatment, y = evenness, fill = warming)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() 

mod_evenness_boxplot_2

mod_evenness_boxplot_1_2018 <- species_evenness_2018 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_1_2018

mod_evenness_boxplot_1_2019 <- species_evenness_2019 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_1_2019

mod_evenness_boxplot_1_2021 <- species_evenness_2021 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_1_2021

mod_evenness_boxplot_1_2022 <- species_evenness_2022 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_1_2022
#__________________________________________________#
#Kjør nullmodell på alle prediktorene først. Ta ut tid. Interressert om det har en effekt. 


#We  also expect that along the precipitation gradient that competition increases, porbaly see a larger difference between transplants in general, novel and extant. 
#The next step is to investigate if the different treatments have an effect in totalt, maybe compare them to a null model?




##############################################################################
######                  Tried something stuff down here                  #####
##############################################################################



#species richness per plot without the frame
plot_richness_Plot <- species_richness|>
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |>
  mutate(treat = factor(treat, levels = c("Cold\nControl", "Cold\nExtant", "Cold\nNovel", "Warm\nControl", "Warm\nExtant", "Warm\nNovel"))) |>
  ggplot(aes(x = year, y = treat_richness)) + 
  geom_line() + 
  facet_grid(treat ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(size = 20,face = "bold"))+
  geom_jitter(aes(x = year, y = richness), width = 0.2) +
  ggtitle("Species richness per plot from 2018-2022")+ 
  xlab("Year")+
  ylab("Richness per Plot")

plot_richness_Plot

#Violin plot
base <-  species_richness |>
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen")))|>
  ggplot(aes(x = treat, y = richness)) +
  facet_grid( ~ site)
p_vio <- base + geom_violin(aes(x = treat, y = richness,fill = treatment))
p_vio + stat_summary(fun = mean, geom = "point", shape = 1, size = 1)

####Testing richness ####



#GLM på begge
#Prediktor 1: nedbør (Enten være tallene eller 1234, eller site navnen) Hvis vi bruker tallene spør vi om det er en lineær sammenheng?.
#Prediktor 2: varme eller ikke varme. 
#Prediktor 3:Hvordan behandle transplantsene. Teste om det er en forskjell mellom transplant og kontroll. Så eventuelt teste om det er en forskjell mellom novel og extant.
#Prediktor 4: Tid

#Strukturvariabler: site, block

#Hvis man kjører på alle årene må man ha variablene * year. FOr masteren kan vi egentlig bare kjøre på 2022. Lag modeller kun for 2022 i begynnelsen. 

#Hvis ikke treveisinteraksjonen: prøv fireveisinteraksjon (Kjør en modell per lokalitet) eller. 
#SI til richard at studiet er satt opp som hypotesetesting og derfor trenger jeg hjelp med det!



#ta vekk site i randomeffects

#Richness er en count. poisson

#Evenness  variable binomialt




#############################
#####Evenness#####

#Species evenness



plot_evenness_Plot <- comp_data|>
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) |>
  mutate(treat = factor(treat, levels = c("Cold\nControl", "Cold\nExtant", "Cold\nNovel", "Warm\nControl", "Warm\nExtant", "Warm\nNovel"))) |>
  ggplot(aes(x = year, y = treat_evenness)) + 
  geom_line() + 
  facet_grid(treat ~ site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(size = 20,face = "bold"))+
  geom_jitter(aes(x = year, y = Estar), width = 0.2) +
  ggtitle("Species evenness per plot from 2018-2022")+ 
  xlab("Year")+
  ylab("Evenness per Plot")
plot_evenness_Plot

ggsave(plot = plot_evenness_Plot, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\evenness.png", width = 10, height = 8, dpi = 300)

#species_abundance_skj <- species_evenness|>
#filter(site == "Skjellingahaugen")|>
#filter(treat == "Warm\nNovel")

#species_abundance_lav <- species_evenness|>
#filter(site == "Lavisdalen")

#species_abundance_gud <- species_evenness|>
#filter(site == "Gudmedalen")

#species_abundance_ulv <- species_evenness|>
#filter(site == "Ulvehaugen")

#distribution_skj <- within(species_abundance_skj, 
#                 species <- factor(species, 
#                                    levels = names(sort(table(species), 
#                                                      decreasing=TRUE))))


plot_skj_evenness <- ggplot(distribution_skj, aes(x = species, y = cover, fill = species)) +
  geom_bar(stat = "identity", width = 1) +
  facet_grid(treat ~ year) + 
  labs(title = "Species Distribution in Skjellingahaugen") +
  xlab("Species") +
  ylab("Cover?") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_skj_evenness


distribution_lav <- within(species_abundance_lav, 
                       species <- factor(species, 
                                         levels = names(sort(table(species), 
                                                             decreasing=TRUE))))

plot_lav_evenness <- ggplot(distribution_lav, aes(x = species, y = cover, fill = species)) +
  geom_bar(stat = "identity", width = 1) +
  facet_grid("year") + 
  xlab(year ~ treat) +
  ylab("Evenness per Plot") +
  labs(title = "Species Distribution in Community_2019") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_lav_evenness

distribution_2021 <- within(species_abundance_2021, 
                       species <- factor(species, 
                                         levels = names(sort(table(species), 
                                                             decreasing=TRUE))))

plot_2021_evenness <- ggplot(distribution_2021, aes(x = species, y = cover, fill = species)) +
  geom_bar(stat = "identity", width = 1) +
  xlab("Year") +
  ylab("Evenness per Plot") +
  labs(title = "Species Distribution in Community_2021") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


distribution_2022 <- within(species_abundance_2022, 
                       species <- factor(species, 
                                         levels = names(sort(table(species), 
                                                             decreasing=TRUE))))

plot_2022_evenness <- ggplot(distribution_2022, aes(x = species, y = cover, fill = species)) +
  geom_bar(stat = "identity", width = 1) +
  xlab("Year") +
  ylab("Evenness per Plot") +
  labs(title = "Species Distribution in Community_2022") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

evenness_together <- grid.arrange(plot_2018_evenness, plot_2019_evenness, plot_2021_evenness, plot_2022_evenness, ncol = 2)

#######################
######Ordinations######
#######################


#devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

#Starting to investigate the axis length with a DCA

DCA <- decorana(sqrt(com_ord_skj))
DCA
plot(DCA)


screeplot(DCA, bstick = TRUE)

#Making a CCA for each location
pca_skj <- rda(sqrt(com_ord_skj))
pca_skj

pca_fort_skj <- fortify(pca_skj, display = "sites") |>
  bind_cols(com_skj[1:6])

Ord_plot_time_skj <- pca_fort_skj %>% 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_skj

#Lavisdalen

pca_lav <- rda(sqrt(com_ord_lav))
pca_lav

pca_fort_lav <- fortify(pca_lav, display = "sites") |>
  bind_cols(com_lav[1:6])

Ord_plot_time_lav <- pca_fort_lav %>% 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_lav

#Gudmedalen

pca_gud <- rda(sqrt(com_ord_gud))
pca_gud

pca_fort_gud <- fortify(pca_gud, display = "sites") |>
  bind_cols(com_gud[1:6])

Ord_plot_time_gud <- pca_fort_gud |> 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_gud

#Ulvehaugen

pca_ulv <- rda(sqrt(com_ord_ulv))

pca_fort_ulv <- fortify(pca_ulv, display = "sites") |>
  bind_cols(com_ulv[1:6])

Ord_plot_time_ulv <- pca_fort_ulv |> 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv

Ordination_plot_PCA <- grid.arrange(Ord_plot_time_skj, Ord_plot_time_lav, Ord_plot_time_gud, Ord_plot_time_ulv, ncol = 2)

ggsave(plot = Ordination_plot_PCA, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots.png", width = 10, height = 8, dpi = 300)


#Making a CCA for each location
cca_skj <- cca(sqrt(com_ord_skj))
pca_skj

cca_fort_skj <- fortify(cca_skj, display = "sites") |>
  bind_cols(com_skj[1:6])

Ord_plot_time_skj_cca <- cca_fort_skj %>% 
  ggplot(aes(x = CA1, y = CA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_skj_cca

#Lavisdalen

cca_lav <- cca(sqrt(com_ord_lav))
pca_lav

cca_fort_lav <- fortify(cca_lav, display = "sites") |>
  bind_cols(com_lav[1:6])

Ord_plot_time_lav_cca <- cca_fort_lav %>% 
  ggplot(aes(x = CA1, y = CA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_lav_cca

#Gudmedalen

cca_gud <- cca(sqrt(com_ord_gud))
pca_gud

cca_fort_gud <- fortify(cca_gud, display = "sites") |>
  bind_cols(com_gud[1:6])

Ord_plot_time_gud_cca <- cca_fort_gud |> 
  ggplot(aes(x = CA1, y = CA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_gud_cca

#Ulvehaugen

cca_ulv <- cca(sqrt(com_ord_ulv))

cca_fort_ulv <- fortify(cca_ulv, display = "sites") |>
  bind_cols(com_ulv[1:6])

Ord_plot_time_ulv_cca <- cca_fort_ulv |> 
  ggplot(aes(x = CA1, y = CA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv_cca

Ordination_plot_cca <- grid.arrange(Ord_plot_time_skj_cca, Ord_plot_time_lav_cca, Ord_plot_time_gud_cca, Ord_plot_time_ulv_cca, ncol = 2)

ggsave(plot = Ordination_plot_cca, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_cca.png", width = 10, height = 8, dpi = 300)


#Making a DCA for each location
dca_skj <- decorana(sqrt(com_ord_skj))


dca_fort_skj <- fortify(dca_skj, display = "sites") |>
  bind_cols(com_skj[1:6])

Ord_plot_time_skj_dca <- dca_fort_skj %>% 
  ggplot(aes(x = DCA1, y = DCA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_skj_dca

#Lavisdalen

dca_lav <- decorana(sqrt(com_ord_lav))

dca_fort_lav <- fortify(dca_lav, display = "sites") |>
  bind_cols(com_lav[1:6])

Ord_plot_time_lav_dca <- dca_fort_lav %>% 
  ggplot(aes(x = DCA1, y = DCA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_lav_dca

#Gudmedalen

dca_gud <- dca(sqrt(com_ord_gud))

dca_fort_gud <- fortify(dca_gud, display = "sites") |>
  bind_cols(com_gud[1:6])

Ord_plot_time_gud_dca <- dca_fort_gud |> 
  ggplot(aes(x = DCA1, y = DCA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_gud_dca

#Ulvehaugen

dca_ulv <- dca(sqrt(com_ord_ulv))

dca_fort_ulv <- fortify(dca_ulv, display = "sites") |>
  bind_cols(com_ulv[1:6])

Ord_plot_time_ulv_dca <- dca_fort_ulv |> 
  ggplot(aes(x = DCA1, y = DCA2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv_dca

Ordination_plot_dca <- grid.arrange(Ord_plot_time_skj_dca, Ord_plot_time_lav_dca, Ord_plot_time_gud_dca, Ord_plot_time_ulv_dca, ncol = 2)

ggsave(plot = Ordination_plot_dca, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_dca.png", width = 10, height = 8, dpi = 300)





#Making a NMDS for each location
nmds_skj <- metaMDS(com_ord_skj)


nmds_fort_skj <- fortify(nmds_skj) |>
  filter(Score == "sites")|>
  bind_cols(com_skj[1:6])

Ord_plot_time_skj_nmds <- nmds_fort_skj %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_skj_nmds

#Lavisdalen

nmds_lav <- metaMDS(com_ord_lav)

nmds_fort_lav <- fortify(nmds_lav) |>
  filter(Score == "sites")|>
  bind_cols(com_lav[1:6])

Ord_plot_time_lav_nmds <- nmds_fort_lav %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_lav_nmds

#Gudmedalen

nmds_gud <- metaMDS(com_ord_gud)

nmds_fort_gud <- fortify(nmds_gud) |>
  filter(Score == "sites")|>
  bind_cols(com_gud[1:6])

Ord_plot_time_gud_nmds <- nmds_fort_gud |> 
  ggplot(aes(x = NMDS1, y = NMDS2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_gud_nmds

#Ulvehaugen

nmds_ulv <- metaMDS(com_ord_ulv)

nmds_fort_ulv <- fortify(nmds_ulv) |>
  filter(Score == "sites")|>
  bind_cols(com_ulv[1:6])

Ord_plot_time_ulv_nmds <- nmds_fort_ulv |> 
  ggplot(aes(x = NMDS1, y = NMDS2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv_nmds

Ordination_plot_nmds <- grid.arrange(Ord_plot_time_skj_nmds, Ord_plot_time_lav_nmds, Ord_plot_time_gud_nmds, Ord_plot_time_ulv_nmds, ncol = 2)

ggsave(plot = Ordination_plot_nmds, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_nmds.png", width = 10, height = 8, dpi = 300)










#Separated in to years 
#Community_2018
community_2018 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2018)|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Community_2019
community_2019 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2019)|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Community_2021
community_2021 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2021)|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Community_2022
community_2022 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2022)|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")