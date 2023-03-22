###############################################################################
#####                         Community analysis                         ######
###############################################################################

#_____Libraries used for analysis_____#
library(osfr) #To download the data from OSF we need this library to get the function osf_auth
library(tidyverse)
library(lubridate)
library(dataDownloader)
library(performance)
library(vegan)
library(ggvegan)
library(gridExtra)
library(glmmTMB)
library(performance)


#_____Downloading the cleaned data from OSF_____#

#Use your OSF token to get excess to osf. From here you can download neccesary files
#osf_auth(token = "")#Your personal OSF token

source("R/Community/cleaning_community.R")

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

#Reading inn the data to the script. Needs the metadata to get precipitation information.
#Cleaned community data
community_subplot_download <- read_delim("data\\INCLINE_community_subplot.csv")

#Cleaned cover data
community_cover_download <- read_delim("data\\INCLINE_community_species_cover.csv")

#Meta data
meta_data_download <- read_delim("data\\INCLINE_metadata.csv") 


#_____Making the data ready to be used in the analysis_____#
#For this master thesis we want to investigate species richness and species evenness to see if the community changes when warming and new biotic interactions are implemented in a already relative stable system. The reason for this, is to see how drastic changes we can expect in the alpine due to accelerating climate warming, and to see how fast we can expect them to occur.
#We also want to investigate specifically what effect the different treatments have on the alpine plant community and are using ordinations to investigate patterns. 

#Downloading some meta data and making an environment to have environment data 
env <- meta_data_download|>
  select(plotID, `precipitation_2009-2019`)

#Using the cleaned datasets as base. For this code we need both the subplot information and the cover information. Therefor we need to combine the two datasets community_subplot_download and community_cover_download

community_analysis <- community_subplot_download |>
  left_join(community_cover_download, by = c("site", "plotID", "warming", "treatment", "year", "date", "recorder", "writer", "functional_group", "species"))

#Making the community data ready for analysis with selecting necessary columns, selecting only subplots analysed in 2022. Making a new column called treat that includes treatment and warming in same column. Remove replicas so that we only get one cover for each specie in each plotID.

the_communities <- community_analysis |>
  select(site|plotID|warming|treatment|year|species|presence|cover|subPlot)|> #Select the columns we want to use.
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|> #Filter away the transplants species as these only function as a treatment and not a part of the original community.
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"plot"))|>#filter away the subplots that are in the frame for the data to be comparable with the 2022 data.
  select(-subPlot)|> #Removing the subplot column from the dataframe.
 # mutate(year = as.numeric(year))|>
  group_by(site, year, species, warming, treatment)|>
  mutate(treat = paste0(warming, "_", treatment)) |> #making a new column called treat that combines the warming treatment and the interaction treatment. 
  filter(!treatment %in% c("R"))|> #Removing the removal treatment as this is not relevant for the master. 
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|> #Recoding the names of the treat so its easier to understand what the different treatments are. 
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  mutate(warming = recode(warming, "W" = "Warm"))|>
  mutate(warming = recode(warming, "C" = "Cold"))|>
  mutate(treatment = recode(treatment, "C" = "control"))|>
  mutate(treatment = recode(treatment, "N" = "novel"))|>
  mutate(treatment = recode(treatment, "E" = "extant"))|>
  ungroup()|>
  group_by(plotID, year)|>
  filter(!duplicated(species))|>
  ungroup()


################################################################################
####                                Richness                                ####
################################################################################

#Starting to make a general code that calculate the richness.
#Calculating species richness
species_richness <- community_analysis |>
  select(year|warming|treatment|site|species|presence|plotID|cover|subPlot)|> #Select the columns we want to use.
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|> #Filter away the transplants species as these only function as a treatment and not a part of the original community.
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"plot"))|>#filter away the subplots that are in the frame for the data to be comparable with the 2022 data.
  select(-subPlot)|> #Removing the subplot column from the dataframe.
  # mutate(year = as.numeric(year))|>
  group_by(site, year, species, warming, treatment)|>
  mutate(treat = paste0(warming, "_", treatment)) |> #making a new column called treat that combines the warming treatment and the interaction treatment. 
  filter(!treatment %in% c("R")) |>#Removing the removal treatment as this is not relevant for the master. 
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|> #Recoding the names of the treat so its easier to understand what the different treatments are. 
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  mutate(warming = recode(warming, "W" = "Warm"))|>
  mutate(warming = recode(warming, "C" = "Cold"))|>
  mutate(treatment = recode(treatment, "C" = "control"))|>
  mutate(treatment = recode(treatment, "N" = "novel"))|>
  mutate(treatment = recode(treatment, "E" = "extant"))|>
  ungroup()|>
  group_by(plotID, year)|>
  filter(!duplicated(species))|>
  ungroup() |>
  select(-species)|>
  group_by(year, plotID) |>
  mutate(transplant = ifelse(treatment %in% c("novel", "extant"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "novel", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "extant", "extant", "other"))|>
  mutate(richness = sum(presence, na.rm = TRUE)) |>
  ungroup()|>
  select(-cover) |>
  unique()|>
  group_by(year, treatment) |>
  mutate(treat_richness = mean(richness))|>
  ungroup() |>
  mutate(transplant = ifelse(treatment %in% c("novel", "extant"), "transplant", "control"))|>
  left_join(env, by = "plotID")|>
  rename(precip = 'precipitation_2009-2019')|>
  select(year, warming, treatment, site, presence, plotID, treat, richness, transplant, novel, extant, treat_richness, precip) |>
  unique()


######_________________________ Richness figure _________________________######

mod_richness_boxplot_1 <- species_richness |>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot() + 
  facet_grid(year ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_richness_boxplot_1

mod_richness_boxplot_2022 <- species_richness |>
  filter(year == 2022) |>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot() + 
  facet_grid(~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_richness_boxplot_2022

mod_richness_boxplot_2018 <- species_richness |>
  filter(year == 2018) |>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot() + 
  facet_grid(~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_richness_boxplot_2018

ggsave(plot = mod_richness_boxplot_1, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richnessboxplot_2018_2022.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_richness_boxplot_2022, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richnessboxplot_2022.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_richness_boxplot_2018, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richnessboxplot_2018.png", width = 10, height = 8, dpi = 300)

#___________________________________#
#Need block to make new site: Therefore takes the block info from community_data and combine it by plotID to species_richness
block_cloumn <- community_data|>
  select(plotID, block)|> 
  unique()

species_richness <- species_richness|>
  left_join(env, by = "plotID")|>
  left_join(block_cloumn, by = "plotID")|>
  filter(year == 2022)|>
  mutate(new_site = paste0(substr(site, 1,3), "_", block)) 

species_richness$precip_scaled <- scale(species_richness$precip)

#######__________________________Richness models__________________________#######

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

################################################################################
####           Making community data ready for evenness, ordination and PRC           #### 
################################################################################
#Making the communities ready for PRC and ordination. Removing unnecessary observations, filtering out rare species, making a new column that includes year, site and plotID. Pivot the dataframe wider. This dataframe are being used both for modeling and making the PRC figure. It will also be used when illustrating the ordination. 

community_ordination <- the_communities|>
  group_by(species)|>
  filter(n()>3)|>
  ungroup()|>
  mutate(plotIDyear = paste0(plotID, "_", year))|> #Making a new column that includes plotid and year
  filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|> #Sal_sp removed since its rare, but are not removed when we removes the three most rare species. 
  left_join(env, by = "plotID")|>
  rename(precip = 'precipitation_2009-2019')|>
  select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year, precip)|>
  pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
  column_to_rownames("plotIDyear") |> #Making the column plotIDyear to rownames. 
  mutate(transplant = ifelse(treatment %in% c("novel", "extant"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "novel", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "extant", "extant", "other"))

com_ord_skj <- community_ordination |>
  filter(site == "Skjellingahaugen")

com_ord_lav <- community_ordination|>
  filter(site == "Lavisdalen")

com_ord_gud <- community_ordination|>
  filter(site == "Gudmedalen")

com_ord_ulv <- community_ordination|>
  filter(site == "Ulvehaugen")



###############################################################################
####                               Evenness                                ####
###############################################################################

#Need block to make new site: Therefore takes the block info from community_data and combine it by plotID to species_richness

species_evenness_maybe <- community_analysis |>
  select(year|warming|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "novel", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "extant", "extant", "other"))|>
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

species_evenness_maybe <- species_evenness_maybe|>
  left_join(env, by = "plotID")|>
  rename("precip" = "precipitation_2009-2019") |>
  left_join(block_cloumn, by = "plotID")

#Only this is neccesary?#
evenness <- eventstar(community_ordination[, -c(1:7, 86:88)])

species_evenness <- merge(evenness,community_ordination, by='row.names',all=TRUE)|>
  select(-c(Hstar,Dstar,qstar))|>
  rename(evenness = "Estar" ) |>
  rename(precip = "precip")

species_evenness_2018 <- species_evenness|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(evenness))|>
  ungroup()|>
  filter(year == 2018) 

species_evenness_2019 <- species_evenness|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(evenness))|>
  ungroup()|>
  filter(year == 2019)

species_evenness_2021 <- species_evenness|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(evenness))|>
  ungroup()|>
  filter(year == 2021)

species_evenness_2022 <- species_evenness|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(evenness))|>
  ungroup()|>
  filter(year == 2022)
#Is treat_evenness necessary?

#Standardise the precip variable
species_evenness$precip_scaled <- scale(species_evenness$precip)

######__________________________Evenness figure__________________________######

mod_evenness_boxplot_1 <- species_evenness |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_grid(year ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_1

mod_evenness_boxplot_2018 <- species_evenness_2018 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_2018

mod_evenness_boxplot_2019 <- species_evenness_2019 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_2019

mod_evenness_boxplot_2021 <- species_evenness_2021 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_2021

mod_evenness_boxplot_2022 <- species_evenness_2022 |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot() + 
  facet_wrap(~ as.factor(precip), nrow = 1) +
  theme_bw() +
  geom_jitter(aes(color = warming))

mod_evenness_boxplot_2022

ggsave(plot = mod_evenness_boxplot_1, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Evennessboxplot_2018_2022.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_evenness_boxplot_2022, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Evennessboxplot_2022.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_evenness_boxplot_2018, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Evennessboxplot_2018.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_evenness_boxplot_2019, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Evennessboxplot_2018_2022.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_evenness_boxplot_2021, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Evennessboxplot_2022.png", width = 10, height = 8, dpi = 300)

ggsave(plot = mod_richness_boxplot_2018, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richnessboxplot_2018.png", width = 10, height = 8, dpi = 300)


######__________________________Evenness models__________________________######

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


################################################################################
####                       RDA ordinations to the PRC                       ####
################################################################################

######____________________________ RDA models ____________________________######
#Making RDA models for using in ANOVA test to see if the effect of treatments. 

#Starting with testing the predictors
RDA_site <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ site, data = community_ordination)
RDA_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ warming, data = community_ordination)
RDA_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ precip, data = community_ordination)
RDA_trans <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant, data = community_ordination)
RDA_treat <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment, data = community_ordination)
RDA_time <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ year, data = community_ordination)

#The eigenvalues we get:
#RDA_site = 9.359575
#RDA_warm = 0.02753673
#RDA_precip = 6.710349

#Making a null model to test the different predictors alone
null_RDA <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ 1 , data = community_ordination)

#Taking an ANOVA test on the model and put it up against the null model. All values can be found in an external table
anova(null_RDA, RDA_site) 
anova(null_RDA, RDA_warm) 
anova(null_RDA, RDA_precip)
anova(null_RDA, RDA_trans)
anova(null_RDA, RDA_treat)
anova(null_RDA, RDA_time)

#We choose to go further with the precipitation. The sites are explaining the most variation however the precipitation also explain a lot of the variation found at the sites. We do the same process for each variable. And test if we want to include the interaction or not

#__________Warming and precip__________#

RDA_warm_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ warming + precip, data = community_ordination) #Hør med Ragnhild, Ragnhild sier behold

RDA_warm_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ warming * precip, data = community_ordination)

anova(RDA_precip, RDA_warm_and_precip)
anova(RDA_precip, RDA_warm_over_precip)
anova(RDA_warm_and_precip, RDA_warm_over_precip)

#By adding warming, the interaction with precip have a larger effect than seperated. We therefor goes further with the interaction

#__________Transplant and precip__________#
RDA_trans_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant + precip, data = community_ordination) 

RDA_trans_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * precip, data = community_ordination)

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

RDA_transplant_adding_warm_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant + warming * precip, data = community_ordination)
#Eigenval = 6.737078
RDA_transplant_over_warm_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * warming * precip, data = community_ordination)
#Eigenval = 6.742093
RDA_transplant_over_precip_adding_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * precip + warming, data = community_ordination)

RDA_transplant_over_warm_adding_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ transplant * warming + precip, data = community_ordination)

anova(RDA_warm_over_precip, RDA_transplant_adding_warm_over_precip) #0.019*
anova(RDA_warm_over_precip, RDA_transplant_over_warm_and_precip) #0.026*
anova(RDA_transplant_adding_warm_over_precip, RDA_transplant_over_warm_and_precip) #Ikke ta interaksjonen

anova(RDA_warm_and_precip, RDA_transplant_over_precip_adding_warm)
anova(RDA_warm_and_precip, RDA_transplant_over_warm_adding_precip)
anova(RDA_transplant_over_precip_adding_warm, RDA_transplant_over_warm_and_precip)
anova(RDA_transplant_over_warm_adding_precip, RDA_transplant_over_warm_and_precip)

#__________Treatment and precip__________#
RDA_treat_and_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment + precip, data = community_ordination) 

RDA_treat_over_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * precip, data = community_ordination)

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
RDA_warm_and_precip_added_treatment <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment + warming * precip, data = community_ordination)
RDA_warm_and_precip_and_treatment <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * warming * precip, data = community_ordination)
RDA_warm_and_treatment_adding_precip <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * warming + precip, data = community_ordination)
RDA_precip_and_treatment_adding_warm <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ treatment * precip + warming, data = community_ordination)

anova(RDA_warm_over_precip, RDA_warm_and_precip_added_treatment) #0.008**, Eigenval = 6.737166
anova(RDA_warm_over_precip, RDA_warm_and_precip_and_treatment) #0.001***, Eigenval = 6.747975
anova(RDA_warm_and_precip_added_treatment, RDA_warm_and_precip_and_treatment) #0.011*

anova(RDA_warm_and_precip, RDA_warm_and_treatment_adding_precip)
anova(RDA_warm_and_precip, RDA_precip_and_treatment_adding_warm)
anova(RDA_warm_and_treatment_adding_precip, RDA_warm_and_precip_and_treatment)
anova(RDA_precip_and_treatment_adding_warm, RDA_warm_and_precip_and_treatment)

#
RDA_warm_and_precip_added_extant <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ extant + warming * precip, data = community_ordination)
RDA_warm_and_precip_and_extant <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ extant * warming * precip, data = community_ordination)

anova(RDA_warm_over_precip, RDA_warm_and_precip_added_extant)
anova(RDA_warm_over_precip, RDA_warm_and_precip_and_extant)
anova(RDA_warm_and_precip_added_extant, RDA_warm_and_precip_and_extant)

RDA_warm_and_precip_added_novel <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ novel + warming * precip, data = community_ordination)
RDA_warm_and_precip_and_novel <- rda(sqrt(community_ordination[, -c(1:7, 86:88)]) ~ novel * warming * precip, data = community_ordination)

anova(RDA_warm_and_precip, RDA_warm_and_precip_added_novel)
anova(RDA_warm_and_precip, RDA_warm_and_precip_and_novel)
anova(RDA_warm_and_precip_added_novel, RDA_warm_and_precip_and_novel)

######___________________________ PRC model _____________________________######

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

mod_3 <- prc(community_ordination[, -c(1:7, 86:88)], community_ordination$treatment, community_ordination$year)

autoplot(mod_3, select = logabu > 500)
plot(mod_3, select = logabu > 500)

ggsave(plot = , "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_treat.png", width = 10, height = 8, dpi = 300)

ggsave(plot = PRC_warming, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_warm.png", width = 10, height = 8, dpi = 300)

#____________Site based_____________#

#SKJ
com_ord_skj$warming <- factor(com_ord_skj$warming)
com_ord_skj$year <- factor(com_ord_skj$year)
com_ord_skj$treatment <- factor(com_ord_skj$treatment)
com_ord_skj$treat <- factor(com_ord_skj$treat)

mod_skj <- prc(com_ord_skj[, -c(1:7, 86:88)], com_ord_skj$treatment, com_ord_skj$year)
autoplot(mod_skj)

mod_skj_2 <- prc(com_ord_skj[, -c(1:7, 86:88)], com_ord_skj$warming, com_ord_skj$year)
autoplot(mod_skj_2)

logabu_site <- colSums(com_ord_skj [,-c(1:7, 86:88)])
plot(mod_skj, select = logabu > 500)
plot(mod_skj_2, select = logabu > 500)

#LAV
com_ord_lav$warming <- factor(com_ord_lav$warming)
com_ord_lav$year <- factor(com_ord_lav$year)
com_ord_lav$treatment <- factor(com_ord_lav$treatment)
com_ord_lav$treat <- factor(com_ord_lav$treat)

mod_lav <- prc(com_ord_lav[, -c(1:7, 86:88)], com_ord_lav$treatment, com_ord_lav$year)
mod_lav
autoplot(mod_lav)

mod_lav_2 <- prc(com_ord_lav[, -c(1:7, 86:88)], com_ord_lav$warming, com_ord_lav$year)
mod_lav_2
autoplot(mod_lav_2)

logabu_site <- colSums(com_ord_lav[, -c(1:7, 86:88)])
plot(mod_lav, select = logabu > 1000)
plot(mod_lav_2, select = logabu > 500)

#GUD
com_ord_gud$warming <- factor(com_ord_gud$warming)
com_ord_gud$year <- factor(com_ord_gud$year)
com_ord_gud$treatment <- factor(com_ord_gud$treatment)
com_ord_gud$treat <- factor(com_ord_gud$treat)

mod_gud <- prc(com_ord_gud[, -c(1:7, 86:88)], com_ord_gud$treatment, com_ord_gud$year)
mod_gud
autoplot(mod_gud)

mod_gud_2 <- prc(com_ord_gud[, -c(1:7, 86:88)], com_ord_gud$warming, com_ord_gud$year)
mod_gud_2
autoplot(mod_gud_2)

logabu_site <- colSums(com_ord_gud[, -c(1:7, 86:88)])
plot(mod_gud, select = logabu > 500)
plot(mod_gud_2, select = logabu > 500)


#ULV
com_ord_ulv$warming <- factor(com_ord_ulv$warming)
com_ord_ulv$year <- factor(com_ord_ulv$year)
com_ord_ulv$treatment <- factor(com_ord_ulv$treatment)
com_ord_ulv$treat <- factor(com_ord_ulv$treat)

mod_ulv <- prc(com_ord_ulv[, -c(1:7, 86:88)], com_ord_ulv$treatment, com_ord_ulv$year)
mod_ulv
autoplot(mod_ulv)

mod_ulv_2 <- prc(com_ord_ulv[, -c(1:7, 86:88)], com_ord_ulv$warming, com_ord_ulv$year)
mod_ulv_2
autoplot(mod_ulv_2)

logabu_site <- colSums(com_ord_ulv[, -c(1:7, 86:88)])
plot(mod_ulv, select = logabu > 500)
plot(mod_ulv_2, select = logabu > 500)

#######################
######Ordinations######
#######################

library(ggvegan)

Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

#Starting to investigate the axis length with a DCA

DCA <- decorana(sqrt(com_ord_skj))
DCA
plot(DCA)


screeplot(DCA, bstick = TRUE)

#Making a CCA for each location
pca_skj <- rda(sqrt(com_ord_skj[, -c(1:7, 86:88)]))
pca_skj

pca_fort_skj <- fortify(pca_skj, display = "sites") |>
  bind_cols(com_ord_skj[c(1:7, 86:88)])

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

pca_lav <- rda(sqrt(com_ord_lav[,-c(1:7, 86:88)]))
pca_lav

pca_fort_lav <- fortify(pca_lav, display = "sites") |>
  bind_cols(com_ord_lav[c(1:7, 86:88)])

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

pca_gud <- rda(sqrt(com_ord_gud[,-c(1:7, 86:88)]))
pca_gud

pca_fort_gud <- fortify(pca_gud, display = "sites") |>
  bind_cols(com_ord_gud[c(1:7, 86:88)])

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

pca_ulv <- rda(sqrt(com_ord_ulv[,-c(1:7, 86:88)]))

pca_fort_ulv <- fortify(pca_ulv, display = "sites") |>
  bind_cols(com_ord_gud[c(1:7, 86:88)])

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

ggsave(plot = Ordination_plot_PCA, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_22_03.png", width = 10, height = 8, dpi = 300)


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////////////#
