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

dev.off()

cold_warm_palette <- c("Cold" = "#377eb8", "Warm" =  "#e41a1c")

#_________Richness 2018 and 2022__________#
mod_richness_boxplot_2018_and_2022 <- species_richness |>
  filter(year %in% c(2018,2022))|>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot(fill = "#EFEFEF") +
  facet_grid(year ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species richness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_richness_boxplot_2018_and_2022

mod_richness_boxplot_2018_and_2022_test <- species_richness |>
  filter(year %in% c(2018,2022))|>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot(aes(fill = as.factor(year))) +
  facet_wrap( ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species richness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_richness_boxplot_2018_and_2022_test

mod_richness_boxplot_2018_and_2022_test_2 <- species_richness |>
  filter(year %in% c(2018,2022))|>
  ggplot(aes(x = treatment, y = richness)) +
  geom_boxplot(aes(fill = as.factor(year))) +
  facet_grid(warming ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species richness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_richness_boxplot_2018_and_2022_test_2


ggsave(plot = mod_richness_boxplot_2018_and_2022, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richness_boxplot_2018_and_2022.png", width = 9.5, height = 10, dpi = 300)

ggsave(plot = mod_richness_boxplot_2018_and_2022_test, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richness_boxplot_2018_and_2022_test.png", width = 9.5, height = 10, dpi = 300)

ggsave(plot = mod_richness_boxplot_2018_and_2022_test_2, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richness_boxplot_2018_and_2022_test_2.png", width = 9.5, height = 10, dpi = 300)


#_________Richness 2018 and 2022 difference__________#
#Making a new dataframe with all the variables that is needed. It makes a new column with richness from 2022 minus richness from 2018
cold_warm_palette_dots <- c("Cold" = "#66A9FF", "Warm" =  "#FF6666")

species_richness_2022_2018_test <- species_richness_2022_2018|>
  group_by(plotID, warming, treatment, treat, precip)|>
  summarise(diff_rich = richness[year == 2022] - richness[year == 2018])
  
  
mod_richness_boxplot_2018_and_2022_diff <- species_richness_2022_2018_test |>
  ggplot(aes(x = treatment, y = diff_rich)) +
  geom_boxplot(aes(fill=warming))+ #fill = "#EFEFEF") +
  facet_wrap( ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette_dots, breaks = c("Cold", "Warm")) +
  scale_fill_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species richness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_richness_boxplot_2018_and_2022_diff

ggsave(plot = mod_richness_boxplot_2018_and_2022_diff_2, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richness_boxplot_2018_and_2022_diff.png", width = 9.5, height = 10, dpi = 300)

mod_richness_boxplot_2018_and_2022_diff_2 <- species_richness_2022_2018_test |>
  ggplot(aes(x = treatment, y = diff_rich)) +
  geom_boxplot(fill = "#EFEFEF") +
  facet_wrap( ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  scale_fill_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species richness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_richness_boxplot_2018_and_2022_diff_2

ggsave(plot = mod_richness_boxplot_2018_and_2022_diff_2, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\Richness_boxplot_2018_and_2022_diff_2.png", width = 9.5, height = 10, dpi = 300)

#######__________________________Richness models__________________________#######
species_richness_2022_2018 <- species_richness |>
  filter(year %in% c(2018,2022))
species_richness_2022_2018$precip_scaled <- scale(species_richness_2022_2018$precip)

#Starting with making a null model so we can test the predictors alone

null_richness <- glmmTMB(richness ~ 1 + (1|site), data = species_richness_2022_2018, family = poisson)

richmod_precip <-  glmmTMB(richness ~ precip_scaled + (1|site), data = species_richness_2022_2018, family = poisson)

anova(null_richness, richmod_precip)

#Also checking warming, site, transplant and treatment separated as well
richmod_warm <- glmmTMB(richness ~ warming + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_site <- glmmTMB(richness ~ site + (1|site), data = species_richness_2022_2018, family = poisson) #Do not think this is right, however its easy to remove if neccesary
richmod_trans <- glmmTMB(richness ~ transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat <- glmmTMB(richness ~ treatment + (1|site), data = species_richness_2022_2018, family = poisson)

anova(null_richness, richmod_warm)
anova(null_richness, richmod_site)
anova(null_richness, richmod_trans)
anova(null_richness, richmod_treat)

#__________Testing precip up against warming__________#

richmod_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + (1|site), data = species_richness_2022_2018, family = poisson)

anova(richmod_precip, richmod_warm_and_precip)
anova(richmod_precip, richmod_warm_over_precip)
anova(richmod_warm_and_precip, richmod_warm_over_precip)

#_________Testing precip up against transplant_________#

richmod_trans_and_precip <-  glmmTMB(richness ~ precip_scaled + transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_trans_over_precip <-  glmmTMB(richness ~ precip_scaled * transplant + (1|site), data = species_richness_2022_2018, family = poisson)

anova(richmod_precip, richmod_trans_and_precip)
anova(richmod_precip, richmod_trans_over_precip)
anova(richmod_trans_and_precip, richmod_trans_over_precip)

#_________Testing transplant up against warming_________#

richmod_trans_and_warm <-  glmmTMB(richness ~ warming + transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_trans_over_warm <-  glmmTMB(richness ~ warming * transplant + (1|site), data = species_richness_2022_2018, family = poisson)

anova(richmod_warm, richmod_trans_and_warm)
anova(richmod_warm, richmod_trans_over_warm)
anova(richmod_trans_and_warm, richmod_trans_over_warm)

#_________Adding transplant__________#

#Making models adding transplant
richmod_trans_and_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_trans_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_trans_over_warm_adding_precip <-  glmmTMB(richness ~ precip_scaled + warming * transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_trans_over_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming * transplant + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_trans_over_precip_adding_warm <-  glmmTMB(richness ~ precip_scaled * transplant + warming + (1|site), data = species_richness_2022_2018, family = poisson)


anova(richmod_warm_and_precip, richmod_trans_and_warm_and_precip)
anova(richmod_warm_and_precip, richmod_trans_warm_over_precip)
anova(richmod_warm_over_precip, richmod_trans_over_warm_adding_precip)
anova(richmod_warm_over_precip, richmod_trans_over_warm_over_precip)
anova(richmod_trans_and_warm_and_precip, richmod_trans_over_warm_adding_precip)
anova(richmod_trans_and_warm_and_precip, richmod_trans_warm_over_precip)
anova(richmod_trans_over_warm_adding_precip, richmod_trans_over_warm_over_precip)
anova(richmod_trans_warm_over_precip, richmod_trans_over_warm_over_precip)
anova(richmod_trans_and_warm_and_precip, richmod_trans_over_precip_adding_warm)
anova(richmod_trans_and_warm_and_precip, richmod_trans_over_warm_over_precip)

#_________Testing precip up against treatment_________#

richmod_treat_and_precip <-  glmmTMB(richness ~ precip_scaled + treatment + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_over_precip <-  glmmTMB(richness ~ precip_scaled * treatment + (1|site), data = species_richness_2022_2018, family = poisson)

anova(richmod_precip, richmod_treat_and_precip)
anova(richmod_precip, richmod_treat_over_precip)
anova(richmod_treat_and_precip, richmod_treat_over_precip)

#_________Testing treatment up against warming_________#

richmod_treat_and_warm <-  glmmTMB(richness ~ warming + treatment + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_over_warm <-  glmmTMB(richness ~ warming * treatment + (1|site), data = species_richness_2022_2018, family = poisson)

anova(richmod_warm, richmod_treat_and_warm)
anova(richmod_warm, richmod_treat_over_warm)
anova(richmod_treat_and_warm, richmod_treat_over_warm)

#__________Adding treatment__________#

#Making models adding treatment
richmod_treat_warm_and_precip <-  glmmTMB(richness ~ precip_scaled + warming + treatment + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming + treatment + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_over_warm_adding_precip <-  glmmTMB(richness ~ precip_scaled + warming * treatment + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_over_warm_over_precip <-  glmmTMB(richness ~ precip_scaled * warming * treatment + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_over_precip_adding_warm <-  glmmTMB(richness ~ precip_scaled * treatment + warming + (1|site), data = species_richness_2022_2018, family = poisson)
richmod_treat_over_precip_over_warm <-  glmmTMB(richness ~ precip_scaled * treatment * warming + (1|site), data = species_richness_2022_2018, family = poisson)

anova(richmod_warm_and_precip, richmod_treat_warm_and_precip)
anova(richmod_warm_and_precip, richmod_treat_warm_over_precip)
anova(richmod_warm_over_precip, richmod_treat_over_warm_adding_precip)
anova(richmod_warm_over_precip, richmod_treat_over_warm_over_precip)
anova(richmod_treat_warm_and_precip, richmod_treat_over_warm_adding_precip)
anova(richmod_treat_warm_and_precip, richmod_treat_warm_over_precip)
anova(richmod_treat_over_warm_adding_precip, richmod_treat_over_warm_over_precip)
anova(richmod_treat_warm_over_precip, richmod_treat_over_warm_over_precip)
anova(richmod_treat_warm_and_precip, richmod_treat_over_precip_adding_warm)
anova(richmod_treat_warm_and_precip, richmod_treat_over_precip_over_warm)

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

#Making evenness with a function called eventastar
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

species_evenness_2022 <- species_evenness|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(evenness))|>
  ungroup()|>
  filter(year == 2022)

species_evenness_2018_2022 <- species_evenness|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(evenness))|>
  ungroup()|>
  filter(year %in% c(2018, 2022))
#Is treat_evenness necessary?

#Standardise the precip variable
species_evenness$precip_scaled <- scale(species_evenness$precip)

######__________________________Evenness figure__________________________######

#__________Evenness from 2018 to 2022____________#
mod_evenness_boxplot_2018_to_2022 <- species_evenness |>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot(fill = "#EFEFEF") +
  facet_grid(year ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species evenness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_evenness_boxplot_2018_to_2022

ggsave(plot = mod_evenness_boxplot_2018_to_2022, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\evennessboxplot_2018_to_2022.png", width = 9.5, height = 10, dpi = 300)


#_________evenness 2018 and 2022__________#
mod_evenness_boxplot_2018_2022 <- species_evenness |>
  filter(year %in% c(2018,2022))|>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot(fill = "#EFEFEF") +
  facet_grid(year ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species evenness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

mod_evenness_boxplot_2018_2022

ggsave(plot = mod_evenness_boxplot_2018_2022, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\evenness_boxplot_2018_2022.png", width = 9.5, height = 10, dpi = 300)

#__________evenness 2022___________#

mod_evenness_boxplot_2022 <- species_evenness |>
  filter(year == 2022)|>
  ggplot(aes(x = treatment, y = evenness)) +
  geom_boxplot(fill = "#EFEFEF") +
  facet_grid(year ~ as.factor(precip)) +
  theme_bw() +
  geom_jitter(aes(color = warming)) +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = cold_warm_palette, breaks = c("Cold", "Warm")) +
  labs(x = "Treatment", y = "Species evenness") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))


mod_evenness_boxplot_2022


ggsave(plot = mod_evenness_boxplot_2022, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\evenness_boxplot_2022.png", width = 9.5, height = 10, dpi = 300)



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
evenmod_trans_over_precip_adding_warm <-  glmmTMB(evenness ~ precip_scaled * transplant + warming + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm_and_precip, evenmod_trans_warm_and_precip)
anova(evenmod_warm_and_precip, evenmod_trans_over_warm_adding_precip)
anova(evenmod_warm_and_precip, evenmod_trans_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_trans_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_trans_over_warm_over_precip)
anova(evenmod_trans_warm_and_precip, evenmod_trans_warm_over_precip)
anova(evenmod_trans_warm_and_precip, evenmod_trans_over_warm_adding_precip)
anova(evenmod_trans_warm_over_precip, evenmod_trans_over_warm_over_precip)
anova(evenmod_trans_over_warm_adding_precip, evenmod_trans_over_warm_over_precip)
anova(evenmod_trans_warm_and_precip, evenmod_trans_over_warm_adding_precip)
anova(evenmod_trans_warm_and_precip, evenmod_trans_over_warm_over_precip)

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

evenmod_optimal<- glmmTMB(evenness ~ precip_scaled + warming + treatment + warming*treatment + (1|site), family = beta_family(link = "logit"), data = species_evenness)

anova(evenmod_warm_and_precip, evenmod_treat_warm_and_precip)
anova(evenmod_warm_and_precip, evenmod_treat_over_warm_adding_precip)
anova(evenmod_warm_and_precip, evenmod_treat_warm_over_precip)
anova(evenmod_warm_over_precip, evenmod_treat_over_warm_adding_precip)
anova(evenmod_warm_over_precip, evenmod_treat_over_warm_over_precip)
anova(evenmod_treat_warm_and_precip, evenmod_treat_over_warm_adding_precip)
anova(evenmod_treat_warm_and_precip, evenmod_treat_warm_over_precip)
anova(evenmod_treat_over_warm_adding_precip, evenmod_treat_over_warm_over_precip)
anova(evenmod_treat_warm_over_precip, evenmod_treat_over_warm_over_precip)
anova(evenmod_treat_warm_and_precip, evenmod_treat_over_warm_adding_precip)

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

######_____________________ PRC models and figures _______________________######

#Making a new column that includes all variables
PRC_com_ord <- community_ordination |>
  mutate(variables = paste0(treat, " ", site))

#Making the variables to factors
PRC_com_ord$treat <- factor(PRC_com_ord$treat)
PRC_com_ord$year <- factor(PRC_com_ord$year)
PRC_com_ord$site <- factor(PRC_com_ord$site)
PRC_com_ord$variables <-factor(PRC_com_ord$variables)

#The models with all variables
mod_treat <- prc(PRC_com_ord[,-c(1:7, 86:89)], PRC_com_ord$variables, PRC_com_ord$year)

logabu_var <- colSums(PRC_com_ord[, -c(1:7, 86:89)])

mod_treat_plot <- autoplot(mod_treat, select = logabu_var >100, xlab = "Year", ylab = "Effect?", )

mod_treat_plot

# Save prc plot
ggsave(plot = mod_treat_plot, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_all.png", width = 10, height = 6, dpi = 300)

#____Making the PRC dataframe site specific_____#

PRC_skj <- PRC_com_ord |>
  filter(site == "Skjellingahaugen")

PRC_lav <- PRC_com_ord |>
  filter(site == "Lavisdalen")

PRC_gud <- PRC_com_ord |>
  filter(site == "Gudmedalen")

PRC_ulv <- PRC_com_ord |>
  filter(site == "Ulvehaugen")

#_______________Skjellingahaugen_______________#
PRC_skj$treat <- factor(PRC_skj$treat)
PRC_skj$year <- factor(PRC_skj$year)
PRC_skj$site <- factor(PRC_skj$site)
PRC_skj$variables <-factor(PRC_skj$variables)

mod_prc_skj <- prc(PRC_skj[, -c(1:7, 86:89)], PRC_skj$treat, PRC_skj$year)
logabu_skj <- colSums(PRC_skj[, -c(1:7, 86:89)])

mod_prc_skj_plot <- autoplot(mod_prc_skj, select = logabu_skj >100, xlab = "year", ylab = "Effect")

mod_prc_skj_plot

# Save prc plot
ggsave(plot = mod_prc_skj_plot, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_skj.png", width = 10, height = 6, dpi = 300)

#_______________Lavisdalen_______________#

PRC_lav$treat <- factor(PRC_lav$treat)
PRC_lav$year <- factor(PRC_lav$year)
PRC_lav$site <- factor(PRC_lav$site)
PRC_lav$variables <-factor(PRC_lav$variables)

mod_prc_lav <- prc(PRC_lav[, -c(1:7, 86:89)], PRC_lav$treat, PRC_lav$year)
logabu_lav <- colSums(PRC_lav[, -c(1:7, 86:89)])

mod_prc_lav_plot <- autoplot(mod_prc_lav, select = logabu_lav >100, xlab = "year", ylab = "Effect")

mod_prc_lav_plot

# Save prc plot
ggsave(plot = mod_prc_lav_plot, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_lav.png", width = 10, height = 6, dpi = 300)

#_______________Gudmedalen_______________#

PRC_gud$treat <- factor(PRC_gud$treat)
PRC_gud$year <- factor(PRC_gud$year)
PRC_gud$site <- factor(PRC_gud$site)
PRC_gud$variables <-factor(PRC_gud$variables)

mod_prc_gud <- prc(PRC_gud[, -c(1:7, 86:89)], PRC_gud$treat, PRC_gud$year)
logabu_gud <- colSums(PRC_gud[, -c(1:7, 86:89)])

mod_prc_gud_plot <- autoplot(mod_prc_gud, select = logabu_gud >100, xlab = "year", ylab = "Effect")

mod_prc_gud_plot

# Save prc plot
ggsave(plot = mod_prc_gud_plot, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_gud.png", width = 10, height = 6, dpi = 300)

#_______________Ulvehaugen_______________#

PRC_ulv$treat <- factor(PRC_ulv$treat)
PRC_ulv$year <- factor(PRC_ulv$year)
PRC_ulv$site <- factor(PRC_ulv$site)
PRC_ulv$variables <-factor(PRC_ulv$variables)

mod_prc_ulv <- prc(PRC_ulv[, -c(1:7, 86:89)], PRC_ulv$treat, PRC_ulv$year)
logabu_ulv <- colSums(PRC_ulv[, -c(1:7, 86:89)])

mod_prc_ulv_plot <- autoplot(mod_prc_ulv, select = logabu_ulv >100, xlab = "year", ylab = "Effect")

mod_prc_ulv_plot

# Save prc plot
ggsave(plot = mod_prc_ulv_plot, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\PRC_ulv.png", width = 10, height = 6, dpi = 300)


################################################################################
####                               Ordination                               ####
################################################################################

Treatment_palette <- c("#FFE970","#CAB22D","#42F4A1","#2EC47E", "#FF7EF7", "#CB65C5")
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

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
  scale_color_manual(values = Treatment_palette) +
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
  scale_color_manual(values = Treatment_palette) +
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
  scale_color_manual(values = Treatment_palette) +
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
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv

Ordination_plot_PCA <- grid.arrange(Ord_plot_time_skj, Ord_plot_time_lav, Ord_plot_time_gud, Ord_plot_time_ulv, ncol = 2)

ggsave(plot = Ordination_plot_PCA, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_23_03.png", width = 10, height = 8, dpi = 300)

#________2018 and 2022________#
com_ord_skj_2018_2022 <- com_ord_skj |>
  filter(year %in% c(2018, 2022))
com_ord_lav_2018_2022 <- com_ord_lav |>
  filter(year %in% c(2018, 2022))
com_ord_gud_2018_2022 <- com_ord_gud |>
  filter(year %in% c(2018, 2022))
com_ord_ulv_2018_2022 <- com_ord_ulv |>
  filter(year %in% c(2018, 2022))

#Making a CCA for each location
pca_skj_2 <- rda(sqrt(com_ord_skj_2018_2022[, -c(1:7, 86:88)]))
pca_skj_2

pca_fort_skj_2 <- fortify(pca_skj_2, display = "sites") |>
  bind_cols(com_ord_skj_2018_2022[c(1:7, 86:88)])

Ord_plot_time_skj_2 <- pca_fort_skj_2 |>
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_skj_2

#Lavisdalen

pca_lav_2<- rda(sqrt(com_ord_lav_2018_2022[,-c(1:7, 86:88)]))
pca_lav_2

pca_fort_lav_2 <- fortify(pca_lav_2, display = "sites") |>
  bind_cols(com_ord_lav_2018_2022[c(1:7, 86:88)])

Ord_plot_time_lav_2 <- pca_fort_lav_2 |>
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_lav_2

#Gudmedalen

pca_gud_2 <- rda(sqrt(com_ord_gud_2018_2022[,-c(1:7, 86:88)]))
pca_gud_2

pca_fort_gud_2 <- fortify(pca_gud_2, display = "sites") |>
  bind_cols(com_ord_gud_2018_2022[c(1:7, 86:88)])

Ord_plot_time_gud_2 <- pca_fort_gud_2 |> 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_gud_2

#Ulvehaugen

pca_ulv_2 <- rda(sqrt(com_ord_ulv_2018_2022[,-c(1:7, 86:88)]))

pca_fort_ulv_2 <- fortify(pca_ulv_2, display = "sites") |>
  bind_cols(com_ord_gud_2018_2022[c(1:7, 86:88)])

Ord_plot_time_ulv_2 <- pca_fort_ulv_2 |> 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv_2

Ordination_plot_PCA_2 <- grid.arrange(Ord_plot_time_skj_2, Ord_plot_time_lav_2, Ord_plot_time_gud_2, Ord_plot_time_ulv_2, ncol = 2)

ggsave(plot = Ordination_plot_PCA_2, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_23_03_2.png", width = 10, height = 8, dpi = 300)

#________2022________# #Funker ikke
com_ord_skj_2022 <- com_ord_skj |>
  filter(year == 2022)
com_ord_lav_2022 <- com_ord_lav |>
  filter(year == 2022)
com_ord_gud_2022 <- com_ord_gud |>
  filter(year == 2022)
com_ord_ulv_2022 <- com_ord_ulv |>
  filter(year == 2022)

#Making a CCA for each location
pca_skj_2022 <- rda(sqrt(com_ord_skj_2022[, -c(1:7, 86:88)]))
pca_skj_2022

pca_fort_skj_2022 <- fortify(pca_skj_2022, display = "sites") |>
  bind_cols(com_ord_skj_2022[c(1:7, 86:88)])

Ord_plot_time_skj_2022 <- pca_fort_skj_2022 |>
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_skj_2022

#Lavisdalen

pca_lav_2<- rda(sqrt(com_ord_lav_2018_2022[,-c(1:7, 86:88)]))
pca_lav_2

pca_fort_lav_2 <- fortify(pca_lav_2, display = "sites") |>
  bind_cols(com_ord_lav_2018_2022[c(1:7, 86:88)])

Ord_plot_time_lav_2 <- pca_fort_lav_2 |>
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_lav_2

#Gudmedalen

pca_gud_2 <- rda(sqrt(com_ord_gud_2018_2022[,-c(1:7, 86:88)]))
pca_gud_2

pca_fort_gud_2 <- fortify(pca_gud_2, display = "sites") |>
  bind_cols(com_ord_gud_2018_2022[c(1:7, 86:88)])

Ord_plot_time_gud_2 <- pca_fort_gud_2 |> 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_gud_2

#Ulvehaugen

pca_ulv_2 <- rda(sqrt(com_ord_ulv_2018_2022[,-c(1:7, 86:88)]))

pca_fort_ulv_2 <- fortify(pca_ulv_2, display = "sites") |>
  bind_cols(com_ord_gud_2018_2022[c(1:7, 86:88)])

Ord_plot_time_ulv_2 <- pca_fort_ulv_2 |> 
  ggplot(aes(x = PC1, y = PC2, colour = treat, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_color_manual(values = Treatment_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 

Ord_plot_time_ulv_2

Ordination_plot_PCA_2 <- grid.arrange(Ord_plot_time_skj_2, Ord_plot_time_lav_2, Ord_plot_time_gud_2, Ord_plot_time_ulv_2, ncol = 2)

ggsave(plot = Ordination_plot_PCA_2, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\four_plots_23_03_2.png", width = 10, height = 8, dpi = 300)


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////////////#
