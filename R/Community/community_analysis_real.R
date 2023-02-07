source("R/Community/cleaning_community.R")

#We want to investigate species richness, species evenness and diversity. We also want to se the effect from each treatment by doing some ordinations
meta_data_env <- meta_data_download|>
  select(plotID, `precipitation_2009-2019`)

#Community_2018
community_2018 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2018)

community_2018 <- community_2018|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))

community_2018 <- community_2018|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Community_2019
community_2019 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2019)

community_2019 <- community_2019|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))

community_2019 <- community_2019|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Community_2021
community_2021 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2021)

community_2021 <- community_2021|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))

community_2021 <- community_2021|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Community_2022
community_2022 <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  filter(year == 2022)

community_2022 <- community_2022|>
  select(plotID, species, cover)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))

community_2022 <- community_2022|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")

#Community_env
community_env <- community_clean|>
  left_join(meta_data_env, by = "plotID")|>
  filter(!species == "Nid_seedling")|>
  filter(!species == "Unknown")|>
  select(plotID, species, year, presence)|>
  group_by(plotID, species)|>
  summarise(mean_cover = mean(cover))|>
  pivot_wider(names_from = "species", values_from = "mean_cover", values_fill = 0)|>
  column_to_rownames("plotID")


#Species richness
species_richness <- community_clean |>
  select(block|plot|year|OTC|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
  mutate(cover = as.numeric(cover))|>
  mutate(year = as.numeric(year))|>
  group_by(site, year, species, OTC, treatment)|>
  mutate(treat = paste0(OTC, "_", treatment)) |>
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
  mutate(richness = sum(pres, na.rm = TRUE))|>
  ungroup()|>
  select(-cover, -pres) |>
  unique()|>
  group_by(site, year, treat) |>
  mutate(treat_richness = mean(richness))|>
  ungroup()|>
  cbind(forsok)
  
richness <- species_richness|>
    mutate(richness = sum(pres, na.rm = TRUE))
richness1<- species_richness|>
  select(year, plotID, richness)

#Species evenness

species_evenness <- community_clean |>
  select(block|plot|year|OTC|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  #select(-subPlot)|>
  mutate(cover = as.numeric(cover))|>
  mutate(year = as.numeric(year))|>
  group_by(site, year, species, OTC, treatment)|>
  mutate(treat = paste0(OTC, "_", treatment)) |>
  filter(!treatment %in% c("R"))|>
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|>
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  ungroup()|>
  group_by(year, plotID)|>
  mutate(evenness = diversity(data = richness1, richness)/log(specnumber(data = richness1, richness)))|>
  ungroup()|>
  select(-cover, -pres) |>
  unique()|>
  group_by(site, year, treat) |>
  mutate(treat_richness = mean(richness))|>
  ungroup()




H <- diversity(community_2018)
J <- H/log(specnumber(community_2018))
J

ja <- (diversity(community_2019)/log(specnumber(community_2019)))
view(nei)

nei <- (diversity(community_2021)/log(specnumber(community_2021)))

tull <- (diversity(community_2022)/log(specnumber(community_2022)))
view(tull)

forsok <- cbind(ja,nei,tull)

view(forsok)

pivot_wider()

plot_evenness <- species_richness |>
  ggplot(aes(x = year, y = treat_richness))|>
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(size = 20,face = "bold"))+
  geom_jitter(aes(x = plotID, y = richness), width = 0.2) +
  
  

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
ggsave(plot_richness_Plot, filename = "plot_richnessINCLINE.jpg", dpi = 600, width = 17, height = 14, units = "cm")

#Violin plot
base <-  species_richness |>
  mutate(site = factor(site, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen")))|>
  ggplot(aes(x = treat, y = richness)) +
  facet_grid( ~ site)
p_vio <- base + geom_violin(aes(x = treat, y = treat_richness,fill = treatment))
p_vio + stat_summary(fun = mean, geom = "point", shape = 1, size = 1)

ggsave(p_vio + stat_summary(fun = mean, geom = "point", shape = 1, size = 1), filename = "plot_violinINCLINE.jpg", dpi = 600, width = 30, height = 28, units = "cm")



#Ordination
#devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

#Starting to investigate the axis length with a DCA

DCA <- decorana(sqrt(community_2018))
DCA
plot(DCA)


screeplot(DCA, bstick = TRUE)

PCA <- rda(sqrt(community_2022))
PCA
autoplot(PCA, layers = "sites", arrows = FALSE)

pca_fort <- fortify(PCA, display = "sites") %>%
  bind_rows(community_2018,community_2019,community_2021,env)

ggplot(pca_fort, aes(x = PC1, y = PC2, colour = treat, group = site)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == min(year), 1, NA_real_)), show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_size(range = 2) +
  coord_equal()

#Env to ordination plot
env <- community_clean |>
  select(block|plot|year|OTC|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
  mutate(cover = as.numeric(cover))|>
  mutate(year = as.numeric(year))|>
  group_by(site, year, species, OTC, treatment)|>
  mutate(treat = paste0(OTC, "_", treatment)) |>
  filter(!treatment %in% c("R"))|>
  mutate(treat = recode(treat, "W_C" = "Warm\nControl"))|>
  mutate(treat = recode(treat, "W_N" = "Warm\nNovel"))|>
  mutate(treat = recode(treat, "C_C" = "Cold\nControl"))|>
  mutate(treat = recode(treat, "C_N" = "Cold\nNovel"))|>
  mutate(treat = recode(treat, "C_E" = "Cold\nExtant"))|>
  mutate(treat = recode(treat, "W_E" = "Warm\nExtant"))|>
  select(year, site, treat, plotID)

#ggsaves

CA <- cca()
plot(CA)

NMDS <- metaMDS()
NMDS
plot(NMDS)

proMod_PCA <- procrustes(meta_data_env, PCA, symmetric = TRUE)
plot(proMod_PCA)

cca <- cca()
plot(cca)



#Data to PRC
#Need to differ the different treatments from each other within different times
#Need a factor of treatments
#Sampling time (as an unordered factors), used as a covariable

PRC <- community_clean|>
  select



#Hva må vi finne ut av:
# - Se kommentar lenger oppe
# - Plottet uten observasjoner i 2018 på ulvehaguen
#Richness Look at last year minus the first year, and all of the years minus the control? Note to myself