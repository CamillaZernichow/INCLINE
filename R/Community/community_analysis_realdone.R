source(here::here("R/Community", "cleaning_community.R")) #Finn ut av

#############################
#####Community analysis######
#############################

#For this master thesis we want to investigate species richness and species evenness to see if the community changes when warming and new biotic interactions are implemented in a already relative stable system. The reason for this, is to see how drastic changes we can expect in the alpine due to accelerating climate warming, and to see how fast we can expact them to occure.
#We also want to investigate specifically what effect the different treatments have on the alpine plant community and are using ordinations to investegate patterns. 

#Downloading some meta data and making an enivronment
env <- meta_data_download|>
  select(plotID, `precipitation_2009-2019`)

#Using the cleaned dataset as 
#Community ready for ordination with only subplots analysed in 2022. Remove replicas so that we only get one cover for each specie in each plotID.
the_communities <- community_clean |>
  select(block|plot|year|warming|treatment|site|species|presence|plotID|cover|subPlot)|> #Select the columns we want to use.
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|> #Selects away the transplants species as these only function as a treatment and not a part of the original community.
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>#Selects away the subplots that are in the frame for the data to be comparable with the 2022 data.
  select(-subPlot)|> #Removing the subplot column from the dataframe.
  mutate(cover = as.numeric(cover))|>
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
  filter(n()>3, .by = species)|>
  mutate(plotIDyear = paste0(plotID, "_", year))|> #Making a new column that includes plotid and year
  filter(!species %in% c("Nid_seedling", "Unknown", "Fern", "Nid_juvenile", "Sal_sp"))|> #Sal_sp removed since its rare, but are not removed when we removes the tre most rare species. 
  left_join(env, by = "plotID")|>
  rename(precip_2009_2019 = 'precipitation_2009-2019')|>
  select(plotIDyear, species, warming, treatment, site, treat, cover, plotID, year, precip_2009_2019)|>
  pivot_wider(names_from = "species", values_from = "cover", values_fill = 0)|>
  column_to_rownames("plotIDyear") |> #Making the column plotIDyear to rownames. 
  mutate(transplant = ifelse(treatment %in% c("N", "E"), "transplant", "control"))|>
  mutate(novel = ifelse(treatment == "N", "novel", "other"))|>
  mutate(extant = ifelse(treatment == "E", "extant", "other"))
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


#Community wider with everything. Combinging this with the community_ordination to get all the locations in the same. 
com_ord_wide <- community_ordination|>
  select(-warming, -treatment, -site, -treat, -plotID, -year, -transplant)

####################################
######Principal response curve######
####################################
#Starting to make a pv table

df <- data.frame("Testing_of_effects" = c("Site", "Time", "Warming", "Site and time","Site over time", "Warming over time", "Warming over site and time", "Adding plants with novel traits", "Adding plants with extant traits", "Precipitaiton gradient with novel", "Precipitation gradient with extant", "Differnet trends among sites", "Total explained by site and warming over time", "Total explained by warming and transplant over time"),
                "Hypothesis" = c("", "","", "", "", "", "", "", "", "", "", ""),
                "Variables" = c("S", "T", "S*T", "S+T","W", "W*T", "W*S+T", "W*N*T", "W*E*T", "W*N*T*P", "W*E*T*P", "S*W*T", "S*W*T", "W*Tr*T"),
                "Covariables" = c("","","", "", "", "", "W*T+S+T", "W*N*T+W*E*T+S+T", "W*E*T+W*N*T+S+T","W*T+S*T", "-" , ""),
                "Variance" = c("9.359", "0.24", "0.28", "9,360", "9.375", "", "", "", "", "", ""),
                "P(999)" = c("0.001***", "0.011*", "0.002**", "0.001***", "0.004**",  "", "", "", "", "", ""))
df

########################################
#######RDA ordinations to the PRC#######
########################################
#What do we want to investigate? Are there a difference between sites or time?
RDA_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ site, data = community_ordination)
RDA_time <- rda(sqrt(community_ordination[,-c(1:7, 87:89)]) ~ year, data = community_ordination)

#To investigate the question, we set up a null model to test the models alone before testing adding other variables
null <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ 1 , data = community_ordination)

#Testing site and time alone up against the null model
anova(null, RDA_site)
anova(null, RDA_time)
anova(null, RDA_warm)

#Site is where the biggest variance exist. Therefor will site be used further to test other differences. 
RDA_site_and_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ site + year, data = community_ordination) #use
RDA_site_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ site * year, data = community_ordination)

#Testing site and time, and site over time
anova(RDA_site, RDA_site_and_time) #The p value is significant, however there is little variance between site and time compared to site alone. 
anova(RDA_site, RDA_site_over_time) #Significant p value.
anova(RDA_site_and_time, RDA_site_over_time) #The p value is not significant and varies between 0.06 and 0.05. However the variance increases more than with site + time

#The next step is to include warming to see if warming have an effect in the sites over time.
RDA_warm_adding_time_and_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming + site + year, data = community_ordination) #This
RDA_warm_adding_time_over_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming + site * year, data = community_ordination)
RDA_warm_over_time_and_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * site + year, data = community_ordination) #This
RDA_warm_over_time_and_over_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * site * year, data = community_ordination)

anova(RDA_site_and_time, RDA_warm_adding_time_and_site) #P value = 0.001 eigenval = 9.36
anova(RDA_site_over_time, RDA_warm_adding_time_over_site) #P value = 0.001 eigenval = 9.376
anova(RDA_site_and_time, RDA_warm_over_time_and_site) #P value = 0.003 eigenval = 9.36
anova(RDA_site_over_time, RDA_warm_over_time_and_over_site) #P value = 0.001 eigenval = 9.43

#When investiagting the variance between the interaction between site and year, and adding site to year, the variance is so little that its probably not necessary to use the interaction. However when testing warming with the site year interaction, this variance is a little bit higher than the others. Therefor more of the variance might be described with the site year interaction than first thought. 
#The next step in my head would therefore be to test the interaction between warming time and site with other variables. 

#First with transplant
RDA_transplant_adding_warm_and_time_and_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ transplant + warming + site + year, data = community_ordination)
#Eigenval = 9.369
RDA_transplant_adding_warm_over_time_and_over_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ transplant + warming * site * year, data = community_ordination)
#Eigenval = 9.437

#Other comment from Ragnhild and Vigdis saying that I can use site + year. Therefor we use this further
anova(RDA_warm_adding_time_and_site, RDA_transplant_adding_warm_and_time_and_site) #p_value = 0.004
anova(RDA_warm_adding_time_and_site, RDA_transplant_adding_warm_over_time_and_over_site) #p_value = 0.001

#So there might be an effect with the transplant compared to the control. Therefor we test if its novel og extant that are giving the most effect with warming
RDA_warm_novel_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ novel + warming * site * year, data = community_ordination)
RDA_warm_extant_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ extant + warming * site * year, data = community_ordination)

anova(RDA_warm_adding_time_and_site, RDA_warm_novel_over_time) #Eigenval = 9.432627
anova(RDA_warm_adding_time_and_site, RDA_warm_extant_over_time) #Eigenval = 9.431591


#Testing the same without warming
RDA_novel_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ novel + site * year, data = community_ordination)
RDA_extant_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ extant + site * year, data = community_ordination)

anova(RDA_site_and_time, RDA_novel_over_time)#p-value between 0.009 and 0.007 eigenval = 9.377195
anova(RDA_site_and_time, RDA_extant_over_time)#p-value between 0.009 and 0.006 eigenval = 9.376232
#Does this mean that the novel is slightly more different than extant both with and without warming? 


#The next step is to see if the precipitation gradient has an effect. We hope to see that the warming effect reduces along the precipitation gradient from dry to wetter? 
RDA_precip_adding_warm_over_site_and_year <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ precip_2009_2019 + warming * site * year, data = community_ordination)
#Eigenval = 9.43032
RDA_precip_over_warm_site_and_year <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ precip_2009_2019 * warming * site * year, data = community_ordination)
#Eigenval = 9.43032

anova(RDA_warm_adding_time_over_site, RDA_precip_adding_warm_over_site_and_year) 
anova(RDA_warm_adding_time_over_site, RDA_precip_over_warm_site_and_year)
#Out of this result, I assume that the precip shows exactly the same as the site. Something that is good. 



#We  also expect that along the precipitation gradient that competition increases, porbaly see a larger difference between transplants in general, novel and extant. 
#The next step is to investigate if the different treatments have an effect in totalt, maybe compare them to a null model?


####RDA ordinations to the PRC####
RDA_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ site, data = community_ordination)
RDA_time <- rda(sqrt(community_ordination[,-c(1:7, 87:89)]) ~ year, data = community_ordination)#Yes
RDA_warm <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming, data = community_ordination)#Yes
RDA_precip <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ precip_2009_2019, data = community_ordination)
RDA_transplant <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ transplant, data = community_ordination)
RDA_novel <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ novel, data = community_ordination)
RDA_extant <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ extant, data = community_ordination)


RDA_warm_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * year, data = community_ordination)
RDA_warm_and_transplant_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * transplant * year, data = community_ordination)

RDA_warm_over_time_and_site <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * year + site, data = community_ordination)
RDA_warm_novel_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * novel * year, data = community_ordination)
RDA_warm_extant_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * extant * year, data = community_ordination)
RDA_warm_control_over_time <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * transplant * year, data = community_ordination)

RDA_complex <- rda(sqrt(community_ordination[, -c(1:7, 87:89)]) ~ warming * transplant * year + site + year, data = community_ordination)

anova(RDA_complex, RDA_warm_and_transplant_over_time)



#Testing site alone against site + year and site * year
anova(RDA_site, RDA_site_and_time)#Testing site with site and time
anova(RDA_site_and_time, RDA_site_over_time) #kan vurder

anova(RDA_site_and_time, RDA_)


#Testing year alone against site + year and site * year
anova(RDA_year, RDA_site_and_time)


#Testing warming alone against site + year and site * year, and testing warming over time with site and time, and site over time.
anova(RDA_warm, RDA_site_and_time)
anova(RDA_warm, RDA_site_over_time)
anova( RDA_site_and_time, RDA_warm_over_time)
anova(RDA_warm_over_time, RDA_site_over_time)
#Doesnt work

anova(RDA_warm, RDA_warm_over_time_and_site)
anova(RDA_warm, RDA_warm_over_time) #Not significant
anova(RDA_warm_over_time, RDA_warm_over_time_and_site)


#Testing site + year agains site * year
anova(RDA_site_and_time, RDA_site_over_time) #Does work but not significant



#Just a test
com_ulv$site <- factor(com_ulv$site) #Had to make it a factor with this line

lol <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ precip_2009_2019, data = com_skj)
lol

lol2 <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ precip_2009_2019 + year, data = com_skj)
lol2

anova(lol, lol2)


#The null model
null <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ 1 , data = community_ordination)
null

#Making an rda for each variable alone
precip <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ precip_2009_2019, data = community_ordination) #yes
warm <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ warming, data = community_ordination)
site <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ site, data = community_ordination)
time <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ year, data = community_ordination) #yes
treat <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ treatment, data = community_ordination)
trans <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ transplant, data = community_ordination)

anova(trans)

anova(null, trans)

prec_warm <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ precip_2009_2019 * warming, data = community_ordination)
anova(prec, prec_warm)

warm_time <- rda(sqrt(community_ordination[, -c(1:7, 87)])~ year * warming, data = community_ordination) #Use with all
site_time <- rda(sqrt(community_ordination[, -c(1:7, 87)])~ year + site, data = community_ordination) 
treat_time <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ year * treatment, data = community_ordination) 

#Kjør det inn som covariater i modellen

warm_time_site <- rda(sqrt(community_ordination[, -c(1:7, 87)])~ year * warming + site + year, data = community_ordination)

anova(warm_time_site, by = "term")
anova(warm_time_site, by = "margin")




#Treatment specific
com_novel <- community_ordination|>
  filter(treatment == "N")
com_control <- community_ordination |>
  filter(treatment == "C")
com_extant <- community_ordination |>
  filter(treatment == "E")
com_treat <- community_ordination |>
  filter(treatment == c("N", "E"))

com_treat_rda <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ warming * year * transplant + warming * year + site + year, data = community_ordination)
anova(com_treat_rda, by = "term")

time_warm_treatment <- rda(sqrt(community_ordination[, -c(1:7, 87)]) ~ treatment * warming * year + site, data = community_ordination) 


anova(com_treat_rda, time_warm_treatment)#Does not work

plot(warm)
text(warm, display = "species")


######Site specific#######
#Not sure if I can use this
#RDAs for warming and precipitation for each site
prec_warm_skj <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ precip_2009_2019 + warming, data = com_skj)
prec_warm_lav <- rda(sqrt(com_lav[, -c(1:7, 87)]) ~ precip_2009_2019 + warming, data = com_lav)
prec_warm_gud <- rda(sqrt(com_gud[, -c(1:7, 87)]) ~ precip_2009_2019 + warming, data = com_gud)
prec_warm_ulv <- rda(sqrt(com_ulv[, -c(1:7, 87)]) ~ precip_2009_2019 + warming, data = com_ulv)

#treatment (traits) and warming
treat_warm_skj <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ treatment + warming, data = com_skj)
treat_warm_lav <- rda(sqrt(com_lav[, -c(1:7, 87)]) ~ treatment + warming, data = com_lav)
treat_warm_gud <- rda(sqrt(com_gud[, -c(1:7, 87)]) ~ treatment + warming, data = com_gud)
treat_warm_ulv <- rda(sqrt(com_ulv[, -c(1:7, 87)]) ~ treatment + warming, data = com_ulv)

treat_skj <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ treatment, data = com_skj)
treat_lav <- rda(sqrt(com_lav[, -c(1:7, 87)]) ~ treatment, data = com_lav)
treat_gud <- rda(sqrt(com_gud[, -c(1:7, 87)]) ~ treatment, data = com_gud)
treat_ulv <- rda(sqrt(com_ulv[, -c(1:7, 87)]) ~ treatment, data = com_ulv)

anova(treat_skj, treat_warm_skj)
anova(treat_lav, treat_warm_lav)
anova(treat_gud, treat_warm_gud)
anova(treat_ulv, treat_warm_ulv)

#year and warming
year_warm_skj <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ year + warming, data = com_skj)
year_warm_lav <- rda(sqrt(com_lav[, -c(1:7, 87)]) ~ year + warming, data = com_lav)
year_warm_gud <- rda(sqrt(com_gud[, -c(1:7, 87)]) ~ year + warming, data = com_gud)
year_warm_ulv <- rda(sqrt(com_ulv[, -c(1:7, 87)]) ~ year + warming, data = com_ulv)

year_skj <- rda(sqrt(com_skj[, -c(1:7, 87)]) ~ year, data = com_skj)
year_lav <- rda(sqrt(com_lav[, -c(1:7, 87)]) ~ year, data = com_lav)
year_gud <- rda(sqrt(com_gud[, -c(1:7, 87)]) ~ year, data = com_gud)
year_ulv <- rda(sqrt(com_ulv[, -c(1:7, 87)]) ~ year, data = com_ulv)

anova(year_skj, year_warm_skj)
anova(year_lav, year_warm_lav)
anova(year_gud, year_warm_gud)
anova(year_ulv, year_warm_ulv)




anova(prec, prec_warm)
anova(null, warm)
anova(null, prec)
anova(null, site)
anova(null, time)
anova(null, treat)
##########################
#####Species richness#####
##########################

#Species richness
species_richness <- community_clean |>
  select(block|plot|year|warming|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
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
  ungroup()

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


#Species evenness
species_evenness <- community_clean |>
  select(block|plot|year|warming|treatment|site|species|presence|plotID|cover|subPlot)|>
  filter(!species %in% c("Car_pal", "Car_pil", "Hyp_mac", "Suc_pra", "Vio_can", "Ver_off"))|>
  filter(!subPlot %in% c(1,2,3,4,5,6,7,8,14,15,21,22,28,29,30,31,32,33,34,35,"whole_plot"))|>
  select(-subPlot)|>
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
  filter(!duplicated(species))
  mutate(richness = sum(pres, na.rm = TRUE)) |> 
  ungroup()|>
  select(-cover, -pres) |>
  unique()|>
  group_by(site, year, treat) |>
  mutate(treat_richness = mean(richness))|>
  ungroup()

evenness <- eventstar(com_ord_wide)
comp_data<-merge(evenness,community_ordination, by='row.names',all=TRUE)|>
  select(-c(Hstar,Dstar,qstar))
comp_data <- comp_data|>
  group_by(site,year,treat)|>
  mutate(treat_evenness = mean(Estar))|>
  ungroup()


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

species_abundance_skj <- species_evenness|>
  filter(site == "Skjellingahaugen")|>
  filter(treat == "Warm\nNovel")

species_abundance_lav <- species_evenness|>
  filter(site == "Lavisdalen")

species_abundance_gud <- species_evenness|>
  filter(site == "Gudmedalen")

species_abundance_ulv <- species_evenness|>
  filter(site == "Ulvehaugen")

distribution_skj <- within(species_abundance_skj, 
                   species <- factor(species, 
                                      levels = names(sort(table(species), 
                                                        decreasing=TRUE))))


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









#Ordination on every location together

Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

pca_wide <- rda(sqrt(com_ord_wide))
pca_wide
autoplot(pca_wide)

pca_fort_wide <- fortify(pca_wide, display = "sites") |>
  bind_cols(community_ordination[1:6])

Ord_plot_time <- pca_fort_wide %>% 
  ggplot(aes(x = PC1, y = PC2, colour = site, group = plotID)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == 2018, 5, NA_real_)), show.legend = FALSE) +
  #scale_color_viridis_d() +
  scale_fill_manual(values = Precip_palette) +
  scale_size(range = 2) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), plot.title = element_text(hjust = 0.1)) 
  #labs(fill = "Yearly precipitation", color = "Yearly precipitation", shape = "Yearly precipitation") +
  #xlab("PCA1 (43.9%)") + #Numbers added manually from the fviz_eig plot above
  #ylab("PCA2 (19.0%)") + #Numbers added manually from the fviz_eig plot above
  #guides(color = "none") +
  #scale_shape_manual(values = c(16, 17, 15))

#Separate the sites and colorcode the treatments. Fire seperate ordinations. Do this in a loop.

Ord_plot_time

ggplot(pca_fort, aes(x = PC1, y = PC2, colour = treat, group = site)) +
  geom_path() + #use geom_path not geom_line
  geom_point(aes(size = if_else(year == min(year), 1, NA_real_)), show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_size(range = 2) +
  coord_equal()


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

#Eva og Rune Ordinations


dca <- decorana(com_ord_wide[, 6:ncol(com_ord_wide)]) # global analysis
dca_skj <- decorana(com_ord_skj[, 6:ncol(com_ord_skj)]) # per site
dca_ulv <- decorana(com_ord_ulv[, 6:ncol(com_ord_ulv)])
dca_lav <- decorana(com_ord_lav[, 6:ncol(com_ord_lav)])
dca_gud <- decorana(com_ord_gud[, 6:ncol(com_ord_gud)])

#Save dca objects
saveRDS(dca, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_global.Rds")
saveRDS(dca_skj, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_skj.Rds")
saveRDS(dca_ulv, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_ulv.Rds")
saveRDS(dca_lav, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_lav.Rds")
saveRDS(dca_gud, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_gud.Rds")


# preliminary plots of first 2 axes
# global
plot(dca, main = "global")
# site-specific
{
  dev.new()
  par(mfrow = c(2, 2))
  plot(dca_skj, main = "Skj")
  plot(dca_ulv, main = "Ulv")
  plot(dca_lav, main = "Lav")
  plot(dca_gud, main = "Gud")
}
dev.off()

# extract DCA subplot scores
{
  # global dca first
  dca1 <-
    scores(dca, display = "sites", origin = FALSE)[, 1]# Note that origin=FALSE implies that origo of the ordination diagram is moved from the centroid to the lower end of each axis
  dca2 <- scores(dca, display = "sites", origin = FALSE)[, 2]
  dca3 <- scores(dca, display = "sites", origin = FALSE)[, 3]
  dca4 <- scores(dca, display = "sites", origin = FALSE)[, 4]
}

{
  # site-specific
  dca1_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 1]
  dca2_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 2]
  dca3_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 3]
  dca4_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 4]
  
  dca1_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 1]
  dca2_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 2]
  dca3_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 3]
  dca4_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 4]
  
  dca1_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 1]
  dca2_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 2]
  dca3_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 3]
  dca4_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 4]
  
  dca1_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 1]
  dca2_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 2]
  dca3_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 3]
  dca4_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 4]
}

# save plot scores
dca_obj_list <-
  list(
    dca1_skj = dca1_skj,
    dca2_skj = dca2_skj,
    dca3_skj = dca3_skj,
    dca4_skj = dca4_skj,
    dca1_ulv = dca1_ulv,
    dca2_ulv = dca2_ulv,
    dca3_ulv = dca3_ulv,
    dca4_ulv = dca4_ulv,
    dca1_lav = dca1_lav,
    dca2_lav = dca2_lav,
    dca3_lav = dca3_lav,
    dca4_lav = dca4_lav,
    dca1_gud = dca1_gud,
    dca2_gud = dca2_gud,
    dca3_gud = dca3_gud,
    dca4_gud = dca4_gud
  )
saveRDS(dca_obj_list,
        "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_list_plotscores_sitespecific.Rds")
dca_obj_list <-
  readRDS("C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_list_plotscores_sitespecific.Rds")
dca_list = c(
  "dca1_skj",
  "dca2_skj",
  "dca3_skj",
  "dca4_skj",
  "dca1_ulv",
  "dca2_ulv",
  "dca3_ulv",
  "dca4_ulv",
  "dca1_lav",
  "dca2_lav",
  "dca3_lav",
  "dca4_lav",
  "dca1_gud",
  "dca2_gud",
  "dca3_gud",
  "dca4_gud"
)
for (i in dca_list) {
  write.csv(
    as.data.frame(dca_obj_list[i]),
    paste("C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\", i, ".csv", sep = ""),
    row.names = FALSE
  )

  
}

# read dca plot scores
dca_obj_list <-
  readRDS("C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\dca_list_plotscores_sitespecific.Rds") # global
for (i in 1:length(dca_list)) {
  # site-specific
  assign(paste(dca_list[i]),
         read.csv(paste(
           "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\", dca_list[i], ".csv", sep = ""
         )))
}

# plot axes against each other
# global
plot(dca1, dca2) # NB! This shows an artifact (~triangular shape)
plot(dca1, dca3)
# site-specific
plot(dca1_skj, dca2_skj)
plot(dca1_skj, dca3_skj)

plot(dca1_ulv, dca2_ulv)
plot(dca1_ulv, dca3_ulv)

plot(dca1_lav, dca2_lav)
plot(dca1_lav, dca3_lav)

plot(dca1_gud, dca2_gud)
plot(dca1_gud, dca3_gud)

# check whether site-specific axes look similar to the global ones
# axes 1 and 2
{
  plot(
    x = dca1,
    y =  dca2,
    xlim = c(-0.2, 5),
    ylim = c(-0.2, 4),
    cex = 0.5,
    col = "grey",
    main = "site-specific plotted over global dca, axes 1 vs 2",
    sub = "black=Skj,blue=Ulv,green=Lav,red=Gud",
    xlab = "axis 1",
    ylab = "axis 2"
  )
  points(dca1_skj$dca1_skj,
         dca2_skj$dca2_skj,
         cex = 0.5,
         col = "black")
  points(dca1_ulv$dca1_ulv,
         dca2_ulv$dca2_ulv,
         cex = 0.5,
         col = "blue")
  points(dca1_lav$dca1_lav,
         dca2_lav$dca2_lav,
         cex = 0.5,
         col = "green")
  points(dca1_gud$dca1_gud,
         dca2_gud$dca2_gud,
         cex = 0.5,
         col = "red")
  abline(lm(dca1_skj$dca1_skj ~ dca2_skj$dca2_skj),
         lwd = 2,
         col = "black")
  abline(lm(dca1_ulv$dca1_ulv ~ dca2_ulv$dca2_ulv),
         lwd = 2,
         col = "blue")
  abline(lm(dca1_lav$dca1_lav ~ dca2_lav$dca2_lav),
         lwd = 2,
         col = "green")
  abline(lm(dca1_gud$dca1_gud ~ dca2_gud$dca2_gud),
         lwd = 2,
         col = "red")
}

# direct correlations of axes between the sites are not possible because they are
# different length (some species were omitted from analysis because they were
# missing from one site but not others). Could potentially go back and remove
# species that lack occurrences in some site(s) to force them to be comparable,
# but I deem it more important to keep the information in the species presences
# (i.e. the real species compositional differences between sites!).

# axes 1 and 3
{
  plot(
    x = dca1,
    y =  dca3,
    xlim = c(-0.2, 5),
    ylim = c(-0.2, 4),
    cex = 0.5,
    col = "grey",
    main = "site-specific plotted over global dca, axes 1 vs 3",
    sub = "black=Skj,blue=Ulv,green=Lav,red=Gud",
    xlab = "axis 1",
    ylab = "axis 3"
  )
  points(dca1_skj$dca1_skj,
         dca3_skj$dca3_skj,
         cex = 0.5,
         col = "black")
  points(dca1_ulv$dca1_ulv,
         dca3_ulv$dca3_ulv,
         cex = 0.5,
         col = "blue")
  points(dca1_lav$dca1_lav,
         dca3_lav$dca3_lav,
         cex = 0.5,
         col = "green")
  points(dca1_gud$dca1_gud,
         dca3_gud$dca3_gud,
         cex = 0.5,
         col = "red")
  abline(lm(dca1_skj$dca1_skj ~ dca3_skj$dca3_skj),
         lwd = 3,
         col = "black")
  abline(lm(dca1_ulv$dca1_ulv ~ dca3_ulv$dca3_ulv),
         lwd = 3,
         col = "blue")
  abline(lm(dca1_lav$dca1_lav ~ dca3_lav$dca3_lav),
         lwd = 3,
         col = "green")
  abline(lm(dca1_gud$dca1_gud ~ dca3_gud$dca3_gud),
         lwd = 3,
         col = "red")
}

# calculate gradient lenghts
# global
(grl1 <- max(dca1) - min(dca1)) # 5.3
(grl2 <- max(dca2) - min(dca2)) # 3.7
(grl3 <- max(dca3) - min(dca3)) # 3.6
(grl4 <- max(dca4) - min(dca4)) # 3.1
# site-specific
grl_sites <-
  data.frame(
    site = rep(
      c("Skjellingahaugen", "Ulvehaugen", "Lavisdalen", "Gudmedalen"),
      each = 4
    ),
    prec = rep(c(2725, 593, 1321, 1925), each = 4),
    axis = rep(1:4, 4),
    length = c(
      grl1_skj = max(dca1_skj) - min(dca1_skj),
      grl2_skj = max(dca2_skj) - min(dca2_skj),
      grl3_skj = max(dca3_skj) - min(dca3_skj),
      grl4_skj = max(dca4_skj) - min(dca4_skj),
      
      grl1_ulv = max(dca1_ulv) - min(dca1_ulv),
      grl2_ulv = max(dca2_ulv) - min(dca2_ulv),
      grl3_ulv = max(dca3_ulv) - min(dca3_ulv),
      grl4_ulv = max(dca4_ulv) - min(dca4_ulv),
      
      grl1_lav = max(dca1_lav) - min(dca1_lav),
      grl2_lav = max(dca2_lav) - min(dca2_lav),
      grl3_lav = max(dca3_lav) - min(dca3_lav),
      grl4_lav = max(dca4_lav) - min(dca4_lav),
      
      grl1_gud = max(dca1_gud) - min(dca1_gud),
      grl2_gud = max(dca2_gud) - min(dca2_gud),
      grl3_gud = max(dca3_gud) - min(dca3_gud),
      grl4_gud = max(dca4_gud) - min(dca4_gud)
    )
  )
write.csv(grl_sites, "C:\\Users\\cam-d\\OneDrive\\Documents\\UIB\\Master\\Master_oppgave\\R\\INCLINE\\gradient_lengths_dca.csv")
plot(grl_sites$prec, grl_sites$length) # looks like the gradient lenght (indicating largest turnover in species composition) may have an optimum in medium-dry sites.

# extract DCA species scores
{
  # global
  dca1var <- scores(dca, display = "species", origin = FALSE)[, 1]
  dca2var <- scores(dca, display = "species", origin = FALSE)[, 2]
  dca3var <- scores(dca, display = "species", origin = FALSE)[, 3]
  dca4var <- scores(dca, display = "species", origin = FALSE)[, 4]
}
{
  # site-specific
  dca1var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 1]
  dca2var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 2]
  dca3var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 3]
  dca4var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 4]
  
  dca1var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 1]
  dca2var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 2]
  dca3var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 3]
  dca4var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 4]
  
  dca1var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 1]
  dca2var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 2]
  dca3var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 3]
  dca4var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 4]
  
  dca1var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 1]
  dca2var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 2]
  dca3var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 3]
  dca4var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 4]
}





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