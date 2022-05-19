# Examine how temporal trends in community-weighted traits vary across snow persistence gradients

library(tidyverse)
library(naniar)

# bring in updated trait files (sent to MFO directly by Jared in early 2022)
traits <- read_csv("data/Niwot_Traits_Plants_20220322.csv")
traits

# change some Latin.name's to fit species comp data from Niwot, 
# and to consolidate some species that are in the data twice under different names
traits  <- traits  %>%
  mutate(Latin.name = as.character(Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Arenaria fendleri", "Arenaria fendleri var. fendleri", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Antennaria alpina", "Antennaria media", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Bistort bistortoides", "Polygonum bistortoides", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Bistort viviparum", "Polygonum viviparum", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Carex heteroneura", "Carex heteroneura var. epapillosa", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Carex rupestris", "Carex rupestris var. drummondiana", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Carex scopulorum", "Carex scopulorum var. scopulorum", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Cerastium arvense", "Cerastium arvense ssp. strictum", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Chamerion angustifolium", "Chamerion angustifolium ssp. angustifolium", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Gentianella amarella", "Gentianella amarella ssp. acuta", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Gentianoides algida", "Gentiana algida", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Geum rossii", "Geum rossii var. turbinatum", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Heuchera parvifolia var. nivalis", "Heuchera parvifolia", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Oreoxis alpina", "Oreoxis alpina ssp. alpina", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Poa glauca", "Poa glauca ssp. rupicola", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Senecio fremontii", "Senecio fremontii var. blitoides", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Silene acaulis", "Silene acaulis var. subacaulescens", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Solidago spathulata", "Solidago simplex ssp. simplex var. nana", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Tetraneuris acaulis", "Tetraneuris acaulis var. caespitosa", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Trifolium parryi", "Trifolium parryi ssp. parryi", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Thlaspi montanum", "Noccaea montana", Latin.name)) %>%
  mutate(Latin.name = ifelse(Latin.name == "Thlaspi montanum var. montanum", "Noccaea montana", Latin.name))

# Change USDA codes for species which are currently listed as "NC" or " " to most up-to-date Niwot species 
trait_data_file <- traits %>%
  mutate(USDA.Code = as.character(USDA.Code)) %>%
  mutate(USDA.Code = if_else(Latin.name == "Thlaspi montanum", "NOMO2", USDA.Code)) %>%
  mutate(USDA.Code = if_else(Latin.name == "Solidago spathulata", "SOSIN", USDA.Code)) %>%
  mutate(USDA.Code = if_else(Latin.name == "Heuchera parvifolia", "HEPA11", USDA.Code)) 

# Replace all NC entries (Not collected) with NA
trait_data_file <- trait_data_file %>%
  mutate(across(c(VegHeight, OHeight, SLA), ~as.character(.))) %>%
  replace_with_na_all(condition = ~.x == "NC") %>%
  mutate(across(c(VegHeight, OHeight, Stomatal.Conductance:CN_ratio), ~as.numeric(.)))

# get rid of trait measurements that were collected from experimentally manipulated plots, relabel TRT to DM, MM, WM, SB
trait_data_file <- trait_data_file %>% 
  filter(!TRT %in% c("PNW","XNW","XXW","PXW","PXX","XNX", "PNX")) %>%
  mutate(TRT = as.character(TRT)) %>%
  mutate(TRT = if_else(TRT == "DRY", "DM", TRT)) %>%
  mutate(TRT = if_else(TRT == "MOIST", "MM", TRT)) %>%
  mutate(TRT = if_else(TRT == "WET", "WM", TRT)) %>%
  mutate(TRT = if_else(TRT == "SNOWBED", "SB", TRT))

# average across all replicates for a subset of traits 
trait_average<- trait_data_file %>% 
  group_by(Latin.name) %>% 
  summarise(across(c(VegHeight, OHeight, Stomatal.Conductance:CN_ratio), mean, na.rm = TRUE)) 
trait_average
view(trait_average)
colnames(trait_average)[1] <- "USDA_name"

# bring in saddle community data
source("scripts/saddle_data_compilation.R")

# add back in USDA_names
vegetation <- 
  left_join(veg_snow, unique(species_hit_totals[,3:4]))
vegetation

#join community data and trait data
community_trait <- 
  left_join(vegetation, trait_average)
community_trait

# filter plots by abundant species having trait data if needed
view(community_trait)

# take community weighted mean for each trait
weighted_traits <- 
  community_trait %>%  
  group_by(year, snow_rank, plot) %>% 
  summarise(W_height = weighted.mean(OHeight, hits, na.rm = T), 
            W_LDMC = weighted.mean(LDMC, hits,  na.rm = T),
            W_SLA = weighted.mean(SLA, hits,  na.rm = T),
            W_CNratio = weighted.mean(CN_ratio, hits,  na.rm = T),
            W_SC = weighted.mean(Stomatal.Conductance, hits,  na.rm = T),
            W_D13 = weighted.mean(D13C, hits,  na.rm = T),
            W_N15 = weighted.mean(D15N, hits,  na.rm = T))
weighted_traits

# add in snow persistence factor
weighted_traits$Snow_Persistence <- "Low Snow"
weighted_traits[weighted_traits$snow_rank == 2, "Snow_Persistence"] <- "Average Snow"
weighted_traits[weighted_traits$snow_rank == 3, "Snow_Persistence"] <- "High Snow"
weighted_traits$Snow_Persistence <- factor(weighted_traits$Snow_Persistence, levels = c("Low Snow", "Average Snow", "High Snow"))

#### Height ####
weighted_traits %>% 
  ggplot(aes(year, W_height, color = Snow_Persistence))+
  geom_point() +
  #geom_line(aes(group = plot))+
  theme_classic()+
  geom_smooth(method = "lm", se = F)+
  xlab("Year")+
  ylab("Community-Weighted Trait (Height)")+
  facet_wrap(.~Snow_Persistence)+
      scale_color_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=14))

fit_height <- lmer(W_height ~ year*Snow_Persistence + (1|plot) + (1|year),data = weighted_traits)
summary(fit_height)
emtrends(fit_height, specs = "Snow_Persistence", var = "year") 
# increasing across snow gradient, strongest increases in average snow sites, but not sig differently groups

#### LDMC ####
weighted_traits %>% 
  ggplot(aes(year, W_LDMC, color = Snow_Persistence))+
  geom_point() +
  #geom_line(aes(group = plot))+
  theme_classic()+
  geom_smooth(method = "lm", se = F)+
  xlab("Year")+
  ylab("Community-Weighted Trait (LDMC)")+
    facet_wrap(.~Snow_Persistence)+
      scale_color_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=14))

fit_LDMC <- lmer(W_LDMC ~ year*Snow_Persistence + (1|plot) + (1|year),data = weighted_traits)
summary(fit_LDMC) # significant positive
emtrends(fit_LDMC, specs = "Snow_Persistence", var = "year") 
# low snow generally has higher LDMC, increasing, not sig diff slopes between groups

#### W_D13 ####
weighted_traits %>% 
  ggplot(aes(year, W_D13, col = Snow_Persistence))+
  geom_point() +
  #geom_line(aes(group = plot)) +
  theme_classic()+
  geom_smooth(method = "lm", se = F)+
  xlab("Year")+
  ylab("Community-Weighted Trait (D13)")+
   facet_wrap(.~Snow_Persistence)+
      scale_color_viridis_d(option = "mako")+
   theme(legend.title=element_blank(), text = element_text(size=14))


fit_D13 <- lmer(W_D13 ~ year*Snow_Persistence + (1|plot) + (1|year), data = weighted_traits)
summary(fit_D13) # significantly positive
emtrends(fit_D13, specs = "Snow_Persistence", var = "year") 
# No change in average or high snow sites, but increase in WUE in low snow sites!!!!!!!


###################################################
#### SLA ####
weighted_traits %>% 
  ggplot(aes(year, W_SLA, col = Snow_Persistence))+
  geom_point() +
  #geom_line(aes(group = plot))+
  theme_classic()+
  geom_smooth(method = "lm", se = F)+
  xlab("Year")+
  ylab("Community-Weighted Trait (SLA)")+
     facet_wrap(.~Snow_Persistence)+
      scale_color_viridis_d(option = "mako")+
   theme(legend.title=element_blank(), text = element_text(size=14))

fit_SLA <- lmer(W_SLA ~ year*Snow_Persistence + (1|plot) + (1|year),data = weighted_traits)
summary(fit_SLA) 
emtrends(fit_SLA, specs = "Snow_Persistence", var = "year") 
# all sig negative slopes, high snow has a significantly stronger decrease in SLA

#### W_N15 ####
weighted_traits %>% 
  ggplot(aes(year, W_N15, col = Snow_Persistence))+
  geom_point() +
  #geom_line(aes(group = plot))+
  theme_classic()+
  geom_smooth(method = "lm", se = F)+
  xlab("Year")+
  ylab("Community-Weighted Trait (N15)")+
       facet_wrap(.~Snow_Persistence)+
      scale_color_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=14))

fit_N15 <- lmer(W_N15 ~ year*Snow_Persistence + (1|plot) + (1|year),data = weighted_traits)
summary(fit_N15) # slight increase
emtrends(fit_N15, specs = "Snow_Persistence", var = "year") 
# low and average snow decreasing, but high snow increasing

# Overall, height and LDMC increasing across all group. WUE increasing only in low snow sites. SLA decreasing in all sites, but steeper loss in high snow sites. N15 decreasing in low and average sites, but increasing in high snow sites.  
