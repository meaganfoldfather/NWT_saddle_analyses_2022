# Look at thermphilization patterns across snow gradient
# M. F. Oldfather
# 20220308

# source data compilation script
source("scripts/saddle_data_compilation.R")

# main dataset
veg_snow

# add back in USDA_names
vegetation <- 
  left_join(veg_snow, unique(species_hit_totals[,3:4]))
vegetation

# bring in climate niche mean data
climate_niche_means <- read_csv("data/species-climate-niche-means.csv")
climate_niche_means <-
 climate_niche_means %>% 
  select(!species)
colnames(climate_niche_means)[5] <- "USDA_name"


# combine veg, snow data and climate niche means
veg_snow_niche <- left_join(vegetation, climate_niche_means)
veg_snow_niche


# add factor for snowiness metric
veg_snow_niche$Snow_Persistence <- "Low Snow"
veg_snow_niche[veg_snow_niche$snow_rank == 2, "Snow_Persistence"] <- "Average Snow"
veg_snow_niche[veg_snow_niche$snow_rank == 3, "Snow_Persistence"] <- "High Snow"

# calculate thermal niche for each plot in each year
weighted_clm <- 
veg_snow_niche %>% 
  group_by(year, snow_rank, Snow_Persistence, plot) %>% 
  summarise(WCMcwd = weighted.mean(CLMcwd, hits, na.rm = T), 
            WCMprecip = weighted.mean(CLMprecip, hits, na.rm = T),
            WCMtemp = weighted.mean(CLMtemp, hits, na.rm = T),
            WCMsummertemp = weighted.mean(CLMsummertemp, hits, na.rm = T))
weighted_clm

plot_thermo <- weighted_clm %>% 
  ggplot(aes(year, WCMtemp, color = Snow_Persistence, fill = Snow_Persistence))+
  geom_point(size = .5)+
  geom_smooth(method = "lm", se =F)+
  theme_classic()+
    xlab("Year")+
  ylab("Community-weighted Climate Niche - Temperature")+
      scale_colour_viridis_d(option = "turbo")+
    scale_fill_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=18))

ggsave(filename = "figure/thermophilzation.jpeg", plot_thermo)

fit_temp <-lmer(WCMtemp ~ year*Snow_Persistence + (1|plot) + (1|year), data = weighted_clm )   
summary(fit_temp)
anova(fit_temp)
emtrends(fit_temp , specs = "Snow_Persistence", var = "year")
