# Pattern of richness through time across snow persistence gradient 

source("scripts/thermophilization_script.R")
veg_snow
veg_snow$SnowPersistence <- "Average Snow"
veg_snow[veg_snow$snow_rank == 1, "SnowPersistence"] <- "Low Snow"
veg_snow[veg_snow$snow_rank == 3, "SnowPersistence"] <- "High Snow"
veg_snow$SnowPersistence <- factor(veg_snow$SnowPersistence, levels = c("Low Snow", "Average Snow", "High Snow"))


richness <-
  veg_snow %>% 
  filter(hits > 0) %>% 
  group_by(year, plot, veg_class, snow_depth, snow_rank, SnowPersistence) %>% 
  summarize(richness = length(unique(USDA_code)))
richness  


richness %>% 
  ggplot(aes(year, richness, color = SnowPersistence, fill = SnowPersistence))+
  #geom_point(size = .5)+
  geom_line(aes(group = plot), color = "lightgray", alpha = .3)+
  geom_smooth(method = "lm", se =F)+
  theme_classic()+
    xlab("Year")+
  ylab("Richness")+
  scale_colour_manual(values = colors)+
    #scale_colour_viridis_d(option = "mako")+
    #scale_fill_viridis_d(option = "mako")+
   theme(legend.title=element_blank(), text = element_text(size=18))

# model fit
fit_richness <-lmer(richness ~ year*SnowPersistence + (1|plot) + (1|year), data = richness)   
summary(fit_richness)
anova(fit_richness)
emtrends(fit_richness, specs = "SnowPersistence", var = "year")
# Only high snow areas increasing in richness - what are these new species? Drier meadow species? Low elevation species? 


richness %>% 
  ggplot(aes(year, richness, color = veg_class, fill = veg_class))+
  #geom_point(size = .5)+
  geom_line(aes(group = plot), color = "gray", alpha = .3)+
  geom_smooth(method = "lm", se =F)+
  theme_classic()+
    xlab("Year")+
  ylab("Richness")+
      scale_colour_viridis_d(option = "turbo")+
    scale_fill_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=18))


richness %>% 
  ggplot(aes(year, richness, color = veg_class, fill = veg_class))+
  geom_boxplot(aes(group = interaction(year, veg_class)))+
  theme_classic()+
    xlab("Year")+
  ylab("Richness")+
      scale_colour_viridis_d(option = "turbo")+
    scale_fill_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=18))+
  facet_wrap(.~veg_class)

# model fit
fit_richness <-lmer(richness ~ year*veg_class + (1|plot) + (1|year), data = richness)   
summary(fit_richness)
anova(fit_richness)
emtrends(fit_richness, specs = "veg_class", var = "year")

# What species are driving the increase in richness in these high snow sites? Look at rates_CNM from species_specific_response.R script 
rates_CNM %>% 
  group_by(USDA_name, SnowPersistence) %>% 
  summarize(total_colonization = sum(colonization)) %>% 
  arrange(-total_colonization) %>% 
  view()

# Campula increasing in low snow sites, lots of Carex and Arenanria in the high snow sites 

