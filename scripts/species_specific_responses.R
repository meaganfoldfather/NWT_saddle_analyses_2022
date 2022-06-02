# Taking a stab at  looking at the specific species driving the turnover and thermophilization trends

library(mblm)

#source("scripts/saddle_data_compilation.R")  # use veg_snow
source("scripts/thermophilization_script.R")
veg_snow
veg_snow$SnowPersistence <- "Average Snow"
veg_snow[veg_snow$snow_rank == 1, "SnowPersistence"] <- "Low Snow"
veg_snow[veg_snow$snow_rank == 3, "SnowPersistence"] <- "High Snow"
veg_snow$SnowPersistence <- factor(veg_snow$SnowPersistence, levels = c("Low Snow", "Average Snow", "High Snow"))

# look at species with largest change in hits through time (includes zeros)
species_slopes <-
veg_snow %>% 
  group_by(USDA_code) %>% 
  summarise(slope = summary(lm(hits ~ year))$coefficients[2])

species_slopes <-
veg_snow %>% 
  filter(!USDA_code %in% c("CAMPA")) %>% 
  group_by(USDA_code) %>% 
  summarise(slope = summary(lmer(hits ~ year + (1|plot)))$coefficients[2])
# using lm or lmer gives you same results. 
plot(density(species_slopes$slope))

species_slopes %>% 
  filter(slope > 0.02 | slope < -0.02) %>% 
ggplot(aes(USDA_code, slope))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()
# Deschampsia and Kobresia have greatly increased. Kobresia is ~cold associated (-4.8) and Deschampsia is hot associated (4.1), that's pretty cool --> are they increasing in different types of plots? 

# look at species with largest change in hits through time by snow rank 
species_slopes_snow <-
veg_snow %>% 
  group_by(USDA_code, snow_rank) %>% 
  summarise(slope = summary(lm(hits ~ year))$coefficients[2])

plot(density(species_slopes_snow$slope))

# rank 1 - exposed sites --> Kobresia is increasing, as is DECE, but to a much lower extent
species_slopes_snow %>% 
  filter(snow_rank == 1) %>% 
  filter(slope > 0.02 | slope < -0.02) %>% 
ggplot(aes(USDA_code, slope))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  ggtitle("Exposed")

# rank 2 - average sites --> Deschampsia is greatly increasing
species_slopes_snow %>% 
  filter(snow_rank == 2) %>% 
  filter(slope > 0.02 | slope < -0.02) %>% 
ggplot(aes(USDA_code, slope))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  ggtitle("Average")

# rank 3 - snowy sites --> Deschampsia is greatly increasing
species_slopes_snow %>% 
  filter(snow_rank == 3) %>% 
  filter(slope > 0.02 | slope < -0.02) %>% 
ggplot(aes(USDA_code, slope))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  ggtitle("Snowy")

# Plot Kobresia through time
veg_snow %>% 
  filter(USDA_code == "KOMY") %>% 
  ggplot(aes(year, hits, color = SnowPersistence)) +
  geom_point()+
  geom_line(aes(group = plot))+
  theme_classic()+
  xlab("Year")+
  ylab("Abundance")+
  facet_wrap(.~ SnowPersistence)+
  ggtitle("Kobresia myosuroides") + 
     scale_color_viridis_d(option = "mako")+
   theme(legend.title=element_blank(), text = element_text(size=14))
# I wonder if the plots that have the highest increased in Kobresia in snow rank 1 are the ones with low ES values? 


# Plot Deschampsia through time
veg_snow %>% 
  filter(USDA_code == "DECE") %>% 
  ggplot(aes(year, hits, color = SnowPersistence)) +
  geom_point()+
  geom_line(aes(group = plot))+
  theme_classic()+
  xlab("Year")+
  ylab("Abundance")+
  facet_wrap(.~ SnowPersistence)+
  ggtitle("Deschampsia cespitosa") + 
     scale_color_viridis_d(option = "mako")+
   theme(legend.title=element_blank(), text = element_text(size=14))

# Other canidates: Artemesia scopolurim (ARSC) and Minuartia obtusiloba (MIOB2) from Sarah 
# Geum is decreasing the less snow sites,  but it's thermal niche is ~0

# Is the thermophilizastion patterns driven by species increases in abundance or decreases? Need to compare slope to thermal niches by rank --> use veg_snow_niche
# need to make a dataframe that is just the crosswalk from codes to names
slope_niches <-
 left_join(species_slopes_snow, distinct(veg_snow_niche[,c(3,9:13)]), by = "USDA_code")
slope_niches

slope_niches$SnowPersistence <- "Average Snow"
slope_niches[slope_niches$snow_rank == 1, "SnowPersistence"] <- "Low Snow"
slope_niches[slope_niches$snow_rank == 3, "SnowPersistence"] <- "High Snow"
slope_niches$SnowPersistence <- factor(slope_niches$SnowPersistence, levels = c("Low Snow", "Average Snow", "High Snow"))

slope_niches %>% 
  ggplot(aes(CLMtemp, slope, color = SnowPersistence, fill  = SnowPersistence))+
  geom_point()+
  theme_bw()+
    theme_classic()+
    facet_wrap(.~ SnowPersistence)+
  ylab("Temporal Change in Abundance")+
  xlab("Thermal Niche")+
  geom_smooth(data =slope_niches[slope_niches$snow_rank == 1,], method = "lm", se = T, lty = "solid")+
    geom_smooth(data =slope_niches[slope_niches$snow_rank == 2:3,], method = "lm", se = T, lty = "dashed")+
    scale_color_viridis_d(option = "mako")+
  scale_fill_viridis_d(option = "mako")+
   theme(legend.title=element_blank(), legend.position = "none", text = element_text(size=18))

# This plot is even more convincing that largely the patterns are driven by the two species discussed above, except maybe in low snow where some warm species are being lost too 
summary(lm(slope ~ CLMtemp, data = slope_niches[slope_niches$snow_rank == 1,])) # significant! But also huge leverage of Kobmyo --> try Theil Sen estimator
summary(mblm(slope ~ CLMtemp, repeated = F, data = na.omit(slope_niches[slope_niches$snow_rank == 1,])))
# still significant!


summary(lm(slope ~ CLMtemp, data = slope_niches[slope_niches$snow_rank == 2,])) # not significant
summary(mblm(slope ~ CLMtemp, repeated = F, data = na.omit(slope_niches[slope_niches$snow_rank == 2,]))) # significantly zero

summary(lm(slope ~ CLMtemp, data = slope_niches[slope_niches$snow_rank == 3,])) # not significant
summary(mblm(slope ~ CLMtemp, repeated = F, data = na.omit(slope_niches[slope_niches$snow_rank == 3,])))
# not significant


slope_niches %>% 
  filter(SnowPersistence == "Low Snow", slope < -0.1)  
# These species are Sellegennal and Geum, which are decreasing

# Investigate of losses/gain os species also driving community composition, or just changes in abudance 
# calculate species-specific extinction and colonization rates
rates <-
veg_snow %>% 
  group_by(SnowPersistence, plot, USDA_code) %>%  
  summarise(
  
    #extinction
    extinction = case_when(
    sum(hits[year %in% 1990:1995]) > 0 & sum(hits[year %in% 2019:2020]) == 0 ~ 1,
    sum(hits[year %in% 1990:1995]) > 0 & sum(hits[year == 2019:2020]) > 0 ~ 0),
  
    #colonization
    colonization = case_when(
    sum(hits[year %in% 1990:1995]) == 0 & sum(hits[year %in% 2019:2020]) > 0 ~ 1,
    sum(hits[year %in% 1990:1995]) == 0 & sum(hits[year %in% 2019:2020]) == 0 ~ 0),
    
    #change in cover 
    
    cover_change = case_when( 
      sum(hits) > 0 ~
      (sum(hits[year %in% 2019:2020]) - sum(hits[year %in% 1990:1995]))),
      # cover could also be done with hits ~ yr (impacted by mean average abundance?)
      cover_slope = case_when( 
      sum(hits) > 0 ~ lm(hits ~ year)$coefficients[2])
    )

rates_CNM <-
  left_join(rates, distinct(veg_snow_niche[,c(3,9:13)]))
rates_CNM


# Models 
# extinction temp
fit_ext_temp <- glmer(extinction ~ CLMtemp*SnowPersistence + (1|USDA_code) + (1|plot), data = rates_CNM, family = "binomial")
summary(fit_ext_temp)
emtrends(fit_ext_temp, specs = "SnowPersistence", var = "CLMtemp")

rates_CNM %>% 
ggplot(aes(CLMtemp, extinction))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  facet_wrap(.~SnowPersistence)

# colonization temp
fit_col_temp <- glmer(colonization ~ CLMtemp*SnowPersistence + (1|USDA_code) + (1|plot), data = rates_CNM, family = "binomial")
summary(fit_col_temp)
emtrends(fit_col_temp, specs = "SnowPersistence", var = "CLMtemp")

rates_CNM %>% 
ggplot(aes(CLMtemp, colonization))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  facet_wrap(.~SnowPersistence)

# Overall low snow sites have had more colonization and extinctions, but not directly related to the thermal niche of the species 


# Does this calculation of cover change and slope tell a different story?
# colonization temp
fit_cov_temp <- lmer(cover_slope ~ CLMtemp*SnowPersistence + (1|USDA_code), data = rates_CNM)
summary(fit_cov_temp)
emtrends(fit_cov_temp, specs = "SnowPersistence", var = "CLMtemp")
# No clear relationship, but still deschampsia and kobresia are the ones that change the most

rates_CNM %>% 
ggplot(aes(CLMtemp, cover_slope))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  facet_wrap(.~SnowPersistence)

fit_cover_temp <- lmer(cover_change ~ CLMtemp*SnowPersistence + (1|USDA_code), data = rates_CNM)
summary(fit_cover_temp)
emtrends(fit_cover_temp, specs = "SnowPersistence", var = "CLMtemp")
# There is a significant change negative relationship between thermal niche and chnage in cover in low snow areas!

rates_CNM %>% 
ggplot(aes(CLMtemp, cover_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  facet_wrap(.~SnowPersistence)


     