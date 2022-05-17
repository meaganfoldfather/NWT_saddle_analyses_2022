# Taking a stab at  looking at the specific species driving the turnover and thermophilization trends

source("scripts/saddle_data_compilation.R")  # use veg_snow
source("scripts/thermophilization_script.R")
veg_snow
veg_snow$SnowPersistence <- "Average Snow"
veg_snow[veg_snow$snow_rank == 1, "SnowPersistence"] <- "Low Snow"
veg_snow[veg_snow$snow_rank == 3, "SnowPersistence"] <- "High Snow"

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
     scale_color_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=16))
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
     scale_color_viridis_d(option = "turbo")+
   theme(legend.title=element_blank(), text = element_text(size=16))

# Other canidates: Artemesia scopolurim (ARSC) and Minuartia obtusiloba (MIOB2) from Sarah 
# Geum is decreasing the less snow sites,  but it's thermal niche is ~0

# Is the thermophilizastion patterns driven by species increases in abundance or decreases? Need to compare slope to thermal niches by rank --> use veg_snow_niche
test <-
 left_join(species_slopes_snow, veg_snow_niche[,c(3,9:13)], by = "USDA_code") # This isn't working... creates huge df
test

# need to make a dataframe that is just the crosswalk from codes to names...



