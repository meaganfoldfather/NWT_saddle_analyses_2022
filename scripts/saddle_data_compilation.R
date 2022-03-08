# Bring in long-term saddle vegetation data, temperature data, and snowpack data; examine temporal patterns in each
# M. F. Oldfather
# 20220307

#libraries
library(tidyverse)
library(emmeans)
library(lmerTest)
library(codyn)
library(lubridate)
library(viridisLite)

# bring in veg data (downloaded October 2021)
veg_data_original<- read_csv("data/saddptqd.hh.data.csv")
veg_data_original

# change year = 1996 to 1995 for plot 37 (typo in dataset)
veg_data_original[veg_data_original$year == 1996,"year"] <- 1995

# keep only bottom and top hits to allow for consistency across time, and limit to 1 hit per x,y - bottom hit if there is only a bottom hit, otherwise, top hit --> "veg_data"
veg_data <-
    veg_data_original %>% 
    filter(hit_type == "bottom" | hit_type == "top") %>%  
    arrange(year, plot, x, y, desc(hit_type)) %>% 
    group_by(year, plot, x,y) %>% 
    slice(1)

# summarize hits per species in each plot in each time point - then add zeros for species not hit in plot-year combinations in case useful moving forward --> "veg_abundance"
species_hit_totals <-
  veg_data %>% 
  group_by(year, plot, USDA_code, USDA_name) %>% 
  summarise(hits = n())
species_hit_totals

data_with_zeros <- expand_grid(year = unique(veg_data$year), plot = unique(veg_data$plot), USDA_code = unique(veg_data$USDA_code))
data_with_zeros

veg_abundance <- left_join(data_with_zeros, species_hit_totals) %>% 
  dplyr::select(-USDA_name) 
veg_abundance[is.na(veg_abundance$hits), "hits"] <- 0
veg_abundance
 
# remove unknown species and non-species --> "veg_abundance_known_species"
non_plant <- c("2RF", "2LICHN", "2X","2LTR","2BARE", "2HOLE", "2MOSS", "2SCATE")
unknown_species <- c("2FORB","2MOSS","2GRAM", "2UNKSC", "POA","2UNK", "CAREX", "2COMP") 

veg_abundance_known_species <-
  veg_abundance %>% 
  filter(!(USDA_code %in% non_plant)) %>% 
  filter(!(USDA_code %in% unknown_species)) %>% 
  filter(!(USDA_code %in% "CASCS2")) # there is duplicates in this species - need to investigate
veg_abundance_known_species

#remove plots where there are no hits in that specific year (plots with only rocks)
subset_veg_abundance_known_species<-
veg_abundance_known_species %>% 
  group_by(year, plot) %>% 
  mutate(totals = sum(hits)) %>% 
  filter(totals > 0)

# remove 1989
#subset_veg_abundance_known_species <- 
#subset_veg_abundance_known_species %>% 
#  filter(year != 1989)

# analysis of turnover from reference time persion 
mumti_change <- multivariate_change(df=subset_veg_abundance_known_species, time.var = "year", species.var = "USDA_code",abundance.var = "hits", replicate.var = "plot", reference.time = 1989)
mumti_change 

plot_temporal_turnover_all<- mumti_change %>% 
ggplot(aes(year2, composition_change)) +
geom_point()+
geom_line()+
theme_classic()+
xlab("Year")+
ylab("Compositional Change Relative to 1989")+
ggtitle("Saddle Veg Temporal Change")+
ylim(0,.15)
plot_temporal_turnover_all

plot_spatial_turnover_all<- mumti_change %>% 
ggplot(aes(year2, dispersion_change)) +
geom_point()+
geom_line()+
theme_classic()+
xlab("Year")+
ylab("Dispersion Change Relative to 1989")+
#ylim(-.025, 0.01)+
geom_hline(yintercept = 0)+
ggtitle("Saddle Veg Spatial Change")
plot_spatial_turnover_all

#Notes: 
# - do we need to remove Snow Fence plots?

#Next steps - divide by different snowiness quantiles and re-do above analyses - do we see certain parts of the landscape changing more?

# bring in snow data
# bring in snow daily data
snow <- read_csv("data/saddsnow.dw.data.csv")
snow$date <-ymd(snow$date)
snow$year <- year(snow$date) 
snow$month <- month(snow$date)
# first year of surveys 
min(snow$year) # 1992

# focus on May depth
snow_May_plot_means <-
snow %>% 
  filter(month == 5) %>% 
  group_by(year, point_ID) %>% 
  summarise(snow_depth = round(mean(mean_depth, na.rm = T),digits = 0))

# look at snow depth through time - no directional patterns - supports taking a average over years; there is some evidence of snowier plots becomes less snowy through time... more evident in May than June
snow_May_plot_means %>% 
ggplot(aes(year, snow_depth, group = point_ID)) +
  geom_point()+
  geom_line()+
  theme_classic()+
  geom_smooth(method = "lm", se=F)

# estimate the snowiness of each plot
snowiness_by_plot <- 
  snow_May_plot_means %>% 
    group_by(point_ID) %>% 
  summarise(snow_depth = round(mean(snow_depth, na.rm = T),digits = 0))
snowiness_by_plot

# put plots in to quantiles by snow  1 = low,, 3 = high snow
snowiness_by_plot <-
snowiness_by_plot  %>% 
mutate(snow_rank = ntile(snow_depth, 3))
colnames(snowiness_by_plot)[1] <- "plot" 
snowiness_by_plot

# combine veg and snow data --> veg_snow
veg_snow <- left_join(subset_veg_abundance_known_species, snowiness_by_plot)
veg_snow

# analysis of turnover from reference time persion 
mumti_change_1 <- 
veg_snow %>% 
  filter(snow_rank == 1) %>% 
multivariate_change(time.var = "year", species.var = "USDA_code",abundance.var = "hits", replicate.var = "plot", reference.time = 1990)
mumti_change_1$Snow_Persistence <- "Low Snow"
mumti_change_1$rank <- 1
mumti_change_1 

mumti_change_2 <- 
veg_snow %>% 
  filter(snow_rank == 2) %>% 
multivariate_change(time.var = "year", species.var = "USDA_code",abundance.var = "hits", replicate.var = "plot", reference.time = 1990)
mumti_change_2$Snow_Persistence <- "Average Snow"
mumti_change_2$rank <- 2
mumti_change_2 

mumti_change_3 <- 
veg_snow %>% 
  filter(snow_rank == 3) %>% 
multivariate_change(time.var = "year", species.var = "USDA_code",abundance.var = "hits", replicate.var = "plot", reference.time = 1990)
mumti_change_3$Snow_Persistence <- "High Snow"
mumti_change_3$rank <- 3
mumti_change_3 

turnover <- rbind(mumti_change_1, mumti_change_2, mumti_change_3)

turnover %>% 
    filter(year2 != 1989) %>% 
  ggplot(aes(year2, composition_change, color = Snow_Persistence)) +
  geom_point(size = 3)+
  geom_line(lwd = 1.1)+
  theme_classic()+
  xlab("Year")+
  ylab("Compositional Change Relative to 1990")+
      scale_colour_viridis_d(option = "turbo")+
  theme(legend.title=element_blank(), text = element_text(size=18))

turnover %>% 
  filter(year2 != 1989) %>% 
  ggplot(aes(year2, dispersion_change, color = Snow_Persistence)) +
  geom_point(size = 3)+
  geom_line(lwd = 1.1)+
  theme_classic()+
  geom_hline(yintercept = 0)+
  xlab("Year")+
  ylab("Dispersion Relative to 1989")+
  scale_colour_viridis_d(option = "turbo")+
  theme(legend.title=element_blank(), text = element_text(size=18))
# the extremes are changing the most - we know from the thermophilization analysis that these extremes are changing in different ways

# correlate changing composition to GDD, look for interactions with snow rank
# bring in temperature data
#bring in temperature data
temp <- read_csv("data/sdl_temp_1981-2020_draft.csv")
temp

# calculate GDD
GDD <- 
  temp %>% 
  filter(yr >= 1989, metric == 'mean') %>% 
  filter(adjusted_airtemp > 0) %>% 
  group_by(yr) %>% 
  summarise(GDD = round(sum(adjusted_airtemp),0))
colnames(GDD)[1] <- "year2"

# combine turnover and GDD
turnover_temp <- left_join(turnover, GDD)
turnover_temp 
fit1 <- lm(composition_change ~ GDD*Snow_Persistence, data = turnover_temp)
summary(fit1)
emtrends(fit1 , specs = "Snow_Persistence", var = "GDD")
emmeans(fit1, pairwise ~ Snow_Persistence)
# low snow and high snow are similar, but average snow is different

turnover_temp %>% 
  ggplot(aes(GDD, composition_change, color = Snow_Persistence, fill = Snow_Persistence))+
  geom_point(size = 2)+
  theme_classic()+
    xlab("Growing Degree-Days")+
  ylab("Compositional Change Relative to 1989")+
      scale_colour_viridis_d(option = "turbo")+
  scale_fill_viridis_d(option = "turbo")+
  theme(legend.title=element_blank(), text = element_text(size=18))+
  geom_smooth(method = "lm", se = T)


# Considering time series analyses...
fit2 <- lm(composition_change ~ year2*Snow_Persistence + lag(year2,1), data = turnover_temp)
summary(fit2)
anova(fit2)
emtrends(fit2 , specs = "Snow_Persistence", var = "year2")

Turnover_temp <-
turnover_temp %>% 
  arrange(year2)


fit3 <- lm(composition_change ~ year2, data = turnover_temp) 
acf(resid(fit3)) # very autocorrelated residuals 


