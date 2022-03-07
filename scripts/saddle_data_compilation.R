# Bring in long-term saddle vegetation data, temperature data, and snowpack data; examine temporal patterns in each
# M. F. Oldfather
# 20220307

#libraries
library(tidyverse)
library(emmeans)
library(codyn)

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

plot1<- mumti_change %>% 
ggplot(aes(year2, composition_change)) +
geom_point()+
geom_line()+
theme_classic()+
xlab("Year")+
ylab("Compositional Change Relative to 1989")+
ggtitle("Saddle Veg Temporal Change")+
ylim(0,.15)
plot1

plot2<- mumti_change %>% 
ggplot(aes(year2, dispersion_change)) +
geom_point()+
geom_line()+
theme_classic()+
xlab("Year")+
ylab("Dispersion Change Relative to 1989")+
#ylim(-.025, 0.01)+
geom_hline(yintercept = 0)+
ggtitle("Saddle Veg Spatial Change")
plot2





