# Ecostress Saddle Data - examining relationship to snowiness, and patterns of community turnover, thermophilization 

# libraries
library(tidyverse)

source("scripts/saddle_data_compilation.R")

# bring in ecostress data
es <- read_csv("data/Elisa_plot_values/SaddleGrid_Ecostress_fromOriginalRasterlayers_BestPixelsOnly.csv")
es$NodeNb # need to modify to match snow and veg plot names (change 10A to 101)
es[es$NodeNb == "10A", "NodeNb"] <- "101"
es[es$NodeNb == "20A", "NodeNb"] <- "201"
es[es$NodeNb == "30A", "NodeNb"] <- "301"
es[es$NodeNb == "40A", "NodeNb"] <- "401"
es[es$NodeNb == "50A", "NodeNb"] <- "501"
es[es$NodeNb == "60A", "NodeNb"] <- "601"
es[es$NodeNb == "70A", "NodeNb"] <- "701"
es[es$NodeNb == "80A", "NodeNb"] <- "801"
es$plot <- as.numeric(es$NodeNb)

es$mean # assume this the mean across all dates
  
# snow data from saddle_data_compilation.R
snowiness_by_plot

# merge snow and es data
es_snow <- left_join(es, snowiness_by_plot,  by = "plot")
es_snow

# look at correlations between snow and es mean
es_snow %>% 
  ggplot(aes(snow_depth, mean))+
  geom_point()+
  theme_classic()+
  xlab("Average Snow Persistence")+
  ylab("Average ECOSTRESS")+
  geom_smooth(method = "lm", color = "black", lty = "dashed", se = F)
summary(lm(mean ~ snow_depth, data = es_snow)) # no significant relationship 

# bring in compositional change data - coords_snow from compilation script - and merge with es
coords_snow
coords_snow_es <- 
  left_join(coords_snow, es[,c("plot", "mean")]) 
coords_snow_es  

# look at relationships
fit_snow_es <- lmer(comp_change ~ year*snow_depth*mean + (1|plot) + (1|year), data = coords_snow_es)
summary(fit_snow_es)  
anova(fit_snow_es)

# make groups of es values for plotting purposes
coords_snow_es<-
  coords_snow_es %>% 
  mutate(es_rank = ntile(mean, 3))
  
coords_snow_es %>% 
  ggplot(aes(year, comp_change, color = as.factor(es_rank)))+
  geom_point() +
  geom_smooth(se = F, method = "lm")+
  facet_wrap(.~snow_rank)+
  theme_classic()+
  scale_colour_viridis_d(option = "viridis")
 # The most composition change is in cold (less stressed?), exposed sites. 

#bring in thermophilization data - weighted_clim is the useful main dataframe
source("scripts/thermophilization_script.R")
es_weighted_clm <-
  #left_join(weighted_clm, es[,c("plot", "mean")])
  left_join(weighted_clm, coords_snow_es)

view(es_weighted_clm)

# look at similar model as above with community compositions change, but for thermophilixation ...likely need to include quadratics... 
thermo_snow_es <- lmer(WCMtemp ~ year*snow_depth*mean + (1|plot) + (1|year), data = es_weighted_clm)
summary(thermo_snow_es)  
anova(thermo_snow_es) 

es_weighted_clm %>% 
  ggplot(aes(year, WCMtemp, color = as.factor(es_rank)))+
  geom_point() +
  geom_smooth(se = F, method = "lm")+
  facet_wrap(.~snow_rank)+
  theme_classic()+
  scale_colour_viridis_d(option = "viridis")
# There are some issues still ... why is there plots with es_rank = NA? That may be plot 28 ... which was removed. But quickly, there seems to be that ecostress largely impacts the exposed sites, with the coldest sites showing reverse thermophilzation 
      