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
  group_by(year, snow_rank, Snow_Persistence, snow_depth, plot) %>% 
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

# snow rank
fit_temp <-lmer(WCMtemp ~ year*Snow_Persistence + (1|plot) + (1|year), data = weighted_clm )   
summary(fit_temp)
anova(fit_temp)
emtrends(fit_temp , specs = "Snow_Persistence", var = "year")

# snow continious
fit_temp <-lmer(WCMtemp ~ year*snow_depth + (1|plot) + (1|year), data = weighted_clm )   
summary(fit_temp)
anova(fit_temp)

# year by quadratic snow interaction
fit_temp_plots <- lmer(WCMtemp ~ year*snow_depth + year*I(snow_depth^2) + (1|plot) + (1|year), data = weighted_clm )
summary(fit_temp_plots)
anova(fit_temp_plots)
acf(residuals(fit_temp_plots)) # not autocorrelated, do I need to rearrange the df?

# snow quadratic no interaction 
fit_temp_simple_quad <- lmer(WCMtemp ~ year*snow_depth + I(snow_depth^2) + (1|plot) + (1|year), data = weighted_clm )
summary(fit_temp_simple_quad)

# compare models
anova(fit_temp, fit_temp_plots)
anova(fit_temp, fit_temp_simple_quad)
anova(fit_temp_plots, fit_temp_simple_quad)
# fit with quadratic interaction is best

#3Dplot
plot_ly(z=weighted_clm$WCMtemp, x=weighted_clm$year, y=weighted_clm$snow_depth, type="scatter3d", mode="markers", color=weighted_clm$WCMtemp)

# try to make a smoother plot
new_data <- as_tibble(expand.grid(year = 1990:2020, snow_depth = 0:350))
new_data
new_data$preds <- predict(object = fit_temp_plots, newdata = new_data, re = NA)
plot_ly(z=new_data$preds, x=new_data$year, y=new_data$snow_depth, type="scatter3d", mode="markers", color=new_data$preds) %>% 
  layout(scene = list(xaxis = list(title = "Year"), yaxis = list(title = "Snow Persistence"), zaxis = list(title = "Community-weighted Climate Niche - Temperature")))
