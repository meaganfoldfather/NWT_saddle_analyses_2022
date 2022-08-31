# Intent: Look at correlations between thermal and functional traits

library(tidyverse)
library(ggcorrplot)
library(GGally)

# bring in in climate data for the species
thermal_trait <- read_csv("data/species-climate-niche-means.csv")
thermal_trait

ggcorrplot::ggcorrplot(cor(thermal_trait[,2:5]), type = "lower")

# bring in functional trait data (trait_average dataframe from trait_trends.R script)
source("scripts/trait_trends.R")
trait_average

# subset to focal traits
focal_traits <-
  trait_average %>% 
  select(USDA_name, OHeight, LDMC, SLA, D13C)
focal_traits

# make species names talk nicely to merge datasets
colnames(focal_traits)[1] <- "saddleName"

all_traits <- left_join(thermal_trait, focal_traits)
all_traits

cor.mat <- cor(all_traits[,c(4,7:10)], use=  "pairwise.complete.obs")
p.mat <- cor_pmat(all_traits[,c(4,7:10)])

ggcorrplot::ggcorrplot(cor(all_traits[,c(4,7:10)], use=  "pairwise.complete.obs" ), type = "lower", colors = c("#6D9EC1", "white", "#E46726"), lab = T, p.mat = p.mat) 
pairs(all_traits[,c(4,7:10)])

ggpairs(all_traits,   columns = c(4,7:10) )+ theme_classic()

all_traits %>% 
  ggplot(aes(CLMtemp, D13C))+
  geom_point()+
  theme_classic()+
  geom_smooth(method = "lm", color = "black")

summary(lm(D13C ~ CLMtemp, data = all_traits)) # there is a negative but non-signficant relationship between CLMtemp and D13C
