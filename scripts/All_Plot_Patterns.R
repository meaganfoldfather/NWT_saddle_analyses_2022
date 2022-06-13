# Figure with all species in analyses through time 

library(gridExtra)
source("scripts/saddle_data_compilation.R")
subset_veg_abundance_known_species

pdf("figure/plots.pdf", onefile = TRUE)
plots <- unique(subset_veg_abundance_known_species$plot)
for(i in 1:length(plots)){
    dat <- subset(subset_veg_abundance_known_species, plot==plots[i])
    dat <- filter(dat, hits > 0)
    top.plot <- ggplot(dat, aes(year,hits, color = USDA_code)) + 
      geom_point() + 
      geom_line()+
      ggtitle(plots[i])+
      scale_colour_viridis_d(option = "turbo")+
      theme_classic() 
    print(top.plot)
}
dev.off()



