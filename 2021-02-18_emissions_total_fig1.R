# Figure 1
#Map of total emissions and 
# barchart of top landuse contributions
#CJ Brown 2020-04-30

rm(list = ls())

library(sf)
library(ggplot2)
library(dplyr)
library(tmap)
library(RColorBrewer)

source("st_recenter.R")

load("Outputs/2020-04-19_emissions-projections.rda")
load("Outputs/2020-04-19_fraction-emission.rda")
prov.abr <- read.csv("PROV_NAMES.csv") # read in abbreviations for fig 1B

meow <- st_read("spatial-data/MEOW", "MEOW_ecos")

wrld <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))

wrld_seasia <- st_recenter(wrld, -200) # change to -250 for true center on SEasia
# plot(wrld_seasia)

wrld_seasia_robin <- st_transform(st_segmentize(st_buffer(wrld_seasia, -0.001), 50000), "+proj=robin +lon_0=-200")
# plot(wrld_seasia_robin)

units <- 1E12/1E6 #units of original are tonnes CO2 equiv. 
emdat$Etotal <- (emdat$Cmax_tree_B + emdat$Cmax_soil2m_B_seq)/units
#Add sequetration once only (ie for soil but not for trees)
emdat <- emdat %>% left_join(prov.abr, by = 'PROVINCE')

emtotal <- subset(emdat, scnr == "Total")  

# write.csv(emtotal, "2020-05-17-model-preds.csv", row.names = FALSE)
# ----------------- #
# Figure 1a map 
# ----------------- #

emmap <- meow %>% group_by(PROVINCE) %>%
  summarize() %>% 
  left_join(emtotal) %>%
  filter(!is.na(Etotal))

emmap_re <- st_recenter(emmap, clon = -200)
emmap_re_robin <- st_transform(st_segmentize(st_buffer(emmap_re, -0.001), 50000), "+proj=robin +lon_0=-200")
  
tm1 <- tm_shape(emmap_re_robin) + 
  tm_polygons(title = expression("TgCO"[2][eq]),
              col = "Etotal",
              style = "log10",
              alpha = 0.7,
              palette = 'YlOrRd') + 
  tm_shape(wrld_seasia_robin) + 
  tm_polygons() +
  tm_legend(legend.outside = TRUE)

tm1  
tmap_save(tm1, 
          filename = "Plots/2021-02-19-figures-paper/fig1a.png",
          dpi = 600)
  

# ----------------- #
# Figure 1b  map 
# ----------------- #

#Save fractional emissions
f1 <- fract_Cmax %>% left_join(select(emtotal,PROVINCE, Etotal))
emf1 <- apply(f1[,1:5], 2, function(x) x*f1$Etotal)

write.csv(cbind(f1$PROVINCE, emf1), "2020-05-18_emissions-fractional-amounts.csv", row.names = FALSE)

#
flong <- fract_Cmax %>%
  tidyr::pivot_longer(-PROVINCE, names_to="LU", values_to = "fract")

lucols <- c("#339933", "#ff9900", "#996633", "#3366cc", "grey")
#which LU is max in each prov? 
fract_Cmax$imax <- apply(fract_Cmax[,1:5],1,  which.max)
fract_Cmax$maxlu_name <- names(fract_Cmax[,1:5])[fract_Cmax$imax]
emmap_re_robin <- emmap_re_robin %>%
  left_join(fract_Cmax) #%>% filter(PROVINCE == 'Southwest Australian Shelf')
emmap_re_robin$maxlu_name <- factor(emmap_re_robin$maxlu_name,
                                    levels = c('Commodities',
                                               'Erosion', 
                                               'Non_Productive_Conversion',
                                               'Extreme_Climatic_Events'),
                                    labels =  c("Agri/aquaculture",
                                                "Erosion", 
                                                "Clearing",
                                                "Climatic events"))

tm2 <- tm_shape(emmap_re_robin) + 
  tm_polygons(title = "",
              col = "maxlu_name",
              palette = lucols,
              alpha = 0.7) + 
  tm_shape(wrld_seasia_robin) + 
  tm_polygons() +
  tm_legend(legend.outside = TRUE)

tm2

tmap_save(tm2, filename = "Plots/2021-02-19-figures-paper/fig3B_map-top-land-use.png",
          dpi = 600)

tboth <- tmap_arrange(tm1, tm2)

tmap_save(tboth, filename = "Plots/2021-02-19-figures-paper/fig1AB.png",
          dpi = 600,
          width = 10,
          height = 6)
# ----------------- #
# Figure 2: barplot of emissions by LU types
# ----------------- #

lucols <- c("#339933", "#ff9900", "#996633", "#3366cc", "grey")
#lucols <- brewer.pal(5, 'Set3')
  
emscnr <- emtotal %>% 
  select(PROVINCE_abr, Etotal, PROVINCE) %>%
  full_join(flong) %>% 
  mutate(Epartial = fract * Etotal)

emtotal <- emtotal%>% arrange(desc(Etotal))
ntop <- 10

emscnr$scnr2 <- factor(emscnr$LU, 
                       levels = c('Commodities',
                                  'Erosion', 
                                  'Non_Productive_Conversion',
                                  'Extreme_Climatic_Events',
                                  'Settlement'),
                       labels =  c("Agriculture/aquaculture",
                                   "Erosion", 
                                   "Clearing", 
                                   "Climatic events",
                                   "Human settlements"))

g1 <- emscnr %>%
  filter(PROVINCE_abr %in% emtotal[1:ntop,]$PROVINCE_abr) %>%
  mutate(PROVINCE_abr = factor(PROVINCE_abr, levels = emtotal$PROVINCE_abr[1:ntop])) %>%
  ggplot() + 
  geom_bar(aes(x = PROVINCE_abr, y = Epartial, fill = scnr2), col = 'black', stat = 'identity', alpha = 0.7) + 
  scale_fill_manual("Land-use", values = lucols) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
        axis.text.y = element_text(size = 10)) + 
  xlab("") + 
  ylab(expression(paste("Emissions (", TgCO[2][eq],")", sep="")))
 # ylab(expression(paste("Emissions (TgCO",[2][eq],")")))
g1  

ggsave(g1, 
       filename = "Plots/2021-02-19-figures-paper/fig2.png",
       dpi = 600,
       width = 7, height = 5)

  