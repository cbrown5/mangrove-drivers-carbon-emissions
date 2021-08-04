# Figure 2
#Emissions by data types 
#CJ Brown 2020-04-19

rm(list  = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)

#assumes colunm order from "2020-03-27_Mangrove_Data_CAB2.csv"
#Check this if data is changed 

load("Outputs/2020-04-30_emissions-projections-fig3.rda")
units <- 1E12/1E6 #units of original are tonnes CO2 equiv. 

emdat_all_years$scnr2 <-  factor(emdat_all_years$scnr, 
                                 levels = 
                                   c("Commodities" ,
                                     "Erosion", 
                                     "Non_Productive_Conversion",
                                     "Extreme_Climatic_Events",  
                                     "Settlement",
                                     "Total"), 
                                 labels = 
                                   c("Agriculture/aquaculture",
                                     "Erosion", 
                                     "Clearing",
                                     "Climatic events", 
                                     "Settlement",
                                     "Total"))

lucols <- c("#339933", "#ff9900", "#996633","#3366cc", "grey")
#
# Carib
#


datc <- filter(emdat_all_years, PROVINCE == "Tropical Northwestern Atlantic") %>%
  filter(scnr %in% c("Erosion", "Extreme_Climatic_Events", "Non_Productive_Conversion")) %>%
  mutate(Etotal = (Cmax_tree_B + Cmax_soil2m_B_seq)/units,
         Year = Year + 2010)

lucolsc <- lucols[c(2,3,4)]

g2 <- ggplot(datc) + 
  aes(x = Year, y = Etotal, color = scnr2)+#, linetype = scnr2) + 
  geom_line(size = 1) +
  theme_classic()  + 
  # ylab(expression('Emissions (Tg CO'[2][eq]*')')) + 
  ylab("")+
  xlab("") + 
  # ylab("Avoided emissions \n (tera grams CO \u00B2 equivalent )") + 
  scale_color_manual("Land-use", values = lucolsc) + 
  scale_linetype_manual("Land-use", values = c(1,4,2)) + 
  theme(
    legend.title = element_blank(),
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    axis.text=element_text(size=12))
g2
#
# CT
#


datct <- filter(emdat_all_years, PROVINCE == "Western Coral Triangle") %>%
  filter(scnr %in% c("Erosion", "Commodities", "Non_Productive_Conversion")) %>%
  mutate(Etotal = (Cmax_tree_B + Cmax_soil2m_B_seq)/units,
         Year = Year + 2010)

lucols <- c("#339933", "#ff9900", "#3366cc", "#996633", "grey")[c(1,2,4)]

g1 <- ggplot(datct) + 
  aes(x = Year, y = Etotal, color = scnr2)+#, linetype = scnr2) + 
  geom_line(size = 1) +
  theme_classic()  + 
  # ylab("Avoided emissions \n (tera grams CO \u00B2 equivalent )") + 
  ylab(expression('Emissions (Tg CO'[2][eq]*')')) + 
  xlab("") + 
  scale_color_manual("Land-use", values = lucols) + 
  scale_linetype_manual("Land-use", values = c(1,4,2)) + 
  theme(
    legend.title = element_blank(),
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    axis.text=element_text(size=12))

g1a <- g1 +
  geom_segment(aes(x = 2100, y = 750, xend = 2100,
  yend = 250), arrow = arrow(length = unit(0.25, "cm")),
  color = lucols[1]) +
  geom_segment(aes(x = 2100, y = 200, xend = 2100,
                   yend = 50), arrow = arrow(length = unit(0.25, "cm")),
               color = lucols[2]) +
  annotate(geom="text", x=2080, y=350, 
           label="Intensification of \n agriculture",
           color="black") + 
  annotate(geom="text", x=2080, y=60, 
           label="Shore stabilisation",
           color="black")
  
xsx <- 3
g2a <- g2 +
  geom_segment(aes(x = 2100, y = 230, xend = 2100,
                   yend = 190), arrow = arrow(length = unit(0.25, "cm")),
               color = lucolsc[1]) +
  geom_segment(aes(x = 2100, y = 150, xend = 2100,
                   yend = 120), arrow = arrow(length = unit(0.25, "cm")),
               color = lucolsc[2]) +
  geom_segment(aes(x = 2100, y = 90, xend = 2100,
                   yend = 20), arrow = arrow(length = unit(0.25, "cm")),
               color = lucolsc[3]) +
  annotate(geom="text", x=2090, y=180, 
           label="Shore \n stabilisation",
           color="black", size = xsx,
           angle = 25) + 
  annotate(geom="text", x=2090, y=120, 
           label="Mangrove \n protection",
           color="black", size = xsx,
           angle = 20) + 
  annotate(geom="text", x=2082, y=43, 
           label="Restoration of  \n mangroves affected \n by tropical storms",
           color="black", size = xsx)

g2a

#
# Plot 
#


p1 <- plot_grid(g1a, g2a, labels = c("(a)", "(b)"),
                label_x = 0.15,
           nrow = 1, 
           rel_widths = c(1, 1))

p1
ggsave2(p1, filename = "Plots/2021-02-19-figures-paper/fig3.png",
        width = 8, height = 3.5, dpi = 600)


#
# Compare proportions
#


dc <- filter(emdat_all_years, PROVINCE == "Tropical Northwestern Atlantic") %>%
  filter(Year == 90) %>% 
  filter(scnr != "Total") %>%
  mutate(Etotal = (Cmax_tree_B + Cmax_soil2m_B_seq))

dc$Etotal/sum(dc$Etotal)

load("Outputs/2020-04-19_fraction-emission.rda")
fract_Cmax %>% filter(PROVINCE == "Tropical Northwestern Atlantic")
