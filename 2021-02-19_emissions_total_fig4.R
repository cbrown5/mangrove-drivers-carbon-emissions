# Figure 2
#Emissions by data types 
#CJ Brown 2020-12-11
#Added comparison to Goldberg data 2020-12
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(scales)

load("Outputs/2020-12-11_emissions-projections-goldberg.rda")
emdatg <- emdat

load("Outputs/2020-04-19_emissions-projections.rda")

#Notes on Figures 2c & d
# For climate reduce deforestation in proportion to prop climate
# ie this means that mangroves can then grow back quickly and 
# be cut down for aquculture
# For erosion - change emission factor to 50%, but erosion still contributes to areal loss. 



#
# ----------------- #
# Barplots 
# ----------------- #
units <- 1E12/1E6 #units of original are tonnes CO2 equiv. 
#Emissions field vs modelled

fdat <- emdat %>% filter(scnr == "Total") %>%
  filter(!is.na(Cmax_soil_field_HC)) %>%
  summarize_at(grep("_B_seq", names(emdat)), sum, na.rm = TRUE) %>% 
  pivot_longer(cols =1:7,names_to = "Scenario", values_to = "Emissions") %>%
  filter(!grepl("erosion", Scenario))
fdat$Scenario
fdat$Scenario <- c("ABC \n modelled", 
                   "SOC 1m \n modelled",
                   "SOC 2m \n modelled",
                   "ABC \n field",
                   "SOC \n field")
ordscnr <- levels(factor(fdat$Scenario))[c(5, 3, 4, 1, 2)]
fdat$col <- c("Tree", "SOC", "SOC", "Tree", "SOC")
g1 <- fdat %>%
  mutate(Scenario = factor(Scenario, 
                           levels = ordscnr)) %>%
  ggplot() + 
  aes(x = Scenario, y = Emissions/units, fill = col) + 
  geom_col(col = "black") + 
  theme_classic() + 
  theme(axis.text.x = element_text(), 
        legend.position = "none") + 
  scale_fill_manual(values = c("grey", "white")) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  xlab("") + 
  ylab("")
  # ylab("Emissions \n (tera grams CO \u00B2 equivalent )") 
g1

# 1m vs 2m, seq vs none, HC vs B
dat_dtype <- emdat %>% filter(scnr == "Total") %>%
  mutate(`SOC 1m; Bunting` = Cmax_tree_B + Cmax_soil1m_B,
         `SOC 2m; Bunting` = Cmax_tree_B + Cmax_soil2m_B,
         `SOC 1m; H&C` =Cmax_tree_HC + Cmax_soil1m_HC,
         `SOC 2m; H&C` = Cmax_tree_HC + Cmax_soil2m_HC) %>%
  summarize_at(32:35, sum, na.rm = TRUE) %>%
  pivot_longer(cols = 1:4, names_to = "Scenario", values_to = "Emissions") %>%
  mutate(Sequestration = FALSE)

dat_dtypes <- emdat %>% filter(scnr == "Total") %>%
  mutate(`SOC 1m; Bunting` = Cmax_tree_B + Cmax_soil1m_B_seq,
         `SOC 2m; Bunting` = Cmax_tree_B + Cmax_soil2m_B_seq,
         `SOC 1m; H&C` =Cmax_tree_HC + Cmax_soil1m_HC_seq,
         `SOC 2m; H&C` = Cmax_tree_HC + Cmax_soil2m_HC_seq) %>%
  summarize_at(32:35, sum, na.rm = TRUE) %>%
  pivot_longer(cols = 1:4, names_to = "Scenario", values_to = "Emissions") %>%
  mutate(Sequestration = TRUE)


g2 <- bind_rows(dat_dtype, dat_dtypes) %>%
  mutate(Scenario = factor(Scenario, 
                           levels = dat_dtype$Scenario[order(dat_dtype$Emissions, decreasing = TRUE)])) %>%
  ggplot() + 
  aes(x = Scenario, y = Emissions/units, fill = Sequestration) + 
  geom_col(position = "dodge") + 
  theme_classic()  + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ 
  xlab("") + 
  ylab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("grey", "black"))
  # ylab("Emissions \n (tera grams CO \u00B2 equivalent )")

g2

# Prov order with  no erosion and climate
emdat$Emissions <- emdat$Cmax_tree_B + emdat$Cmax_soil2m_B_seq
emdat$Emissions_no_ero <- emdat$Cmax_tree_no_erosion_B + 
  emdat$Cmax_soil2m_no_erosion_B_seq

dat_clim <- emdat %>% filter(scnr == "Extreme_Climatic_Events") %>%
  select(PROVINCE, scnr, E_climate = Emissions ) 
dat_total <- emdat %>% filter(scnr == "Total") %>%
  select(PROVINCE, scnr, Emissions, Emissions_no_ero)

dat_all <- bind_cols(dat_total, dat_clim) %>%
  mutate(`Emissions no climate` = Emissions - E_climate,
         `Rank total` = order(Emissions, decreasing = TRUE),
         `Rank low erosion` = order(Emissions_no_ero, decreasing = TRUE),
         `Rank no climate` = order(`Emissions no climate`, decreasing = TRUE))

g3 <- ggplot(dat_all) + 
  aes(x = `Emissions`/units, y =  `Emissions no climate`/units) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point() +
theme_classic()  + 
  scale_x_log10(labels=trans_format('log10',math_format(10^.x))) + 
  scale_y_log10(labels=trans_format('log10',math_format(10^.x))) +
  ylab("") + 
  xlab("")+
  annotate(geom="text", x=80, y=0.1, 
           label="Without emissions \n from climatic \n events",
           color="black") +
  annotate(geom="text", x=0.095, y=100, 
           label="With emissions \n from climatic \n events",
           color="black")

  # xlab("Emissions \n (tera grams CO \u00B2 equivalent )") + 
  # ylab("Emissions no climate emissions \n (tera grams CO \u00B2 equivalent )")
g3

g4 <- ggplot(dat_all) + 
  aes(x = `Emissions`/units, y =  Emissions_no_ero/units) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point() +
  theme_classic()  + 
  scale_x_log10(labels=trans_format('log10',math_format(10^.x))) + 
  scale_y_log10(labels=trans_format('log10',math_format(10^.x))) +
  ylab("") + 
  xlab("") +
  annotate(geom="text", x=80, y=0.1, 
           label="50% loss of \n carbon from \n erosion",
           color="black") +
  annotate(geom="text", x=0.08, y=100, 
           label="100% loss of \n carbon from \n erosion",
           color="black")
  
  # xlab("Emissions \n (tera grams CO \u00B2 equivalent )") + 
  # ylab("Emissions low erosion emissions \n (tera grams CO \u00B2 equivalent )")
g4



#
# Comparison to Goldberg 
#


# Prov order with  no erosion and climate
emdatg$Emissions <- emdatg$Cmax_tree_B + emdatg$Cmax_soil2m_B_seq
emdatg$Emissions[is.nan(emdatg$Emissions)] <- 0
emdat$Emissions[is.nan(emdat$Emissions)] <- 0

dat_all2 <- emdatg %>% 
  select(PROVINCE, Emissions, scnr) %>%
  left_join(emdat, by = c("PROVINCE", "scnr"))  %>%
  filter(scnr == "Total")

g5 <- ggplot(dat_all2) + 
  aes(x = Emissions.y/units, y =  Emissions.x/units) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point() +
  theme_classic()  + 
  scale_x_log10(labels=trans_format('log10',math_format(10^.x))) + 
  scale_y_log10(labels=trans_format('log10',math_format(10^.x))) +
  ylab("") + 
  xlab("")+
 xlab("Emissions H&C \n (tera grams CO \u00B2 equivalent )") + 
 ylab("Emissions Goldberg et al. \n (tera grams CO \u00B2 equivalent )")
g5

ggsave(g5,
       filename = "Plots/goldberg-hc-comparison.png",
        width = 5, height = 4)

dat_all2$fract <- dat_all2$Emissions.x/dat_all2$Emissions.y

write.csv(arrange(select(dat_all2, PROVINCE, 
                 emissions_goldberg = Emissions.x,
                 emissions_hc = Emissions.y,
                 fraction = fract),fraction),
          "Plots/Goldberg-comparison.csv")

dat_dtypesg <- emdatg %>% filter(scnr == "Total") %>%
  mutate(`SOC 1m; Bunting` = Cmax_tree_B + Cmax_soil1m_B_seq,
         `SOC 2m; Bunting` = Cmax_tree_B + Cmax_soil2m_B_seq,
         `SOC 1m; H&C` =Cmax_tree_HC + Cmax_soil1m_HC_seq,
         `SOC 2m; H&C` = Cmax_tree_HC + Cmax_soil2m_HC_seq) %>%
  summarize_at(33:36, sum, na.rm = TRUE) %>%
  pivot_longer(cols = 1:4, names_to = "Scenario", values_to = "Emissions") %>%
  mutate(Sequestration = TRUE,
         Rate = "Goldberg et al. 2020")

dat_dtypes$Rate = "Hamilton and Casey \n 2016"

#rename 'scenario'

datg6 <- bind_rows(dat_dtypes, dat_dtypesg) %>%
  mutate(Scenario = factor(Scenario, 
                           levels = 
                             dat_dtypes$Scenario[
                               order(dat_dtypes$Emissions, 
                                     decreasing = TRUE)]))
datg6$Scenario2 <- datg6$Scenario
levels(datg6$Scenario2) <- c("SOC 2m \n Bunting et al. \n 2018",
                            "SOC 2m \n Hamilton \n and Casey \n 2016",
                            "SOC 1m \n Bunting et al. \n 2018",
                            "SOC 1m \n Hamilton \n and Casey \n 2016")
g6 <- ggplot(datg6) + 
  aes(x = Scenario2, y = Emissions/units, fill = Rate) + 
  geom_col(position = "dodge") + 
  theme_classic()  + 
  theme(axis.text.x = element_text()) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+ 
  xlab("") + 
  ylab("") +
  theme(legend.position = c(0.8, 0.9),
        legend.title = element_blank()) +
  scale_fill_manual(values = c("grey", "black")) 
# ylab("Emissions \n (tera grams CO \u00B2 equivalent )")
g6

ggsave(g6, filename = "Plots/goldberg-total-comparison.png",
       width = 5, height = 5)



gall <- plot_grid(g1, g6, g3, g4, 
                  labels = paste0("(",letters[1:4],")"),
                  nrow = 2, ncol = 2,
                  label_x = 0.15)
gall

ggsave2(gall, filename = "Plots/2021-02-19-figures-paper/fig4_sensitivity.png",
        width = 8, height = 6, dpi = 600)

