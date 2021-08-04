#Create figure 1 for the paper 

#CJ Brown
# 2020-04-30
#This script assumes a certain column order of input data 
#19th April: Updated to calculate fractional amounts due to each LU

#Set provinces to select ones and years to select years to do 
# figure 3. Use all provs, but 1 year for figs 1/2. 


rm(list = ls())

library(tidyverse)
library(ggrepel)
library(patchwork)
library(RColorBrewer)
library(cowplot)

source("emission_model.R")

#assumes colunm order from "2020-03-27_Mangrove_Data_CAB2.csv"
#Check this if data is changed 

dat <- read_csv("2020-03-27_Mangrove_Data.csv")

s_rate <- 6.5 #tonnes CO2 per ha
emission_rate <- 0.1 

tmax <- 90
years <- tmax
years <- 1:tmax

#
# Extract a few params
#

# Provinces <- unique(dat$PROVINCE)
Provinces <- c("Western Coral Triangle", "Tropical Northwestern Atlantic")
nProv <- length(Provinces)

LUs <- names(dat)[3:7]
print(LUs)
nLUs <- length(LUs)


# Calculate a yearly instantaneous rate  - USE HAMILTON AND CASEY DATA FOR DEF RATES 
dat <- within(dat, {
  def_rate_instant_HC <- log(AreaHC_2012_ha/AreaHC_2000_ha)/(12-0) # div length of time series 
  
  #Set def rate to zero for those with no HC data: 
  def_rate_instant_HC <- ifelse(is.nan(def_rate_instant_HC), 0,
                                    def_rate_instant_HC)
  def_percent_HC = 100*(exp(def_rate_instant_HC)-1) # Finite rate, for plotting
})

# ----------------- #
# Loop over provinces and years and calculate emissions 
# ----------------- #

#This produces a dataframe of emissions by all provinces at tmax years
#, and for each carbo type using each area estimate 
# It also has 'scenarios' for total (all landuses), 
#then 'partial effects' with each landuse iteratively removed. 
#To get total emissions, first filter for scnr == "Total"
# Then you can sum over columns (carbon types)/ rows (provinces) as required


Eout <- NULL
for (iyr in years){ #ugly loop, but turned out easier this way
  print(iyr)
for (i in 1:nProv){
  datt <- filter(dat, PROVINCE == Provinces[i])
  
  #Relative area converted
  propLU <- datt[,unlist(lapply(LUs, match, names(datt)))]/100
  
  fAGBLU <- datt[,unlist(lapply(paste0("AGB_", LUs), match, names(datt)))]
  fSOCLU <- datt[,unlist(lapply(paste0("SOC_", LUs), match, names(datt)))]
  
  #50% erosion 
  fAGBLU2 <- fAGBLU
  fSOCLU2 <- fSOCLU
  fAGBLU2$AGB_Erosion <- 0.5
  fSOCLU2$SOC_Erosion <- 0.5
  
  
  #Assumes names are in same order as above 
  cmaxdat <- with(datt, data.frame(LU = names(propLU),
                        propLU = as.numeric(propLU), 
                        Cmax_tree = as.numeric(TreeC_C02_ha  * fAGBLU),
                        Cmax_soil1m = as.numeric(SOC_1m_CO2_ha * fSOCLU),
                        Cmax_soil2m = as.numeric(SOC_2m_CO2_ha * fSOCLU),
                        Cmax_tree_no_erosion = as.numeric(TreeC_C02_ha  * fAGBLU2),
                        Cmax_soil2m_no_erosion = as.numeric(SOC_2m_CO2_ha * fSOCLU2),
                        Cmax_tree_field = as.numeric(Tree_field_CO2_ha* fAGBLU),
                        Cmax_soil_field = as.numeric(Soil_field_CO2_ha * fSOCLU)
  ))
  icols_sum <- 3:9
  # print(names(cmaxdat)[icols_sum])
  #Total emissions for each carbon type
  Edat <- cmaxdat %>% summarize_at(icols_sum, list(HC = ~emission_model(iyr, datt$AreaHC_2012_ha,
                                                                  -datt$def_rate_instant_HC,
                                                                  r = emission_rate,
                                                                  Cmax = sum(.*propLU),#cmax is sum of 
                                                                  # max carbon emissions by proportoin
                                                                  # of each land-use to be converted. 
                                                                  # this amounts to having deforestation rate
                                                                  # split in proportion to land-use conversion types
                                                                  s=0),
                                             B = ~emission_model(iyr, datt$Area_2010,
                                                                 -datt$def_rate_instant_HC,
                                                                 r = emission_rate,
                                                                 Cmax = sum(.*propLU),
                                                                 s=0),
                                   HC_seq = ~emission_model(iyr, datt$AreaHC_2012_ha,
                                                        -datt$def_rate_instant_HC,
                                                        r = emission_rate,
                                                        Cmax = sum(.*propLU),#cmax is sum of 
                                                        # max carbon emissions by proportoin
                                                        # of each land-use to be converted. 
                                                        # this amounts to having deforestation rate
                                                        # split in proportion to land-use conversion types
                                                        s=s_rate),
                                   B_seq = ~emission_model(iyr, datt$Area_2010,
                                                       -datt$def_rate_instant_HC,
                                                       r = emission_rate,
                                                       Cmax = sum(.*propLU),
                                                       s=s_rate))) %>%
    mutate(scnr = "Total")
  
  #
  # Partial emissions from each land-use 
  #
  #Run without each land-use, then subtract from total to get partials 
  
  for (iLU in 1:nLUs){
    cmaxtemp <- cmaxdat %>%
      filter(!(LU %in% LUs[iLU]))
    
    #Remove deforestation from particular LU
    dmod <- 
      sum(-datt$def_rate_instant_HC * cmaxtemp$propLU)
    Etemp <- cmaxtemp %>% #remove each LU
      summarize_at(icols_sum, list(HC = ~emission_model(iyr, datt$AreaHC_2012_ha,
                                                  dmod,
                                                  r = emission_rate,
                                                  Cmax = sum(.*propLU),
                                                  s=0),
                             B = ~emission_model(iyr, datt$Area_2010,
                                                 dmod,
                                                 r = emission_rate,
                                                 Cmax = sum(.*propLU),
                                                 s=0),
                             HC_seq = ~emission_model(iyr, datt$AreaHC_2012_ha,
                                                  dmod,
                                                  r = emission_rate,
                                                  Cmax = sum(.*propLU),
                                                  s=s_rate),
                             B_seq = ~emission_model(iyr, datt$Area_2010,
                                                 dmod,
                                                 r = emission_rate,
                                                 Cmax = sum(.*propLU),
                                                 s=s_rate)))
    #Now substract this from overall to get partial for this LU
    Etemp2 <- Edat[1,]
    icols <- 1:(4*length(icols_sum))
    Etemp2[,icols] <- Edat[1,icols] - Etemp[,icols]
    Etemp2$scnr <- LUs[iLU]
    #Fix zeros (numerical errors)
    Etemp2[,icols[which(Etemp2[,icols]<0)]] <- 0
    
    Edat <- bind_rows(Edat, Etemp2)
  }
  Edat$PROVINCE <- datt$PROVINCE
  Edat$Year <- iyr
  
  Eout <- c(Eout, list(Edat))
}
}

emdat_all_years <- bind_rows(Eout)

#
# Calculate fractions attributable to each landuse 
#

agb <- grep("AGB", names(dat))
soc <- grep("SOC_", names(dat))[1:5]

#Check order matches
stopifnot(substr(names(dat)[agb], 5, nchar(names(dat)[agb])) == substr(names(dat)[soc], 5, nchar(names(dat)[soc])))
stopifnot(substr(names(dat)[agb],5, nchar(names(dat)[agb])) == LUs)

luCmax <- as.matrix(
  (dat[,agb] * (dat[,LUs]/100) * dat$TreeC_C02_ha) + (dat[,soc] * (dat[,LUs]/100) * dat$SOC_2m_CO2_ha))
totals <- rowSums(luCmax)
fract_Cmax <- apply(luCmax, 2, function(x) x/totals)
rowSums(fract_Cmax) #should be 1 

fract_Cmax <- data.frame(fract_Cmax)
names(fract_Cmax) <- LUs
fract_Cmax$PROVINCE <- dat$PROVINCE

luCmax <- data.frame(luCmax) %>% mutate(PROVINCE = dat$PROVINCE)

write.csv(luCmax, "2020-05-17_emissions-LU.csv", row.names = FALSE)
write.csv(fract_Cmax, "2020-05-17_emissions-fractions-LU.csv", row.names = FALSE)

#
# Save results 
#

# save(emdat_all_years, file = "Outputs/2020-04-30_emissions-projections-fig3.rda")

emdat <- filter(emdat_all_years, Year == tmax)
save(fract_Cmax, file = "Outputs/2020-04-19_fraction-emission.rda")
save(emdat, file = "Outputs/2020-04-19_emissions-projections.rda")
# save(emdat_all_years, file = "Outputs/2020-04-19_emissions-projections-all-years.rda")
