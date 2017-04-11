

setwd("~/Documents/R/Strandings/ENSO_Mapping/Env Data")

###Fish

#Anchovy CPUE, natural log of catch+1, 44-46N
Anch_CPUE <- read.csv("Anch_CPUE.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Anchovy CPUE, natural log of catch+1, calCOFI lines?
Anchovy_South <- read.csv("Anchovy_South.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Copepod biomass anomaly north, monthly, log(anomaly), 44N
CBA_North <- read.csv("CBA_North.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Copepod biomass anomaly south, monthly, log(anomaly), 44N
CBA_South <- read.csv("CBA_South.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Ratio of running mean of spawning escapements
Chin_growth <- read.csv("Chin_growth.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Chinook escapement anomalies, Columbia River
Chinook_escape <- read.csv("Chinook_escape.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Total coastwide coastal pelagics landings, thousand mt
CPS_lands <- read.csv("CPS_lands.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Herring CPUE nat log catch+1, 44N
Herring_CPUE <- read.csv("Herring_CPUE.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Total salmon landings, thousand mt, pacfin
Salmon_lands <- read.csv("Salmon_lands.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Sardine CPUE nat log catch+1, 44N
Sardine_CPUE <- read.csv("Sardine_CPUE.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Sardine CPUE nat log catch+1, CalCOFI line
Sardine_South <- read.csv("Sardine_South.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Total coastwide comm and rec fisheries landings thousand mt
Tot_lands <- read.csv("Tot_lands.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)

fish <- Anch_CPUE %>% merge(Anchovy_South, by = 'Date', all = T) %>%
  merge(CBA_North, by = 'Date', all = T) %>% merge(CBA_South, by = 'Date', all = T) %>%
  merge(Chin_growth, by = 'Date', all = T) %>% merge(Chinook_escape, by = 'Date', all = T) %>% 
  merge(CPS_lands, by = 'Date', all = T) %>% merge(Herring_CPUE, by = 'Date', all = T) %>% 
  merge(Salmon_lands, by = 'Date', all = T) %>% merge(Sardine_South, by = 'Date', all = T) %>% 
  merge(Sardine_CPUE, by = 'Date', all = T) %>% merge(Tot_lands, by = 'Date', all = T) %>%
  transform(Date = as.Date(Date)) 
fish$Year.of.Observation <- as.numeric(format(as.Date(fish$Date, format = "%Y-%M-%D"),"%Y"))
fish$Mo <- as.numeric(format(as.Date(fish$Date, format = "%Y-%m-%D"),"%m"))
fish <- fish %>% filter(Year.of.Observation > 1988) %>%
  transform(Month.of.Observation = ifelse(Mo == 1, 'JAN', 
                                          ifelse(Mo == 2, 'FEB',
                                                 ifelse(Mo == 3, 'MAR',
                                                        ifelse(Mo == 4, 'APR',
                                                               ifelse(Mo == 5, 'MAY',
                                                                      ifelse(Mo == 6, 'JUN',
                                                                             ifelse(Mo == 7, 'JUL',
                                                                                    ifelse(Mo == 8, 'AUG',
                                                                                           ifelse(Mo == 9, 'SEP',
                                                                                                  ifelse(Mo == 10, 'OCT',
                                                                                                         ifelse(Mo == 11, 'NOV', 'DEC')))))))))))) %>% 
  select(-Mo)


###Ocean 
#http://oceanview.pfeg.noaa.gov/cciea-table/?opentab=1

#Multivariate ENSO Index
MEI <- read.csv("MEI.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#North Pac Gyre Osc Index
NPGO <- read.csv("NPGO.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Pacific Decadal Osc Index
PDO <- read.csv("PDO.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#SST degrees C, 39N
SST_39 <- read.csv("SST_39.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#SST degrees C, 44N
SST_44 <- read.csv("SST_44.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Upwelling Index, m^3/s/100m coastline, 39N
Upwell_39 <- read.csv("Upwell_39.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Upwelling Index, m^3/s/100m coastline, 45N
Upwell_45 <- read.csv("Upwell_45.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Meridional Wind, m/s 39N
Wind_39 <- read.csv("Wind_39.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Meridional Wind, m/s 44N
Wind_44 <- read.csv("Wind_44.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 

ocean <- MEI %>% merge(NPGO, by = 'Date', all = T) %>%
  merge(PDO, by = 'Date', all = T) %>% merge(SST_39, by = 'Date', all = T) %>%
  merge(SST_44, by = 'Date', all = T) %>% merge(Upwell_39, by = 'Date', all = T) %>% 
  merge(Upwell_45, by = 'Date', all = T) %>% merge(Wind_39, by = 'Date', all = T) %>% 
  merge(Wind_44, by = 'Date', all = T) %>%
  transform(Date = as.Date(Date)) %>%
  transform(ENSO_cat = ifelse(MEI < 0, "La Nina", ifelse(MEI > 0, "El Nino", '')))
ocean$Year.of.Observation <- as.numeric(format(as.Date(ocean$Date, format = "%Y-%M-%D"),"%Y"))
ocean$Mo <- as.numeric(format(as.Date(ocean$Date, format = "%Y-%m-%D"),"%m"))
ocean <- ocean %>% filter(Year.of.Observation > 1988) %>%
  transform(Month.of.Observation = ifelse(Mo == 1, 'JAN', 
                                          ifelse(Mo == 2, 'FEB',
                                                 ifelse(Mo == 3, 'MAR',
                                                        ifelse(Mo == 4, 'APR',
                                                               ifelse(Mo == 5, 'MAY',
                                                                      ifelse(Mo == 6, 'JUN',
                                                                             ifelse(Mo == 7, 'JUL',
                                                                                    ifelse(Mo == 8, 'AUG',
                                                                                           ifelse(Mo == 9, 'SEP',
                                                                                                  ifelse(Mo == 10, 'OCT',
                                                                                                         ifelse(Mo == 11, 'NOV', 'DEC')))))))))))) %>% 
  select(-Mo)

#write.csv(ocean, file = "./ocean.csv", row.names = F)

#Lag 1 month, manually moved oceanographic data one month down, same below
ocean_lag1 <- read.csv("ocean_lag1.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
ocean_lag1$Date <- as.Date(ocean_lag1$Date, format = "%m/%d/%y")
ocean_lag1$Year.of.Observation <- as.numeric(format(as.Date(ocean_lag1$Date, format = "%Y-%M-%D"),"%Y"))
ocean_lag1$Mo <- as.numeric(format(as.Date(ocean_lag1$Date, format = "%Y-%m-%D"),"%m"))
ocean_lag1 <- ocean_lag1 %>% filter(Year.of.Observation > 1988) %>%
  transform(Month.of.Observation = ifelse(Mo == 1, 'JAN', 
                                          ifelse(Mo == 2, 'FEB',
                                                 ifelse(Mo == 3, 'MAR',
                                                        ifelse(Mo == 4, 'APR',
                                                               ifelse(Mo == 5, 'MAY',
                                                                      ifelse(Mo == 6, 'JUN',
                                                                             ifelse(Mo == 7, 'JUL',
                                                                                    ifelse(Mo == 8, 'AUG',
                                                                                           ifelse(Mo == 9, 'SEP',
                                                                                                  ifelse(Mo == 10, 'OCT',
                                                                                                         ifelse(Mo == 11, 'NOV', 'DEC')))))))))))) %>% 
  select(-Mo)

ocean_lag2 <- read.csv("ocean_lag2.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
ocean_lag2$Date <- as.Date(ocean_lag2$Date, format = "%m/%d/%y")
ocean_lag2$Year.of.Observation <- as.numeric(format(as.Date(ocean_lag2$Date, format = "%Y-%M-%D"),"%Y"))
ocean_lag2$Mo <- as.numeric(format(as.Date(ocean_lag2$Date, format = "%Y-%m-%D"),"%m"))
ocean_lag2 <- ocean_lag2 %>% filter(Year.of.Observation > 1988) %>%
  transform(Month.of.Observation = ifelse(Mo == 1, 'JAN', 
                                          ifelse(Mo == 2, 'FEB',
                                                 ifelse(Mo == 3, 'MAR',
                                                        ifelse(Mo == 4, 'APR',
                                                               ifelse(Mo == 5, 'MAY',
                                                                      ifelse(Mo == 6, 'JUN',
                                                                             ifelse(Mo == 7, 'JUL',
                                                                                    ifelse(Mo == 8, 'AUG',
                                                                                           ifelse(Mo == 9, 'SEP',
                                                                                                  ifelse(Mo == 10, 'OCT',
                                                                                                         ifelse(Mo == 11, 'NOV', 'DEC')))))))))))) %>% 
  select(-Mo)

ocean_lag3 <- read.csv("ocean_lag3.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
ocean_lag3$Date <- as.Date(ocean_lag3$Date, format = "%m/%d/%y")
ocean_lag3$Year.of.Observation <- as.numeric(format(as.Date(ocean_lag3$Date, format = "%Y-%M-%D"),"%Y"))
ocean_lag3$Mo <- as.numeric(format(as.Date(ocean_lag3$Date, format = "%Y-%m-%D"),"%m"))
ocean_lag3 <- ocean_lag3 %>% filter(Year.of.Observation > 1988) %>%
  transform(Month.of.Observation = ifelse(Mo == 1, 'JAN', 
                                          ifelse(Mo == 2, 'FEB',
                                                 ifelse(Mo == 3, 'MAR',
                                                        ifelse(Mo == 4, 'APR',
                                                               ifelse(Mo == 5, 'MAY',
                                                                      ifelse(Mo == 6, 'JUN',
                                                                             ifelse(Mo == 7, 'JUL',
                                                                                    ifelse(Mo == 8, 'AUG',
                                                                                           ifelse(Mo == 9, 'SEP',
                                                                                                  ifelse(Mo == 10, 'OCT',
                                                                                                         ifelse(Mo == 11, 'NOV', 'DEC')))))))))))) %>% 
  select(-Mo)

ocean_full <- ocean %>%
  merge(ocean_lag1, by = c('Year.of.Observation', 'Month.of.Observation'), all = T, suffixes = c("", "_lag1")) %>%
  merge(ocean_lag2, by = c('Year.of.Observation', 'Month.of.Observation'), all = T, suffixes = c("", "_lag2")) %>%
  merge(ocean_lag3, by = c('Year.of.Observation', 'Month.of.Observation'), all = T, suffixes = c("", "_lag3"))

setwd("~/Documents/R/Strandings/ENSO_Mapping")

write.csv(ocean_full, file = "./ocean.csv", row.names = F)
write.csv(fish, file = "./fish.csv", row.names = F)


