

setwd("~/Documents/Research/Strandings/ENSO_Mapping/Env Data")

###Fish

MSquid <- read.csv("MSquid_N.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
Herring <- read.csv("Herring_NCC.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
Age1Sard <- read.csv("Age1Sard_NCC.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
AGe1Anch <- read.csv("Age1Anch_NCC.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
#Copepod biomass anomaly north, monthly, log(anomaly), 44N
CBA_North <- read.csv("CBA_North.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 
CBA_South <- read.csv("CBA_South.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) 

fish <- CBA_South %>% 
  merge(CBA_North, by = 'Date', all = T) %>% 
  merge(MSquid, by = 'Date', all = T) %>% 
  merge(Herring, by = 'Date', all = T) %>%  
  merge(AGe1Anch, by = 'Date', all = T) %>% 
  merge(Age1Sard, by = 'Date', all = T) %>%
  transform(Date = as.Date(Date)) 
fish$Year.of.Observation <- as.numeric(format(as.Date(fish$Date, format = "%Y-%M-%D"),"%Y"))
fish$Mo <- as.numeric(format(as.Date(fish$Date, format = "%Y-%m-%D"),"%m"))
fish <- fish %>% filter(Year.of.Observation > 2002) %>%
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
  dplyr::select(-Mo)


###Ocean 
#http://oceanview.pfeg.noaa.gov/cciea-table/?opentab=1

#Multivariate ENSO Index
MEI <- read.csv("MEI.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
NOI <- read.csv("NOI.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  select(Date, NOI) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#North Pac Gyre Osc Index
NPGO <- read.csv("NPGO.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#Pacific Decadal Osc Index
PDO <- read.csv("PDO.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#SST degrees C, 39N
SST_39 <- read.csv("SST_39.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#SST degrees C, 44N
SST_44 <- read.csv("SST_44.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#Upwelling Index, m^3/s/100m coastline, 39N
Upwell_39 <- read.csv("Upwell_39.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#Upwelling Index, m^3/s/100m coastline, 45N
Upwell_45 <- read.csv("Upwell_45.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)%>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d")) 
#Meridional Wind, m/s 39N
Wind_39 <- read.csv("Wind_39.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))
#Meridional Wind, m/s 44N
Wind_44 <- read.csv("Wind_44.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  transform(Date = sub('T.*', '', Date)) %>%
  transform(Date = as.Date(Date, format = "%Y-%m-%d"))

ocean <- MEI %>% merge(NPGO, by = 'Date') %>%
  merge(NOI, by = 'Date') %>%
  merge(PDO, by = 'Date') %>% 
  merge(SST_39, by = 'Date') %>%
  merge(SST_44, by = 'Date') %>% merge(Upwell_39, by = 'Date') %>% 
  merge(Upwell_45, by = 'Date') %>% merge(Wind_39, by = 'Date') %>% 
  merge(Wind_44, by = 'Date') %>%
  #transform(Date = as.Date(Date)) %>%
  transform(ENSO_cat = ifelse(MEI < 0, "La Nina", ifelse(MEI > 0, "El Nino", '')))
ocean$Year.of.Observation <- as.numeric(format(as.Date(ocean$Date, format = "%Y-%M-%D"),"%Y"))
ocean$Mo <- as.numeric(format(as.Date(ocean$Date, format = "%Y-%m-%D"),"%m"))
ocean <- ocean %>% filter(Year.of.Observation > 2002) %>%
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
  dplyr::select(-Mo)

write.csv(ocean, file = "./ocean.csv", row.names = F)

#manually adjust date column for lags
ocean_full <- read.csv("ocean.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE) %>%
  filter(Year.of.Observation > 2002) 

# ocean_full$Date_lag1 <- as.numeric(format(as.Date(ocean_full$Date_lag1, format = "%m/%d/%y")))
# ocean_full$Date_lag3 <- as.numeric(format(as.Date(ocean_full$Date_lag3, format = "%m/%d/%y")))
# ocean_full$Date_lag6 <- as.numeric(format(as.Date(ocean_full$Date_lag6, format = "%m/%d/%y")))
# # ocean_full$Mo_lag1 <- as.numeric(format(as.Date(ocean_full$Date_lag1, format = "%Y-%m-%D"),"%m"))
# # ocean_full$Mo_lag3 <- as.numeric(format(as.Date(ocean_full$Date_lag3, format = "%Y-%m-%D"),"%m"))
# # ocean_full$Mo_lag6 <- as.numeric(format(as.Date(ocean_full$Date_lag6, format = "%Y-%m-%D"),"%m"))


#Derive mean SST values
meanSST <- ocean %>%
  dplyr::select(matches("SST"), Month.of.Observation, -matches("_lag")) %>%
  group_by(Month.of.Observation) %>%
  dplyr::summarize(SST_39_mean = mean(SST_39, na.rm = T),
            SST_44_mean = mean(SST_44, na.rm = T))
#Add means into df and subtract from values for anomalies
ocean_full <- ocean_full %>%
  merge(meanSST, by = 'Month.of.Observation') %>%
  transform(SST_39_anom = SST_39 - SST_39_mean) %>%
  transform(SST_44_anom = SST_44 - SST_44_mean) %>%
  dplyr::select(-c(SST_39_mean, SST_44_mean))

# anom_lags <- ocean %>% dplyr::select(Date, Month.of.Observation, SST_39_anom, SST_44_anom) %>%
#   arrange(Date)
# write.csv(anom_lags, "anom_lags.csv", row.names = F)

#Manually added CBA in here, since it is the only "fish" variable that is monthly - may use the other "fish" later
# temp_anom_CBA_lags <- read.csv("temp_anom_CBA_lags.csv", header = T, na.strings = "NA", stringsAsFactors = F) %>%
#   transform(Date = as.Date(Date, format = "%m/%d/%Y"))

# ocean_full <- ocean_full %>%
#   #merge(temp_anom_CBA_lags, by = 'Date') %>%
#   transform(Month.of.Observation = Month.of.Observation.y,
#             Year.of.Observation = Year.of.Observation.y) %>%
#   dplyr::select(-c(Month.of.Observation.x, Year.of.Observation.x, Month.of.Observation.y, Year.of.Observation.y)) 
 
setwd("~/Documents/Research/Strandings/ENSO_Mapping/Env data")

write.csv(ocean_full, file = "./ocean_full.csv", row.names = F)
write.csv(fish, file = "./fish.csv", row.names = F)


