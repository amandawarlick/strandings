
library(knitr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(reshape2)
library(xtable)
library(dplyr)

##Load stranding data, remove unnecessary columns, make pinniped/cetacean designation, fix common name to remove comma
setwd("~/Documents/R/Strandings/")
#setwd("~/Strands")

case <- function(x)
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

pinnipeds_data_added <- read.csv("pinnipeds_data_protected.csv", stringsAsFactors = F)

pinnipeds_data <- pinnipeds_data_added %>%
  transform(Interaction.Type = 
        ifelse(Fishery.Interaction == 'Y', 'Fisheries',
            ifelse(Shot == 'Y', 'Gunshot',
                 ifelse(Boat.Collision == 'Y', 'Boat', 
                      ifelse(Other.Human.Interaction == 'Y', "Other", 'NA'))))) %>%
  transform(Water.Body = ifelse(County == 'Pacific' | County == 'Grays Harbor' | 
                                  County == 'Clallam' & Affiliation == 'Makah Fisheries Management' & City == 'La Push' & City == 'Clallam Bay' & City == 'Neah Bay' & 
                                    City == 'Olympic National Park' & City != 'Sequim' & City != 'Port Angeles' & City != 'Port Townsend' & 
                                    Affiliation != 'Feiro Marine Life Center' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' | 
                                  County == 'Jefferson' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' &
                                  City == 'Dosewallips' & City == 'Port Ludlow' & City != 'Brinnon' & City != 'Quilcene' & City != 'Port Townsend', 'WA_Coast',
                                ifelse(State == 'OR', 'OR_Coast', 
                                       ifelse(State == 'CA', 'CA Coast', 'Inland_WA')))) %>% 
  transform(Short_name = ifelse(Pinniped.Common.Name == 'California sea lion', 'CSL', 
                                 ifelse(Pinniped.Common.Name == 'Harbor seal', 'HS', 
                                        ifelse(Pinniped.Common.Name == 'Guadalupe fur seal', 'GFS',
                                               ifelse(Pinniped.Common.Name == 'Northern fur seal', 'NFS',
                                                      ifelse(Pinniped.Common.Name == 'Steller sea lion', 'SSL',
                                                             ifelse(Pinniped.Common.Name == 'Northern elephant seal', 'NES', 'NA')))))))


mean_lat_long <- pinnipeds_data %>%
  filter(County != 'NA') %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  group_by(County) %>%
  summarize(mean_lat = mean(Latitude, na.rm = T), mean_long = mean(Longitude, na.rm = T)) %>%
  filter(!is.na(mean_long))

#Applying mean lat/longs for erroneous or missing
pinnipeds_data <- pinnipeds_data %>%
  merge(mean_lat_long, by = 'County', all = T) %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  transform(Long_error = mean_long - as.numeric(Longitude),
            Lat_error = mean_lat - as.numeric(Latitude)) %>%
  transform(long_corr = ifelse(is.na(Longitude) | 
                                 Long_error < -1.5 | Long_error > 1.2, mean_long, Longitude)) %>%
  transform(lat_corr = ifelse(is.na(Latitude) | 
                                Lat_error < -.5 | Lat_error > .5, mean_lat, Latitude))

#only 2 missing lat/longs, most values replaced were in Island, which is only 0.5 latitude long
sum(is.na(pinnipeds_data$long_corr))
# testreplace <- pinnipeds_data[pinnipeds_data$Latitude != pinnipeds_data$lat_corr,]
# testreplace <- testreplace %>% filter(!is.na(Latitude))

write.csv(pinnipeds_data, file = "~/Documents/R/Strandings/pinnipeds_data_protected.csv", row.names = F)


```