
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

pnw_1989_2015 <- read.csv("Strandings1989_2015.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE) %>% filter(State != 'CA')
pnw_2016 <- read.csv("OR_WA_2016.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

pnw_2016$Longitude <- gsub("\\-", "", pnw_2016$Longitude)
pnw_2016$Longitude <- gsub("\\-", "", pnw_2016$Longitude)
pnw_2016$Longitude <- gsub("\\_", "", pnw_2016$Longitude)
pnw_2016$Longitude <- gsub("\\ ", ".", pnw_2016$Longitude)
pnw_2016$Latitude <- gsub(" ", ".", pnw_2016$Latitude)

pnw_1989_2015$Longitude <- gsub("\\-", "", pnw_1989_2015$Longitude)
pnw_1989_2015$Longitude <- gsub("\\-", "", pnw_1989_2015$Longitude)
pnw_1989_2015$Longitude <- gsub("\\_", "", pnw_1989_2015$Longitude)
pnw_1989_2015$Longitude <- gsub("\\ ", ".", pnw_1989_2015$Longitude)
pnw_1989_2015$Latitude <- gsub(" ", ".", pnw_1989_2015$Latitude)

all_data_pnw <- bind_rows(pnw_1989_2015, pnw_2016)

resting_test <- all_data_pnw %>%
  filter(Observation.Status == 'ALIVE') %>%
  group_by(Year.of.Observation, Left.at.Site) %>%
  summarize(cnt = n_distinct(National.Database.Number))

all_data_pnw$Longitude <- as.numeric(all_data_pnw$Longitude)
all_data_pnw$Longitude <- all_data_pnw$Longitude * (-1)
all_data_pnw$Latitude <- as.numeric(all_data_pnw$Latitude)

all_data_ca <- read.csv("CAdata.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)
all_data_ca$Longitude <- as.numeric(all_data_ca$Longitude)
all_data_ca$Longitude <- all_data_ca$Longitude * (-1)
all_data_ca$Latitude <- as.numeric(all_data_ca$Latitude)

all_data <- bind_rows(all_data_pnw, all_data_ca) %>%
  dplyr::select(-c(Nmfs.Regional.Number, Confidence.Code, Report.Status, Latitude.Units, Longitude.Units, 
            Latitude.Actual.Estimate, Longitude.Actual.Estimate, How.lat.long.determined, Group.Event.Number,
            Examiner.Name, Stranding.Agreement.Authority, Body.of.Water, Group.Event.Flag, Group.Type..Mass.Stranding,
            Mass.Stranding.Number, Region, SW.Fishery.Type, SW.Other.Human.Int.Type, Group.Type..Cow.Calf.Pair,
            Inaccesible.Flag, Unknown.CBD.Flag, Immediate.Release.at.Site, Relocated.Flag, Died.at.Site.Flag, 
            Euthanized.at.Site.Flag, Left.at.Site, Condition.at.Examination, Observation.Date,
            Length.actual.estimate, Weight.actual.estimate, Transferred.to.Rehabilitation, Deemed.Healthy.Releasable.Flag)) %>%
  transform(Mammal.Type = 
        ifelse(grepl('Phoca', Genus), 'Pinniped',
        ifelse(grepl('Steller', Common.Name), 'Pinniped',
        ifelse(grepl('Zalophus', Genus), 'Pinniped',
        ifelse(grepl('Mirounga', Genus), 'Pinniped',
        ifelse(grepl('Arctocephalus', Genus), 'Pinniped',
        ifelse(grepl('Callorhinus', Genus), 'Pinniped', 
        ifelse(grepl('Pinniped', Common.Name), 'Pinniped',
        ifelse(grepl('Otariid', Common.Name), 'Pinniped',
        ifelse(grepl('Phocid', Common.Name), 'Pinniped', 
        ifelse(grepl('PHOCID', Common.Name), 'Pinniped', 
        ifelse(grepl('OTARIID', Common.Name), 'Pinniped',
        ifelse(grepl('PINNIPED', Common.Name), 'Pinniped', 'Cetacean'))))))))))))) %>%
  transform(Interaction.Type = 
        ifelse(Fishery.Interaction == 'Y', 'Fisheries',
            ifelse(Shot == 'Y', 'Gunshot',
                 ifelse(Boat.Collision == 'Y', 'Boat', 
                      ifelse(Other.Human.Interaction == 'Y', "Other", 'NA'))))) %>%
  transform(Pinniped.Common.Name = 
        ifelse(Common.Name == 'Sea lion, California', 'California sea lion',
             ifelse(Common.Name == 'Seal, harbor', 'Harbor seal',
                  ifelse(Common.Name == 'Sea lion, Steller', 'Steller sea lion',
                       ifelse(Common.Name == 'Seal, Guadalupe fur', 'Guadalupe fur seal',
                             ifelse(Common.Name == 'Seal, northern elephant', 'Northern elephant seal',
                                  ifelse(Common.Name == 'Seal, Northern fur', 'Northern fur seal', 'Unidentified'))))))) %>%
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

#Regional ifelse statement cleaning
test <- all_data %>%
  filter(is.na(Water.Body)) %>%
  summarize(n_distinct(National.Database.Number))
  
  
#Only pinniped data

cetaceans <- all_data %>% filter(Mammal.Type != 'Pinniped' & Common.Name != 'Seal, harp')
pinnipeds_data_all <- all_data %>% filter(Mammal.Type == 'Pinniped' & Common.Name != 'Seal, harp')

pinnipeds_data_all$Age.Class[is.na(pinnipeds_data_all$Age.Class)] <- "NA"
pinnipeds_data_all$Age.Class <- gsub("NA", "Unid", pinnipeds_data_all$Age.Class)
pinnipeds_data_all$Age.Class <- gsub("UNKNOWN", "Unid", pinnipeds_data_all$Age.Class)
pinnipeds_data_all$Age.Class <- gsub("PUP/CALF", "PUP", pinnipeds_data_all$Age.Class)
pinnipeds_data_all$Age.Class <- case(pinnipeds_data_all$Age.Class)

pinnipeds_data_all$Sex[is.na(pinnipeds_data_all$Sex)] <- "NA"
pinnipeds_data_all$Sex <- gsub("NA", "Unid", pinnipeds_data_all$Sex)
pinnipeds_data_all$Sex <- gsub("UNKNOWN", "Unid", pinnipeds_data_all$Sex)
pinnipeds_data_all$Sex <- case(pinnipeds_data_all$Sex)

#OR and WA only for further data cleaning
pinnipeds_data <- pinnipeds_data_all %>% filter(State != 'CA')

mean_lat_long <- pinnipeds_data %>%
  filter(County != 'NA') %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  group_by(County) %>%
  summarize(mean_lat = mean(Latitude, na.rm = T), mean_long = mean(Longitude, na.rm = T))

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
# sum(is.na(pinnipeds_data$long_corr))
# testreplace <- pinnipeds_data[pinnipeds_data$Latitude != pinnipeds_data$lat_corr,]
# testreplace <- testreplace %>% filter(!is.na(Latitude))

#California data
pinnipeds_data_ca <- pinnipeds_data_all %>% filter(State == 'CA')

write.csv(pinnipeds_data, file = "~/Documents/R/Strandings/pinnipeds_data.csv", row.names = F)
write.csv(pinnipeds_data_ca, file = "~/Documents/R/Strandings/pinnipeds_data_ca.csv", row.names = F)

write.csv(pinnipeds_data_all, file = "~/Documents/R/Strandings/ENSO_Mapping/pinnipeds_data_all.csv", row.names = F)

```