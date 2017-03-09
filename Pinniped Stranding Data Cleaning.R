
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

all_data_pnw <- bind_rows(pnw_1989_2015, pnw_2016)
all_data_pnw$Longitude <- gsub("\\-", "", all_data_pnw$Longitude)
all_data_pnw$Longitude <- gsub("\\-", "", all_data_pnw$Longitude)
all_data_pnw$Longitude <- gsub("\\_", "", all_data_pnw$Longitude)
all_data_pnw$Longitude <- gsub("\\ ", ".", all_data_pnw$Longitude)
all_data_pnw$Latitude <- gsub(" ", ".", all_data_pnw$Latitude)

all_data_pnw$Longitude <- as.numeric(all_data_pnw$Longitude)
all_data_pnw$Longitude <- all_data_pnw$Longitude * (-1)
all_data_pnw$Latitude <- as.numeric(all_data_pnw$Latitude)

all_data_ca <- read.csv("CAdata.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)
all_data_ca$Longitude <- as.numeric(all_data_ca$Longitude)
all_data_ca$Longitude <- all_data_ca$Longitude * (-1)
all_data_ca$Latitude <- as.numeric(all_data_ca$Latitude)



all_data <- bind_rows(all_data_pnw, all_data_ca) %>%
  select(-c(Nmfs.Regional.Number, Confidence.Code, Report.Status, Latitude.Units, Longitude.Units, 
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
                                  County == 'Clallam' & City != 'Sequim' & City != 'Port Angeles' & City != 'Port Townsend' & 
                                    Affiliation != 'Feiro Marine Life Center' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' | 
                                  County == 'Jefferson' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' &
                                  City != 'Brinnon' & City != 'Quilcene' & City != 'Port Townsend', 'WA_Coast',
                                ifelse(State == 'OR', 'OR_Coast', 
                                       ifelse(State == 'CA', 'CA Coast', 'Inland_WA'))))

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

pinnipeds_data <- pinnipeds_data_all %>% filter(State != 'CA')
pinnipeds_data_ca <- pinnipeds_data_all %>% filter(State == 'CA')

write.csv(pinnipeds_data, file = "~/Documents/R/Strandings/pinnipeds_data.csv", row.names = F)
write.csv(pinnipeds_data_ca, file = "~/Documents/R/Strandings/pinnipeds_data_ca.csv", row.names = F)

write.csv(pinnipeds_data_all, file = "~/Documents/R/Strandings/ENSO_Mapping/pinnipeds_data_all.csv", row.names = F)

```