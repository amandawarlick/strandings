
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

#MEI from http://www.esrl.noaa.gov/psd/enso/mei/table.html
MEI <- read.csv("MEI_89_15.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE) %>%
  melt(id.vars = "X", variable.name = "MONTH") %>%
  rename(Year.of.Observation = X, MEI = value, Month.of.Observation = MONTH) %>%
  transform(ENSO = ifelse(MEI < 0, "La Nina", ifelse(MEI > 0, "El Nino", '')))

all.data <- read.csv("Strandings1989_2015.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE) %>%
  filter(State != 'CA') %>%
  select(-c(Field.Number, Nmfs.Regional.Number, Confidence.Code, Report.Status, Latitude.Units, Longitude.Units, 
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
  merge(MEI, by = c("Month.of.Observation", "Year.of.Observation"), all = T) #check this merge


all.data$Longitude <- gsub("\\-", "", all.data$Longitude)
all.data$Longitude <- gsub("\\-", "", all.data$Longitude)
all.data$Longitude <- gsub("\\_", "", all.data$Longitude)
all.data$Longitude <- gsub("\\ ", ".", all.data$Longitude)
all.data$Latitude <- gsub(" ", ".", all.data$Latitude)

all.data$Longitude <- as.numeric(all.data$Longitude)
all.data$Longitude <- all.data$Longitude * (-1)

all.data$Latitude <- as.numeric(all.data$Latitude)

##Numbered months in case ever needed.
# all.data$Num.Month <- with(all.data,
#   ifelse(Month.of.Observation == 'JAN', 1, ifelse(Month.of.Observation == 'FEB', 2, ifelse(Month.of.Observation == 'MAR', 3,
#   ifelse(Month.of.Observation == 'APR', 4, ifelse(Month.of.Observation == 'MAY', 5, ifelse(Month.of.Observation == 'JUN', 6, 
#   ifelse(Month.of.Observation == 'JUL', 7, ifelse(Month.of.Observation == 'AUG', 8, ifelse(Month.of.Observation == 'SEP', 9,
#   ifelse(Month.of.Observation == 'OCT', 10, ifelse(Month.of.Observation == 'NOV', 11, ifelse(Month.of.Observation == 'DEC', 12, ''))))))))))))


#Only pinniped data

pinnipeds.data <- all.data %>% filter(Mammal.Type == 'Pinniped' & Common.Name != 'Seal, harp')

pinnipeds.data$Age.Class[is.na(pinnipeds.data$Age.Class)] <- "NA"
pinnipeds.data$Age.Class <- gsub("NA", "Unid", pinnipeds.data$Age.Class)
pinnipeds.data$Age.Class <- gsub("UNKNOWN", "Unid", pinnipeds.data$Age.Class)
pinnipeds.data$Age.Class <- gsub("PUP/CALF", "PUP", pinnipeds.data$Age.Class)
pinnipeds.data$Age.Class <- case(pinnipeds.data$Age.Class)

pinnipeds.data$Sex[is.na(pinnipeds.data$Sex)] <- "NA"
pinnipeds.data$Sex <- gsub("NA", "Unid", pinnipeds.data$Sex)
pinnipeds.data$Sex <- gsub("UNKNOWN", "Unid", pinnipeds.data$Sex)
pinnipeds.data$Sex <- case(pinnipeds.data$Sex)

write.csv(pinnipeds.data, file = "~/Documents/R/Strandings/pinnipeds.data.csv", row.names = F)
#write.csv(pinnipeds.data, file = "~/Strands/pinnipeds.data.csv", row.names = F)

```