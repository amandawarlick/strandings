library(tidyr)
library(ggmap)
library(data.table)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
library(captioner)
library(knitr)
library(reshape2)
library(stringr)
library(magrittr)

all.data.orig <- read.csv("1989_2015_raw.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE) %>%
  filter(State != 'CA') %>%
  select(c(National.Database.Number, Genus, Common.Name, Affiliation, City...from.strandings.table, County, State, Locality.Detail,
           Latitude, Latitude.Units, Longitude, Longitude.Units, Year.of.Observation, Observation.Date)) %>%
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
  filter(Mammal.Type == 'Pinniped')

all.data.orig$Longitude <- gsub("\\-", "", all.data.orig$Longitude)
all.data.orig$Longitude <- gsub("\\-", "", all.data.orig$Longitude)
all.data.orig$Longitude <- gsub("\\_", "", all.data.orig$Longitude)
all.data.orig$Longitude <- gsub("\\ ", ".", all.data.orig$Longitude)
#all.data.orig$Longitude <- gsub("<d0>", "", all.data.orig$Longitude)
all.data.orig$Latitude <- gsub(" ", ".", all.data.orig$Latitude)

all.data.orig$Longitude <- as.numeric(all.data.orig$Longitude)
all.data.orig$Longitude <- all.data.orig$Longitude * (-1)

sum(is.na(all.data.orig$Latitude))
sum(!is.na(all.data.orig$Latitude))
sum(is.na(all.data.orig$Longitude))
sum(!is.na(all.data.orig$Longitude))

#all.data.orig$Latitude <- as.numeric(all.data.orig$Latitude)

pinnipeds.data.raw <- all.data.orig %>% filter(Mammal.Type == 'Pinniped' & Common.Name != 'Seal, harp')

write.csv(pinnipeds.data.raw, file = "~/Documents/R/Strandings/pinnipeds.data.raw.csv", row.names = F)


