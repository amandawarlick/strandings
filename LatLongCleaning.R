options(java.parameters = "-Xmx80000m")
library(xlsx)
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

pinnipeds_data <- read.csv("pinnipeds_data.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

#Pacific northwest
all.data.orig <- read.csv("1989_2015_raw.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE) %>%
  filter(State != 'CA') %>%
  select(c(National.Database.Number, Field.Number, Genus, Common.Name, Affiliation, City...from.strandings.table, County, State, Locality.Detail,
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

all.data.orig$Longitude <- str_replace(all.data.orig$Longitude, "/", ".")
all.data.orig$Longitude <- str_replace(all.data.orig$Longitude, " ", ".")
all.data.orig$Longitude <- gsub("\\�", "", all.data.orig$Longitude)
all.data.orig$Longitude <- gsub("\\/", "", all.data.orig$Longitude)

all.data.orig$Latitude <- str_replace(all.data.orig$Latitude, "/", ".")
all.data.orig$Latitude <- str_replace(all.data.orig$Latitude, " ", ".")
all.data.orig$Latitude <- gsub("\\�", "", all.data.orig$Latitude)
all.data.orig$Latitude <- gsub("\\/", "", all.data.orig$Latitude)

# all.data.orig$Longitude <- gsub("\\-", "", all.data.orig$Longitude)
# all.data.orig$Longitude <- gsub("\\-", "", all.data.orig$Longitude)
# all.data.orig$Longitude <- gsub("\\_", "", all.data.orig$Longitude)
#all.data.orig$Longitude <- gsub("\\ ", ".", all.data.orig$Longitude)

all.data.orig$Latitude <- as.numeric(all.data.orig$Latitude)
all.data.orig$Longitude <- as.numeric(all.data.orig$Longitude)
all.data.orig$Longitude <- all.data.orig$Longitude * (-1)

sum(is.na(all.data.orig$Latitude))
sum(!is.na(all.data.orig$Latitude))
sum(is.na(all.data.orig$Longitude))
sum(!is.na(all.data.orig$Longitude))

mean_lat_long <- pinnipeds_data %>%
  filter(County != 'NA') %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  group_by(County) %>%
  summarize(mean_lat = mean(Latitude, na.rm = T), mean_long = mean(Longitude, na.rm = T))

all.data.orig <- all.data.orig %>%
  select(-c(Latitude.Units, Longitude.Units, Genus, Mammal.Type)) %>%
  filter(!is.na(Latitude)) %>%
  merge(mean_lat_long, by = "County")

Lat_Long_ordered <- all.data.orig %>%
  transform(Long_error = mean_long - Longitude,
            Lat_error = mean_lat - Latitude) %>%
  filter(Long_error < -1.4 | Long_error > 1 | Lat_error < -.5 | 
           Lat_error > .5 | Latitude > 49.05) %>%
  select(Field.Number, National.Database.Number, Affiliation, Common.Name, Year.of.Observation, Observation.Date,
         State, County, City...from.strandings.table, Locality.Detail, Latitude, mean_lat,
         Lat_error, Longitude, mean_long, Long_error) %>%
  arrange(Latitude)

write.csv(Lat_Long_ordered, file = "~/Documents/R/Strandings/Lat_Long_ordered.csv", row.names = F)

###California

#Read in
all.data.orig.ca <- read.csv("CA_data_raw.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

all.data.orig.ca <- all.data.orig.ca %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  transform(Longitude = Longitude*(-1))

mean_lat_long_ca <- all.data.orig.ca %>%
  filter(Latitude > 31.5 & Latitude < 42) %>%
  filter(Longitude > -125 & Longitude < -116) %>%
  group_by(County) %>%
  summarize(mean_lat = mean(Latitude, na.rm = T), mean_long = mean(Longitude, na.rm = T))

Lat_Long_ordered_ca <- all.data.orig.ca %>%
  merge(mean_lat_long_ca, by = "County") %>%
  transform(Long_error = mean_long - Longitude,
            Lat_error = mean_lat - Latitude) %>%
  filter(Long_error < -1 | Long_error > 1 | Lat_error < -.5 | Lat_error > .5) %>%
  select(Field.Number, National.Database.Number, Affiliation, Common.Name, Observation.Date,
         State, County, City...from.strandings.table, Locality.Detail, Latitude, mean_lat,
         Lat_error, Longitude, mean_long, Long_error) %>%
  arrange(Latitude)

write.csv(Lat_Long_ordered_ca, file = "~/Documents/R/Strandings/Lat_Long_ordered_ca.csv", row.names = F)

