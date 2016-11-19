# Pinniped Stranding Data
# Possion Regression For Strandings
# Sean Warlick
# Date 11/19/16
###############################################################################

# Library Loads 
library(ggplot2)
library(dplyr)

# Data Load ----

# Data is pre-cleaned
pinnipeds.data <- read.csv("pinnipeds.data.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Set Up Factors
pinnipeds.data$Age.Class <- factor(pinnipeds.data$Age.Class, 
          levels = c("Pup", "Yearling", "Subadult", "Adult", "Unid"))

pinnipeds.data$Month.of.Observation <- factor(pinnipeds.data$Month.of.Observation, 
          levels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'))

# Model Data Preperation: Aggregate by Year species ----

## Prep Data Modeling
yearly_species <- pinnipeds.data %>%
  filter(Findings.of.Human.Interaction == 'Y') %>%
  group_by(Pinniped.Common.Name, Year.of.Observation) %>%
  summarize(cnt = n_distinct(National.Database.Number)) 

## Fit Models

# Saturate Model
yr_species_model <- glm(cnt ~ Pinniped.Common.Name + Year.of.Observation + Pinniped.Common.Name*Year.of.Observation, family = poisson(link = log), data = yearly_species)

# Independent Model
yr_species_model2 <- glm(cnt ~ Pinniped.Common.Name + Year.of.Observation, family = poisson(link = log), data = yearly_species)

# The Null Model
yr_species_model3 <- glm(cnt ~ 1, family = poisson(link = log), data = yearly_species)

summary(yr_species_model)
summary(yr_species_model2)
summary(yr_species_model3)

## Look at Model Residuals 
round(c(yr_species_model$dev, yr_species_model$null.dev),4)
round(c(yr_species_model2$dev, yr_species_model2$null.dev),4)
round(c(yr_species_model3$dev, yr_species_model3$null.dev),4)

## Goodness of Fit
pchisq(yr_species_model$deviance, df = yr_species_model$df.residual, lower.tail = FALSE)








