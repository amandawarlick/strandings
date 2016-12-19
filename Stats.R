# Pinniped Stranding Data
# Possion Regression For Strandings
# Sean Warlick
# Date 11/19/16
###############################################################################

# Library Loads 
library(ggplot2)
library(dplyr)
setwd("~/Documents/R/Strandings")

# Data Load 
###############################################################################
# Data is pre-cleaned
pinnipeds_data <- read.csv("pinnipeds_data.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Set Up Factors
pinnipeds_data$Age.Class <- factor(pinnipeds_data$Age.Class, 
                                   levels = c("Pup", "Yearling", "Subadult", "Adult", "Unid"))

pinnipeds_data$Month.of.Observation <- factor(pinnipeds_data$Month.of.Observation, 
                      levels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'))

# Model Data Preparation: Aggregate by Year, species
###############################################################################

# Prep Data Modeling ----
yearly_species <- pinnipeds_data %>%
  filter(Findings.of.Human.Interaction == 'Y' & Pinniped.Common.Name != 'Unidentified') %>%
  group_by(Pinniped.Common.Name, Year.of.Observation) %>%
  summarize(cnt = n_distinct(National.Database.Number)) 

# Fit Saturated Model ----
yr_species_model <- glm(cnt ~ Pinniped.Common.Name + Year.of.Observation + Pinniped.Common.Name*Year.of.Observation, 
                        family = poisson(link = log), data = yearly_species)

# Model Summary
summary(yr_species_model)

# LRT test for Overall Significance: H0 is that all Beta = 0
d <- yr_species_model$null.dev - yr_species_model$dev
c('LRT' = d, "p value" = 1 - pchisq(d, 120 - 107))
# Results indicate that at least one coefficent is not equal to zero.

# Pearson GOF
pearson_resid <- residuals(yr_species_model, type = "pearson")
pgof <- sum(pearson_resid ^ 2)

## Null Hypothesis is that the fit is sufficent.
## Degrees of freedom are number of unique covariate patterns minus number of prams (including intercept)
c("Chi Squared: " = pgof, "p value:" = 1 - pchisq(pgof, df = 121 - 14))

# Deviance GOF
dev <- yr_species_model$dev
c('D:' = dev, "p value" = 1 - pchisq(dev, df = 107))

# Fit Independent Model ----
yr_species_model2 <- glm(cnt ~ Pinniped.Common.Name + Year.of.Observation, family = poisson(link = log), data = yearly_species)

# Model Summary
summary(yr_species_model2)

# LRT Test For Model Signifigance
d2 <- yr_species_model2$null.dev - yr_species_m

c("Chi Squared: " = pgof2, "p value:" = 1 - pchisq(pgof2, df = 121 - 8))

# Deviance GOFodel2$dev
c('LRT' = d2, "p value" = 1 - pchisq(d2, 120 - 113))

# Pearson GOF
pearson_resid2 <- residuals(yr_species_model2, type = "pearson")
pgof2 <- sum(pearson_resid2 ^ 2)
dev2 <- yr_species_model2$dev
c('D:' = dev2, "p value" = 1 - pchisq(dev2, df = 121 - 8))

# GOF results for the independent model indicate the fit is not sufficent.

#### Questions

## 1. Changes in mean annual strandings per year
# Are mean annual combined strandings or each species increasing over time?  timeseries_all_model, timeseries_all_model_sp
# Are mean annual human interaction cases increasing over time for:
        # - combined species? timeseries_HI_model
        # - certain HI types? timeseries_HI_type_model
        # - for individual species?  timeseries_HI_model_sp
# **Is the prevalence of human interaction cases increasing or decreasing over time for certain species? timeseries_HIprev_model_sp
## If yes, which types of cases?  HItype_model_sp

## 2. Seasonality
# Is there a seasonal peak in stranding cases for certain species?  monthly_all_model_sp
# Same but for human interactions cases?  monthly_HI_model_sp

## 3. Spatial distribution
# Is there a statistical difference in strandings/HI across counties? ORcounty_model, WAcounty_model
# If yes, ideally, would show which are different from the "baseline" of overall stranding cases, but struggled with this.
# Are gunshot, entanglements, or boat collisions increasing in certain counties? 
# Is there a difference in regional distribution of species?

## 4. Age/Sex characteristics
# Is there a difference in sex of strandings/HI cases?  sex_all_mo_model (also shows month), sex_HI_model
# Is there a difference in age class strandings/HI cases?  age_model

#age_sex_table has counts and percs of age classes for each sex









