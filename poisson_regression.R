# Pinniped Stranding Data
# Possion Regression For Strandings
# Sean Warlick
# Date 11/19/16
###############################################################################

# Library Loads 
library(ggplot2)
library(dplyr)

# Data Load 
###############################################################################
# Data is pre-cleaned
pinnipeds.data <- read.csv("pinnipeds.data.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Set Up Factors
pinnipeds.data$Age.Class <- factor(pinnipeds.data$Age.Class, 
          levels = c("Pup", "Yearling", "Subadult", "Adult", "Unid"))

pinnipeds.data$Month.of.Observation <- factor(pinnipeds.data$Month.of.Observation, 
          levels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'))

# Model Data Preperation: Aggregate by Year species
###############################################################################

# Prep Data Modeling ----
yearly_species <- pinnipeds.data %>%
  filter(Findings.of.Human.Interaction == 'Y') %>%
  group_by(Pinniped.Common.Name, Year.of.Observation) %>%
  summarize(cnt = n_distinct(National.Database.Number)) 


# Fit Saturated Model ----
yr_species_model <- glm(cnt ~ Pinniped.Common.Name + Year.of.Observation + Pinniped.Common.Name*Year.of.Observation, family = poisson(link = log), data = yearly_species)

# Model Summary
summary(yr_species_model)

# LRT test for Overall Significance: H0 is that all Beta = 0
d <- yr_species_model$null.dev - yr_species_model$dev
c('LRT' = d, "p value" = 1 - pchisq(d, 120 - 107))
# Results indicate that at least on coefficent is not equal to zero.

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
d2 <- yr_species_model2$null.dev - yr_species_model2$dev
c('LRT' = d2, "p value" = 1 - pchisq(d2, 120 - 113))

# Pearson GOF
pearson_resid2 <- residuals(yr_species_model2, type = "pearson")
pgof2 <- sum(pearson_resid2 ^ 2)

c("Chi Squared: " = pgof2, "p value:" = 1 - pchisq(pgof2, df = 121 - 8))

# Deviance GOF
dev2 <- yr_species_model2$dev
c('D:' = dev2, "p value" = 1 - pchisq(dev2, df = 121 - 8))

# GOF results for the independent model indicate the fit is not sufficent.  