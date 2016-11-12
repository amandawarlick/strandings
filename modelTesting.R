# Pinniped Strandings 
# Regression Model Development
# Sean Warlick
# November 12, 2016
###############################################################################

# Library Load ----
library(ggplot2)
library(GGally)
library(dplyr)

# Data Load ----

# Data is pre-cleaned
pinnipeds.data <- read.csv("pinnipeds.data.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# Set Up Factors
pinnipeds.data$Age.Class <- factor(pinnipeds.data$Age.Class, 
          levels = c("Pup", "Yearling", "Subadult", "Adult", "Unid"))

pinnipeds.data$Month.of.Observation <- factor(pinnipeds.data$Month.of.Observation, 
          levels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'))

# Model Month By Strandings with Fisheries Interactions. --------
#Is there an effect of month on FI?
monthly.FI <- pinnipeds.data %>%
  filter(Fishery.Interaction == 'Y') %>%
  filter(Observation.Status == 'ALIVE' | Observation.Status == 'FRESH DEAD') %>%
  group_by(Year.of.Observation, Month.of.Observation) %>%
  summarize(cnt = n_distinct(National.Database.Number))

# Fit Model
model <- lm(cnt ~ Month.of.Observation, data = monthly.FI)
summary(model)

# Evaluate Model
par(mfrow = c(2, 2))
plot(model, which = 1:4)




# Monthly Yearly Species ---------------

# Prep Data For Modling 
monthly.species.HI <- pinnipeds.data %>%
  filter(Observation.Status == 'ALIVE' | Observation.Status == 'FRESH DEAD') %>%
  filter(Findings.of.Human.Interaction == 'Y') %>%
  group_by(Month.of.Observation, Pinniped.Common.Name, Year.of.Observation) %>%
  summarize(cnt = n_distinct(National.Database.Number)) 

# Visual Exploration of Data
ggplot(monthly.species.HI, aes(x = Year.of.Observation, y = cnt)) +
	geom_point() +
	geom_smooth() +
	facet_wrap(~Pinniped.Common.Name)


# Create Model
model1 <- lm(cnt ~ Year.of.Observation + Pinniped.Common.Name, data = monthly.species.HI)

summary(model1)

# Evaluate Model
par(mfrow = c(2, 2))
plot(model, which = 1:4)
 # Non Linear Fit