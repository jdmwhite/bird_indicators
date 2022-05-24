# Libraries
library(tidyverse)
library(lme4)
library(car)
library(effects)
library(ggplot2)
library(beepr)

#importing final dataset
final <- read_csv('output/final_df/final_df.csv')

# Load fynbos stacked df
karoo_df <- filter(final, indicator=='karoo')

#number of indicator species
length(unique(karoo_df$Species_Code))

# add in year variable
karoo_df$Year <- lubridate::year(karoo_df$StartDate)
str(karoo_df)

# Option to run with Indicator Type as a fixed effect
# glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = karoo_df)

# measure time taken...
start_time <- Sys.time()
# run a mixed effects model; year = fixed; species = random
glm_karoo <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = karoo_df)
beepr::beep()
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(glm_karoo)

# Anova
Anova(glm_karoo)

# Plot effects
plot(allEffects(glm_karoo))

#predict
karoo_df$predict <- predict(glm_karoo, karoo_df)

karoo_clean <- karoo_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))

ggplot(data=karoo_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

