# Libraries
library(tidyverse)
library(lme4)
library(car)
library(effects)
library(ggplot2)
library(beepr)
library(sjPlot)
library(glmmTMB)

#importing final dataset
final <- read_csv('output/final_df/final_df.csv')

# Load fynbos stacked df
sav_df <- filter(final, indicator=='savanna')

#number of indicator species
length(unique(sav_df$Species_Code))

# add in year variable
sav_df$Year <- lubridate::year(sav_df$StartDate)
str(sav_df)

# Option to run with Indicator Type as a fixed effect
# glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = sav_df)

# measure time taken...
start_time <- Sys.time()
# run a mixed effects model; year = fixed; species = random
glm_sav <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = sav_df)
beepr::beep()
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(glm_sav)

# Anova
Anova(glm_sav)

# Plot effects
plot(allEffects(glm_sav))

#predict
sav_df$predict <- predict(glm_sav, sav_df)

sav_clean <- sav_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))

ggplot(data=sav_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

plot_model(glm_sav, type = "re")

