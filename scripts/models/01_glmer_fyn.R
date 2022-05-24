# Libraries
library(tidyverse)
library(lme4)
library(car)
library(effects)
library(ggplot2)

#importing final dataset
final <- read_csv('output/final_df/final_df.csv')

# Load fynbos stacked df
fyn_df <- filter(final, indicator=='fynbos')

#number of indicator species
length(unique(fyn_df$Species_Code))

# add in year variable
fyn_df$Year <- lubridate::year(fyn_df$StartDate)
str(fyn_df)

# Option to run with Indicator Type as a fixed effect
# glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)

# measure time taken...
start_time <- Sys.time()
# run a mixed effects model; year = fixed; species = random
glmer1 <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = fyn_df)
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(glmer1)

# Anova
Anova(glmer1)

# Plot effects
plot(allEffects(glmer1))

#predict
fyn_df$predict <- predict(glmer1, fyn_df)

fm1 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)

ggplot(data=fyn_df, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

