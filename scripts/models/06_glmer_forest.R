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
forest_df <- filter(final, indicator=='forest')

#number of indicator species
forest_df %>% filter(!Common_name %in% c("Pigeon, Eastern Bronze-naped")) -> forest_df

length(unique(forest_df$Common_name))
unique(forest_df$Common_name)
unique(forest_df$Species_Code)

# add in year variable
forest_df$Year <- lubridate::year(forest_df$StartDate)
str(forest_df)

# Option to run with Indicator Type as a fixed effect
## glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = forest_df)

# measure time taken...
start_time <- Sys.time()

# run a mixed effects model; year = fixed; species = random
forest_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = forest_df)
beepr::beep()
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(forest_glmer)

# Anova
Anova(forest_glmer)

# Plot effects
plot(allEffects(forest_glmer))

#predict
forest_df$predict <- predict(forest_glmer, forest_df)

forest_clean <- forest_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))

ggplot(data=forest_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

plot_model(forest_glmer, type = "re")

