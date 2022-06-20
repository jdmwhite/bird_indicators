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

#Load fynbos stacked df
coast_df <- filter(final, indicator=='coastal')

#number of indicator species
coast_df %>% filter(!Common_name %in% c()) -> coast_df

length(unique(coast_df$Common_name))
unique(coast_df$Common_name)
unique(coast_df$Species_Code)

# add in year variable
coast_df$Year <- lubridate::year(coast_df$StartDate)
str(coast_df)

# Option to run with Indicator Type as a fixed effect
## glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = coast_df)

# measure time taken...
start_time <- Sys.time()

# run a mixed effects model; year = fixed; species = random
coast_glmer <- glmer(Presence ~ Year  + (Year|Species_Code) + (1|Pentad), family = 'binomial', data = coast_df)
beepr::beep()
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(coast_glmer)

# Anova
Anova(coast_glmer)

# Plot effects
plot(allEffects(coast_glmer))

#predict
coast_df$predict <- predict(coast_glmer, coast_df)

coast_clean <- coast_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))

ggplot(data=coast_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

plot_model(coast_glmer, type = "re")
