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
wet_df <- filter(final, indicator=='wetland')

#number of indicator species
wet_df %>% filter(!Common_name %in% c()) -> wet_df

length(unique(wet_df$Common_name))
unique(wet_df$Common_name)
unique(wet_df$Species_Code)


# add in year variable
wet_df$Year <- lubridate::year(wet_df$StartDate)
str(wet_df)
# Option to run with Indicator Type as a fixed effect
## glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = wet_df)

# measure time taken...
start_time <- Sys.time()
# run a mixed effects model; year = fixed; species = random
wet_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = wet_df)
beepr::beep()
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(wet_glmer)

# Anova
Anova(wet_glmer)

# Plot effects
plot(allEffects(wet_glmer))

#predict
wet_df$predict <- predict(wet_glmer, wet_df)

wet_clean <- wet_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))

ggplot(data=wet_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

plot_model(wet_glmer, type = "re")


