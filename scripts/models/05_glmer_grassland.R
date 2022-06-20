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
grass_df <- filter(final, indicator=='grassland')

#number of indicator species
grass_df %>% filter(!Common_name %in% c("Flufftail, White-winged","Phalarope, Red", "Duck, Hybrid Red/Yellow-billed")) -> grass_df

length(unique(grass_df$Common_name))
unique(grass_df$Common_name)
unique(grass_df$Species_Code)


# add in year variable
grass_df$Year <- lubridate::year(grass_df$StartDate)
str(grass_df)

# Option to run with Indicator Type as a fixed effect
## glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = grass_df)

# measure time taken...
start_time <- Sys.time()

# run a mixed effects model; year = fixed; species = random
grass_glmer <- glmer(Presence ~ Year  + (Year|Species_Code) + (1|Pentad), family = 'binomial', data = grass_df)
beepr::beep()
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(grass_glmer)

# Anova
Anova(grass_glmer)

# Plot effects
plot(allEffects(grass_glmer))

#predict
grass_df$predict <- predict(grass_glmer, grass_df)

grass_clean <- grass_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))

ggplot(data=grass_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))

plot_model(grass_glmer, type = "re")
