# Libraries
library(tidyverse)
library(lme4)
library(car)
library(effects)

# Load fynbos stacked df
fyn_df <- read_csv('output/biome_indicator_dfs/fynbos/fynbos_df.csv')

# add in year variable
fyn_df$Year <- lubridate::year(fyn_df$StartDate)
str(fyn_df)

# Option to run with Indicator Type as a fixed effect
# glmer1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)

# measure time taken...
start_time <- Sys.time()
# run a mixed effects model; year = fixed; species = random
glmer1 <- glmer(Presence ~ Year  + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)
end_time <- Sys.time()

# How long did this take to run?
end_time - start_time

# Summary of model
summary(glmer1)

# Anova
Anova(glmer1)

# Plot effects
plot(allEffects(glmer1))



