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

# run a mixed effects model; year = fixed; species = random
# glm1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)
glm1 <- glmer(Presence ~ Year + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)
summary(glm1)

# Anova
Anova(glm1)

# Plot effects
plot(allEffects(glm1))



