library(tidyverse)
library(sf)
library(magrittr)

## Pentads
pent <- read_sf('data/pentads/original/pentad_sa.shp')
head(pent)

## Read in template (this has min 20 cards, at least 5 in each half period)
template <- read_csv('output/template/template.csv')
length(unique(template$Pentad))

example <- read_csv(paste0('data/sabap_cards_raw/',181,'.csv'))
length(unique(example$Pentad))

## Identify species codes
fyn_select <- c("181", "749", "753", "855", "540", "869", "612", "870")

# load in each species df and add it to a list
fyn_list <- list()
for (i in fyn_select) {
  fyn_list[[i]] <- read_csv(paste0('data/sabap_cards_raw/',i,'.csv'))
}

## Filter all species dfs to the template cards
fyn_list_filt <- map(fyn_list, ~filter(.x, CardNo %in% template$CardNo))

# Clean up species column ----
# Convert any number to 1
# Convert any dash (-) to 0
for(i in 1:length(fyn_list_filt)){
  fyn_list_filt[[i]]$Presence <- gsub("(\\d+)", '1', fyn_list_filt[[i]]$Spp)
  fyn_list_filt[[i]]$Presence <- gsub("-", '0', fyn_list_filt[[i]]$Presence)
  fyn_list_filt[[i]]$Presence <- as.numeric(fyn_list_filt[[i]]$Presence)
}

# presence filter > 1 per pentad
# add on sum of presences
fyn_list_pres <- map(fyn_list_filt, ~.x %>% group_by(Pentad) %>% mutate(no_pres = sum(Presence)))

## Filter all species dfs to the have at least 1 presence per pentad
fyn_list_clean <- map(fyn_list_pres, ~filter(.x, no_pres > 1))

# stack them together into a single df
fyn_df <- bind_rows(fyn_list_clean, .id = 'Species_Code')

# Export stacked df
write_csv(fyn_df, 'output/biome_indicator_dfs/fynbos/fynbos_df.csv')

### TEST MODEL ----
library(lme4)
library(car)
library(effects)

# add in year variable
fyn_df$Year <- lubridate::year(fyn_df$StartDate)
str(fyn_df)

# run a mixed effects model; year = fixed; species = random
# glm1 <- glmer(Presence ~ Year*Indicator_Type + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)
glm1 <- glmer(Presence ~ Year + (1 + Year|Species_Code), family = 'binomial', data = fyn_df)
beepr::beep(4)
summary(glm1)

Anova(glm1)

# Plot effects
plot(allEffects(glm1))

# Note: make full df, with all birds and their indicator type
