library(tidyverse)
library(sf)
library(magrittr)

## Pentads
pent <- read_sf('data/pentads/original/pentad_sa.shp')
head(pent)

# Biome Indicator Birds ----
pres_df <- read_csv('data/presence_values/full_presence_biome_and_cards.csv')

pres_df %>% filter(biome == 'Fynbos' | biome == 'Nama-Karoo') -> example_ind
indicator_spp <- example_ind

## Read in template (this has min 20 cards, at least 5 in each half period)
template <- read_csv('output/template/template.csv')

# Read in example Pentad
example <- read_csv(paste0('data/sabap_cards_raw/',181,'.csv'))

# How many pentads have we removed?
length(unique(pent$PENTADE)) # all SA pentads
length(unique(example$Pentad)) # all pentads in the raw SABAP file
length(unique(template$Pentad)) # all pentads in the filtered file

## Identify species codes
indicator_spp$species_id

#NOTE: we need to take a different approach here...
# Consider running the template filter, adding in 1/0 presence on each species, adding in whether they are an indicator or general, saving a new csv for each and then load them in one at a time, append them to the csv and then remove them from local memory.
# I don't think R can easily handle loading in 286 indicator species in one go. 286*30mb each = 8.5gb
# = list of 286, which gets 
# Actually, this would be ALL species, so it will be MUCH bigger! Way better to process each individually. 

# load in each species df and add it to a list
indicator_list <- list()
for (i in indicator_spp$species_id) {
  indicator_list[[i]] <- read_csv(paste0('data/sabap_cards_raw/',i,'.csv'))
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

