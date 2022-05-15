library(tidyverse)
library(dplyr)

#importing the presence file with the all different indicators
pres_df <- read_csv('data/presence_values/full_presence_biome_and_cards.csv')

#filtering the dataframe to get specific indicators for the different biomes
pres_df %>% filter(biome == 'Fynbos') -> fyn_spp
pres_df %>% filter(biome == 'Savanna') -> sav_spp
pres_df %>% filter(biome == 'Grassland') -> grass_spp
pres_df %>% filter(biome == 'Nama-Karoo') -> karoo_spp

#importing the marine, wetland, coastal, and forest indicators created using roberts
marine <-  read_csv('data/other_indicators/marine_species.csv')
wetland <- read_csv('data/other_indicators/wetland_species.csv')
coastal <- read_csv('data/other_indicators/coastal_species.csv')
forest <- read_csv('data/other_indicators/forest_species.csv')

#creating a list of all the SABAP2 references from the roberts lists
biome_list <- c(marine$Ref, coastal$Ref,forest$Ref,wetland$Ref)

#filtering out any matches in the quantitative indicators with the roberts lists
fyn_filt <- filter(fyn_spp, !fyn_spp$species_id %in% biome_list)
grass_filt <- filter(grass_spp, !grass_spp$species_id %in% biome_list)
sav_filt <- filter(sav_spp, !sav_spp$species_id %in% biome_list)
karoo_filt <- filter(karoo_spp, !karoo_spp$species_id %in% biome_list)

#writing csvs for the filtered indicators
write_csv(fyn_filt, 'output/indicator_spp/fynbos.csv')
write_csv(grass_filt, 'output/indicator_spp/grassland.csv')
write_csv(sav_filt, 'output/indicator_spp/savanna.csv')
write_csv(karoo_filt,'output/indicator_spp/karoo.csv')
