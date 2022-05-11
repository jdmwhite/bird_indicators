library(sf)
library(tidyverse)
library(magrittr)
library(ggsci)
library(patchwork)

#### load pentads ----
pent_cov <- read_sf('data/pent_biome_cover/pent_biome_cover.shp')

#### Start loop ----

# Load in the file names
file_names <- list.files('data/sabap_cards_raw/')
file_names %<>% str_remove('.csv') # remove the .csv suffix to only have the file numbers

# ii <- file_names[6]
# file_names[1]

for(ii in file_names){
#### Load SABAP data ----
bird <- read_csv(paste0('data/sabap_cards_raw/',ii,'.csv'))

#### view bird data

#### APPROACH 1 - just presences ----
#### filter to pentads
bird <- bird %>% filter(Pentad %in% pent_cov$PENTADE)
# remove absences
bird_pres <- bird %>% filter(!Spp == '-')

###### NEXT STATEMENT!
## If there are no presences at all for the species, move to the next one
if (length(bird_pres$Spp) == 0){
  next
}

# filter the pentad dataset to only include pentads with bird presences
pent_cov %>% filter(PENTADE %in% bird_pres$Pentad) -> pent_w_pres


# summarise
freq_df <- as.data.frame((table(pent_w_pres$dom_biome)))
names(freq_df) <- c('biome', 'freq_pres')
freq_df$perc_pres <- freq_df$freq_pres/sum(freq_df$freq_pres) * 100
freq_df$perc_pres <- round(freq_df$perc_pres)

#### APPROACH 2 number cards per pentad ----
pent_cov %>% select(PENTADE, dom_biome) %>% rename(Pentad = PENTADE) %>% st_drop_geometry() -> pent_dom_biome

# join biome cover dataset onto bird presence dataset
bird_pres %>% left_join(pent_dom_biome, by = 'Pentad') -> bird_biome

#### Count biome presences
bird_biome_freq <- as.data.frame(table(bird_biome$dom_biome))
names(bird_biome_freq) <- c('biome', 'freq_cards')
bird_biome_freq$perc_cards <- bird_biome_freq$freq_cards/sum(bird_biome_freq$freq_cards) * 100
bird_biome_freq$perc_cards <- round(bird_biome_freq$perc_cards)


#### Join the two methods together
freq_df %>% left_join(bird_biome_freq, by = 'biome') -> freq_comb

## Make a vector of all the biome columns
cols_to_add <- c(Mixed = NA_real_, Savanna = NA_real_, Grassland = NA_real_, `NA` = NA_real_, Other = NA_real_, `Nama-Karoo` = NA_real_, `Indian Ocean Coastal Belt` = NA_real_, `Succulent Karoo` = NA_real_, Forests = NA_real_, Fynbos = NA_real_, `Albany Thicket` = NA_real_)

# Reorganise the df and add a species column
freq_comb %>% select(biome, perc_pres, perc_cards) %>% pivot_longer(perc_pres:perc_cards) %>% pivot_wider(names_from = biome) %>% rename(method = name) %>% add_column(species_id = unique(bird_pres$Spp), common_name = unique(bird_pres$Common_name),taxonomic_name = unique(bird_pres$Taxonomic_name)) -> freq_wide

# Fill in the remaining columns and reorder and replace na with 0
freq_wide %>% add_column(!!!cols_to_add[!names(cols_to_add) %in% names(.)]) %>% select(order(colnames(.))) %>% relocate(c('species_id', 'common_name','taxonomic_name','method')) %>% replace(is.na(.), 0) -> freq_all_biomes

names(freq_all_biomes)


colnames(freq_all_biomes)

# Write to csv and add new rows for each species
write.table(freq_all_biomes, file = 'output/biome_pres/biome_pres_31032022.csv',
            append = TRUE, col.names = !file.exists('output/biome_pres/biome_pres_31032022.csv'), row.names = FALSE, sep = ',')


}


