library(tidyverse)
library(magrittr)
library(sf)

pent_cover <- read_sf('data/pent_biome_cover/pent_biome_cover.shp')

#### Read in biome dominance values
pres <- read_csv('output/biome_pres/biome_pres_31032022.csv')
names(pres)
pres$method

#### METHOD 1----
meth1 <- pres %>% filter(method == 'perc_pres')

meth1 %>% pivot_longer(cols = `Albany Thicket`:`Succulent Karoo`,
                       names_to = 'biome', values_to = 'perc') -> meth1_long

meth1_long %>% 
  group_by(species_id) %>%
  filter(perc == max(perc)) %>% 
  filter(!biome == 'Mixed') %>%
  filter(perc > 50) -> dom_vals_1

####
dom_vals_1$species_id
# ii <- dom_vals_1$species_id[1]

for(ii in dom_vals_1$species_id) {
  
  # load bird data
  bird <- read_csv(paste0('data/sabap_cards_raw/',ii,'.csv'))
  
  #### Select out pentads for the biome that species is >50% in
  spp_row <- dom_vals_1 %>% filter(species_id == ii)
  
  pent_cover %>% st_drop_geometry() %>% filter(dom_biome == spp_row$biome) %>% select(PENTADE) -> biome_pentad_id
  #### Filter sabap data by biome pentads
  bird %>% filter(Pentad %in% biome_pentad_id$PENTADE) -> bird_filt
  
  #### Change spp values to pres/abs
  bird_filt$Presence <- gsub("(\\d+)", "1", bird_filt$Spp)
  bird_filt$Presence <- gsub("-", "0", bird_filt$Presence)
  
  perc_pres <- as.numeric(table(bird_filt$Presence)[2] / (table(bird_filt$Presence)[1] + table(bird_filt$Presence)[2]) * 100)
  presences <- as.numeric(table(bird_filt$Presence)[2])
  absences <- as.numeric(table(bird_filt$Presence)[1])
  presence_vals <- data.frame(ii ,presences, absences, perc_pres)
  names(presence_vals)[1] <- 'spp'
  
  # write_csv(presence_vals, paste0('output/presence_values/',ii,'.csv'))
  write.table(presence_vals, file = 'data/presence_values/presence_values.csv',
              append = TRUE, col.names = !file.exists('data/presence_values/presence_values.csv'), row.names = FALSE, sep = ',')

}

#### END ----