library(tidyverse)
library(magrittr)

#### 
biome_pres <- read_csv('output/biome_pres/biome_pres_31032022.csv')
names(biome_pres)
biome_pres$method

#### METHOD 1----
meth1 <- biome_pres %>% filter(method == 'perc_pres')

meth1 %>% pivot_longer(cols = `Albany Thicket`:`Succulent Karoo`,
                       names_to = 'biome', values_to = 'perc') -> meth1_long

meth1_long %>% 
  group_by(species_id) %>%
  filter(perc == max(perc)) %>% 
  filter(!biome == 'Mixed') %>%
  filter(perc > 50) -> dom_vals_1

names(dom_vals_1)[6] <- 'biome_dom_perc'

###
pres <- read_csv('data/presence_values/presence_values.csv')
names(pres)[1] <- 'species_id'

###
pres_join <- dom_vals_1 %>% left_join(pres, by = 'species_id')

# write_csv(pres_join, 'data/presence_values/full_presence_biome_and_cards.csv')

####
# pres_join %>% filter(perc_pres > quantile(pres_join$perc_pres)[3]) -> pres_quantile

pres_join %>% filter(biome == 'Fynbos') -> fynbos_birds
pres_join %>% filter(biome == 'Grassland') -> grass_birds
pres_join %>% filter(biome == 'Savanna') -> savan_birds
pres_join %>% filter(biome == 'Nama-Karoo') -> karoo_birds
