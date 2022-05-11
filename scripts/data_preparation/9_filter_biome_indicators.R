library(tidyverse)

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

#### METHOD 2----

meth2 <- pres %>% filter(method == 'perc_cards')

meth2 %>% pivot_longer(cols = `Albany Thicket`:`Succulent Karoo`,
                       names_to = 'biome', values_to = 'perc') -> meth2_long

meth2_long %>% 
  group_by(species_id) %>%
  filter(perc == max(perc)) %>% 
  filter(!biome == 'Mixed') %>%
  filter(perc > 50) -> dom_vals_2

####

setdiff(dom_vals_2$species_id, dom_vals_1$species_id)
setdiff(dom_vals_1$species_id, dom_vals_2$species_id)
