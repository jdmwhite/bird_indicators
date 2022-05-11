library(sf)
library(tidyverse)
library(patchwork)

## Pentads
pent <- read_sf('data/pentads/pentad_sa.shp')
head(pent)

## Cards
card <- read_csv('data/sabap_cards/test/151.csv')
card_null <- read_csv('data/sabap_cards/test/151_null.csv')
names(card_null)
# Clean cards ----

# Some pentads have a 'c' instead of an '_'
card_null$Pentad <- gsub("c", '_', card_null$Pentad)

card_null %>% filter(Pentad %in% pent$PENTADE) -> card_pents

card_pents %>% group_by(Pentad) %>% tally() %>% rename(PENTADE = Pentad) -> pent_n
head(pent_n)

####

pent %>% left_join(pent_n, by = 'PENTADE') -> pent

#### Plot results ----
ggplot() +
  geom_sf(data = pent, aes(fill = log(n)), lwd = 0) +
  scale_fill_viridis_c(name = 'log(Cards)') +
  theme_minimal() +

ggplot() +
  geom_sf(data = pent, aes(fill = n), lwd = 0) +
  scale_fill_viridis_c(name = 'Cards') +
  theme_minimal()

#### Save
ggsave('output/cards_per_pentad.png', width = 9.54, height = 4)

#### Clean up and export pentad sf
pent %>% select(PENTADE, n) %>% rename(Pentad = PENTADE) -> pent_clean

write_sf(pent_clean, 'output/pent_tally.shp')
