library(tidyverse)
library(sf)
library(magrittr)

#creating a list of off south african species downloaded
spp <- list.files('data/sabap_cards_raw/',)
head(spp)

#loading in pentad template
template <- read_csv('output/template/template.csv')

#individually filtering each species file
for (i in spp) {
species <- read_csv(paste0('data/sabap_cards_raw/',i,'')) #loading in each file

species_filt <- filter(species, CardNo %in% template$CardNo) #fitting to the template

species_filt <- species_filt %>% dplyr::select(CardNo:StartDate, Pentad, Spp:Taxonomic_name) #getting rid of unneeded col

#creating a presence col with 1 for present on a card and 0 for absent on a card
species_filt$Presence <- gsub("(\\d+)", '1', species_filt$Spp) 
species_filt$Presence <- gsub("-", '0', species_filt$Presence)
species_filt$Presence <- as.numeric(species_filt$Presence)

#filtering pentads that have had atleast 1 presence in them for that species
species_filt <- species_filt %>% 
  group_by(Pentad) %>% 
  mutate(no_pres = sum(Presence))

species_filt_clean <- filter(species_filt, no_pres > 1)

#writing the output to a new folder
write_csv(species_filt_clean, paste0('data/sabap_cards_filt/',i,''))

}

#takes all of the species data from 29GB to just 3GB
#around 10 species or so had no presences in the pentads deemed "good quality" in the template