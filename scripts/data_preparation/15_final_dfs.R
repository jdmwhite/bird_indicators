library(tidyverse)
library(dplyr)

###Coastal species----
coastal <- read_csv('data/other_indicators/coastal_species.csv')

coastal_ind <- c(coastal$Ref)
coastal_ind <- sort(coastal_ind)

coastal_spp <- list()

for(i in coastal_ind){
  coastal_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv'))
}

coastal_clean <- coastal_spp[lapply(coastal_spp, length) > 0]

names(coastal_clean) <- coastal_ind

for(i in 1:length(coastal_clean)){
  coastal_clean[[i]]$indicator <- 'coastal'
}

coastal_df <- bind_rows(coastal_clean, .id = 'Species_Code')

###Forest Species----
forest <- read_csv('data/other_indicators/forest_species.csv')

#if there are cards that arent present in the filtered list have to remove them here
forest <- filter(forest, Ref != 591)

forest_ind <- c(forest$Ref)
forest_ind <- sort(forest_ind)

print(forest_ind)

forest_spp <- list()

for(i in forest_ind){
  try(forest_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv')))
}

forest_clean <- forest_spp[lapply(forest_spp, length) > 0]

names(forest_clean) <- forest_ind

for(i in 1:length(forest_clean)){
  forest_clean[[i]]$indicator <- 'forest'
}

forest_df <- bind_rows(forest_clean, .id = 'Species_Code')

###Wetland species----
wetland <- read_csv('data/other_indicators/wetland_species.csv')

#if there are cards that arent present in the filtered list have to remove them here
wetland <- filter(wetland, Ref != 591)

wetland_ind <- c(wetland$Ref)
wetland_ind <- sort(wetland_ind)

print(wetland_ind)

wetland_spp <- list()

for(i in wetland_ind){
  try(wetland_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv')))
}

wetland_clean <- wetland_spp[lapply(wetland_spp, length) > 0]

names(wetland_clean) <- wetland_ind

for(i in 1:length(wetland_clean)){
  wetland_clean[[i]]$indicator <- 'wetland'
}

wetland_df <- bind_rows(wetland_clean, .id = 'Species_Code')

###fynbos species----
fynbos <- read_csv('output/indicator_spp/fynbos.csv')

#if there are cards that arent present in the filtered list have to remove them here
fynbos <- filter(fynbos, species_id != 466)

fynbos_ind <- c(fynbos$species_id)
fynbos_ind <- sort(fynbos_ind)

print(fynbos_ind)

fynbos_spp <- list()

for(i in fynbos_ind){
  try(fynbos_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv')))
}

fynbos_clean <- fynbos_spp[lapply(fynbos_spp, length) > 0]

names(fynbos_clean) <- fynbos_ind

for(i in 1:length(fynbos_clean)){
  fynbos_clean[[i]]$indicator <- 'fynbos'
}

fynbos_df <- bind_rows(fynbos_clean, .id = 'Species_Code')

###grassland species----
grassland <- read_csv('output/indicator_spp/grassland.csv')

#if there are cards that arent present in the filtered list have to remove them here
grassland <- filter(grassland, species_id != 885,species_id != 886)

grassland_ind <- c(grassland$species_id)
grassland_ind <- sort(grassland_ind)

print(grassland_ind)

grassland_spp <- list()

for(i in grassland_ind){
  try(grassland_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv')))
}

grassland_clean <- grassland_spp[lapply(grassland_spp, length) > 0]

names(grassland_clean) <- grassland_ind

for(i in 1:length(grassland_clean)){
  grassland_clean[[i]]$indicator <- 'grassland'
}

grassland_df <- bind_rows(grassland_clean, .id = 'Species_Code')

###karoo species----
karoo <- read_csv('output/indicator_spp/karoo.csv')

#if there are cards that arent present in the filtered list have to remove them here
karoo <- filter(karoo, species_id != 885,species_id != 886)

karoo_ind <- c(karoo$species_id)
karoo_ind <- sort(karoo_ind)

print(karoo_ind)

karoo_spp <- list()

for(i in karoo_ind){
  try(karoo_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv')))
}

karoo_clean <- karoo_spp[lapply(karoo_spp, length) > 0]

names(karoo_clean) <- karoo_ind

for(i in 1:length(karoo_clean)){
  karoo_clean[[i]]$indicator <- 'karoo'
}

karoo_df <- bind_rows(karoo_clean, .id = 'Species_Code')

###savanna species----
savanna <- read_csv('output/indicator_spp/savanna.csv')

#if there are cards that arent present in the filtered list have to remove them here
savanna <- filter(savanna, species_id != 787,species_id != 111)

savanna_ind <- c(savanna$species_id)
savanna_ind <- sort(savanna_ind)

print(savanna_ind)

savanna_spp <- list()

for(i in savanna_ind){
  try(savanna_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv')))
}

savanna_clean <- savanna_spp[lapply(savanna_spp, length) > 0]

names(savanna_clean) <- savanna_ind

for(i in 1:length(savanna_clean)){
  savanna_clean[[i]]$indicator <- 'savanna'
}

savanna_df <- bind_rows(savanna_clean, .id = 'Species_Code')

###Final DF----
final_df <- bind_rows(coastal_df,forest_df,wetland_df, fynbos_df,grassland_df,karoo_df,savanna_df)

write_csv(final_df, 'output/final_df/final_df.csv')

###Other species----
final_df <- read_csv('output/final_df/final_df.csv')
all_spp <- read_csv('data/sabap_cards_raw/country_southafrica_specieslist.csv')

indicator_species <- c(unique(final_df$Species_Code))

other_species <- filter(all_spp, !Ref %in% indicator_species)

#if there are cards that arent present in the filtered list have to remove them here
other_species <- filter(other_species, Ref != 0, Ref !=9, Ref !=10, Ref !=14,
                        Ref !=28, Ref !=39, Ref !=111, Ref !=116, Ref !=153, 
                        Ref !=180, Ref !=249, Ref !=332, Ref !=356, Ref !=446, 
                        Ref !=466, Ref !=577, Ref !=651, Ref !=787, Ref !=796, 
                        Ref !=885, Ref !=886, Ref !=991, Ref !=911, Ref !=933, 
                        Ref !=953, Ref !=983, Ref !=1015, Ref !=1022, Ref !=1027, 
                        Ref !=1102, Ref !=1185, Ref !=1236, Ref !=1334, 
                        Ref !=1338, Ref !=1636, Ref !=1664, Ref !=2046, Ref !=3852, 
                        Ref !=4155, Ref !=10002, Ref !=10013, Ref !=10019)

other_ind <- c(other_species$Ref)
other_ind <- sort(other_ind)

other_spp <- list()

for(i in other_ind){
  other_spp[[i]] <- read_csv(paste0('data/sabap_cards_filt/',i,'.csv'))
}

other_clean <- other_spp[lapply(other_spp, length) > 0]

names(other_clean) <- other_ind

for(i in 1:length(other_clean)){
  other_clean[[i]]$indicator <- 'other'
}

other_df <- bind_rows(other_clean, .id = 'Species_Code')

other_df$Species_Code <- as.double(other_df$Species_Code)

all_species <- bind_rows(final_df, other_df)

write_csv(other_df, 'output/final_df/other_species.csv')
rm(all_species)
