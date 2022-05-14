library(tidyverse)
library(dplyr)
library(stringr)

#importing the data
roberts <- read_csv('data/roberts/robertsbirds.csv')

#separating the different species based on habitat
#keeping all species that have that habitat as their main (==3) and making sure 
#that no other habitat is there main (<3)
#start with forest species
forest_species <- roberts %>%
  group_by(Forest)  %>%
  filter(Wetland<2,Cliffs<2,Forest==3,Woodland<2,Grassland<2,Savanna<2, 
         Thicket<2,Fynbos<2,Karoo<2,Lagoons<2,`Semi-arid`<2,`Namib desert`<2, 
         Agricultural<2, `Open coast`<2,Marine<2)

#wetland species next
wetland_species <- roberts %>%
  group_by(Wetland) %>%
  filter(Wetland==3,Cliffs<2,Forest<2,Woodland<2,Grassland<2,Savanna<2, 
         Thicket<2,Fynbos<2,Karoo<2,Lagoons<2,`Semi-arid`<2,`Namib desert`<2, 
         Agricultural<2, `Open coast`<2,Marine<2)

#coastal species
coastal_species <- roberts %>%
  group_by(`Open coast`) %>%
  filter(Wetland<3,Cliffs<3,Forest<3,Woodland<3,Grassland<3,Savanna<3, 
         Thicket<3,Fynbos<3,Karoo<3,Lagoons<3,`Semi-arid`<3,`Namib desert`<3, 
         Agricultural<3, `Open coast`==3,Marine<3)

#woodland species
woodland_species <- roberts %>%
  group_by(Woodland) %>%
  filter(Wetland<2,Cliffs<2,Forest<2,Woodland==3,Grassland<2,Savanna<2, 
         Thicket<2,Fynbos<2,Karoo<2,Lagoons<2,`Semi-arid`<2,`Namib desert`<2, 
         Agricultural<2, `Open coast`<2,Marine<2)

#marine species to filter other lists by
coastal_species <- roberts %>%
  group_by(`Open coast`) %>%
  filter(Wetland<3,Cliffs<3,Forest<3,Woodland<3,Grassland<3,Savanna<3, 
         Thicket<3,Fynbos<3,Karoo<3,Lagoons<3,`Semi-arid`<3,`Namib desert`<3, 
         Agricultural<3, `Open coast`<3,Marine==3)



write_csv(coastal_species, 'data/wetland&forest/coastal_species.csv')
write_csv(forest_species, 'data/wetland&forest/forest_species.csv')
write_csv(wetland_species, 'data/wetland&forest/wetland_species.csv')
write_csv(marine_species, 'data/wetland&forest/marine_species.csv')
