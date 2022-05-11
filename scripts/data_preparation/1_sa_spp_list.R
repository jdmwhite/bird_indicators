library(tidyverse)

spp_list <- read_csv('data/SA_species_list_SABAP2/country_southafrica_specieslist.csv')

# Load in the file names
file_names <- list.files('data/sabap_cards_raw/')
file_names %<>% str_remove('.csv') # remove the .csv suffix to only have the 
new_names <- setdiff(spp_list$Ref, file_names)
new_names <- new_names[-1]