library(tidyverse)
library(magrittr)

# If in Windows run the following:
options(download.file.method="libcurl", url.method="libcurl")

# Set up output working directory
outputwd <- 'data/sabap_cards_raw/'

spp_list <- read_csv('data/SA_species_list_SABAP2/country_southafrica_specieslist.csv')

# Load in the file names
file_names <- list.files('data/sabap_cards_raw/')
file_names %<>% str_remove('.csv') # remove the .csv suffix to only have the 
new_names <- setdiff(spp_list$Ref, file_names)
new_names <- new_names[-1]

# Specify the sequence of SABAP IDs that you would like to download
# birds <- c(1:1000)
# protea canary
# c(749, 745, 151)
# cape sugarbird, red-wing starling, bateleur

birds <- new_names

# Run a for loop that places these numbers into the url. 
# The url is set to download the data in csv format with the full protocol with null counts
for(bird_name in birds) {

# url
# url <- paste0('api.adu.org.za/sabap2/v2/cards/species/info/',ii,'?format=csv&inclnull=1')
url <- paste0('https://api.birdmap.africa/sabap2/v2/cards/species/info/',bird_name,'?format=csv&inclnull=1')

# download
download.file(url, paste0(outputwd,bird_name,'.csv'))

}
