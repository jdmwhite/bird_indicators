library(tidyverse)
library(sf)
library(magrittr)

## Pentads
pent <- read_sf('data/pentads/original/pentad_sa.shp')
head(pent)

plot(pent['QDGC'])

## Template card
template <- read_csv('data/sabap_cards_raw/181.csv')

# Some pentads have a 'c' instead of an '_'
template$Pentad <- gsub("c", '_', template$Pentad)

template %>% filter(Pentad %in% pent$PENTADE) -> template_pents

#### Year variable ----
template_pents$year <- lubridate::year(template_pents$StartDate)
sort(unique(template_pents$year))

template_pents %<>% filter(year > 2007 & year < 2022)
length(sort(unique(template_pents$year)))

#### Count number of cards
template_pents %>% group_by(Pentad) %>% add_tally() -> template_n

#### filter by min 20
template_n %>% filter(n > 20) -> template_20

#### early/late periods
template_20$Period <- ifelse(template_20$year >= 2015, "L", "E")

#### tally n cards per period for each pentad
template_20 %>% group_by(Pentad, Period) %>% add_tally(name = 'period_tally') -> template_period

#### filter to have a minimum of 5 cards per period
template_period %>% filter(period_tally > 5) -> template_end

#### save template
write_csv(template_end,'output/template/template.csv')

as.data.frame(table(template_n$n)) %>% ggplot() +
  geom_col(aes(x = Var1, y = Freq))

?geom_histogram
# load template
# create a function to filter by cards/pentad (minimum of 20, with at least 5 in either first half or second half)
# loop/apply the template to each indicator species
# stack them together into a single df





fynbos_birds

# include clapper lark with big number
fyn_select <- c(181, 749, 753, 855, 540, 869, 612, 870)

list.files('data/sabap_cards_raw')

my_data <- list()
for (i in seq_along(fyn_select)) {
  my_data[[i]] <- read_csv(paste0('data/sabap_cards_raw/',i,'.csv'))
}

my_data

write_csv('fynbos_df.csv')