library(sf)
library(tidyverse)
library(patchwork)

#### load data ----
pent <- read_sf('data/pentads/original/pentad_sa.shp')
perc_wat <- read_csv('data/pentad_water_cover/pentad_water.csv')

# select cols
perc_wat %>% select(PENTADE, histogram) -> perc_wat

# remove brackets
perc_wat$histogram <- gsub("[{}]", "", perc_wat$histogram)

# split in two
hist_cols <- as.data.frame(str_split_fixed(perc_wat$histogram, ',', 2))

# remove gap
hist_cols$V2 <- gsub(' ', '', hist_cols$V2)
hist_cols

# replace empty cells with 0 values from col 1
hist_cols$V2 <- ifelse(
  hist_cols$V2 == '', hist_cols$V1, hist_cols$V2
)

# remove 1s and 0s
hist_cols$V1 <- substring(hist_cols$V1, 3)
hist_cols$V2 <- substring(hist_cols$V2, 3)

# if v1 == v2, then make it 0, if =/ then keep v1
hist_cols$V1 <- ifelse(
  hist_cols$V1 == hist_cols$V2, 0, hist_cols$V1
)

# Calculate % water cover
hist_cols$perc_wat <- (as.numeric(hist_cols$V1)/(as.numeric(hist_cols$V1) + as.numeric(hist_cols$V2)))*100

# Join the water perc back onto the pentad dataset
perc_wat$perc_wat <- hist_cols$perc_wat

# clean the df
perc_wat %>% select(PENTADE, perc_wat) -> perc_wat

# join onto the spatial pentad layer
pent %>% select(PENTADE) %>% left_join(perc_wat, by = 'PENTADE') -> pent_wat

## Plots ----
plot0 <- ggplot() +
  geom_sf(data = pent, col = 'gray', fill = 'gray') +
  geom_sf(data = pent_wat %>% filter(perc_wat > 0), aes(fill = perc_wat), col = NA) +
  scale_fill_viridis_c(limits = c(0, 100)) +
  theme_bw() +
  labs(title = paste0('All pentads with >',0,'% water cover'),
       subtitle = paste0(pent_wat %>% filter(perc_wat > 0) %>% nrow(),' pentads in total')) 

plot1 <- ggplot() +
  geom_sf(data = pent, col = 'gray', fill = 'gray') +
  geom_sf(data = pent_wat %>% filter(perc_wat > 0.5), aes(fill = perc_wat), col = NA) +
  scale_fill_viridis_c(limits = c(0, 100)) +
  theme_bw() +
  labs(title = paste0('All pentads with >',0.5,'% water cover'),
       subtitle = paste0(pent_wat %>% filter(perc_wat > 0.5) %>% nrow(),' pentads in total')) 

plot2.5 <- ggplot() +
  geom_sf(data = pent, col = 'gray', fill = 'gray') +
  geom_sf(data = pent_wat %>% filter(perc_wat > 1), aes(fill = perc_wat), col = NA) +
  scale_fill_viridis_c(limits = c(0, 100)) +
  theme_bw() +
  labs(title = paste0('All pentads with >',1,'% water cover'),
       subtitle = paste0(pent_wat %>% filter(perc_wat > 1) %>% nrow(),' pentads in total')) 

plot5 <- ggplot() +
  geom_sf(data = pent, col = 'gray', fill = 'gray') +
  geom_sf(data = pent_wat %>% filter(perc_wat > 2.5), aes(fill = perc_wat), col = NA) +
  scale_fill_viridis_c(limits = c(0, 100)) +
  theme_bw() +
  labs(title = paste0('All pentads with >',2.5,'% water cover'),
       subtitle = paste0(pent_wat %>% filter(perc_wat > 2.5) %>% nrow(),' pentads in total')) 

(plot0 + plot1)/(plot2.5 + plot5) + 
  plot_layout(guides = 'collect')

ggsave('output/water_pentads.png', width = 10.6, height = 8, dpi = 600)
