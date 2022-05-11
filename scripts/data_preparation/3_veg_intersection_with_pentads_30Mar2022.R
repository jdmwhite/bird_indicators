# only use this if you get an error for the intersection
sf::sf_use_s2(FALSE)


install.packages

library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)

###
## Vegmap
vegmap_simp <- read_sf('data/vegmap/simplified/vegmap_simp.shp')

pent <- read_sf('data/pentads/original/pentad_sa.shp')
pent %>% distinct(PENTADE, .keep_all = TRUE) -> pent

####

st_crs(vegmap_simp)
st_crs(pent)
vegmap_simp <- st_transform(vegmap_simp, crs = st_crs(pent))

veg_valid <- st_make_valid(vegmap_simp)
beepr::beep(4)

#### Don't worry about cropping when running over the full dataset
veg_sub <- st_crop(veg_valid, pent_sub)
beepr::beep(4)

veg_sub %>%
  group_by(BIOME_18) %>%
  summarise() -> veg_dissolve

table(st_is_valid(veg_dissolve))
table(st_is_valid(pent_sub))

# # if there are any falses, run...
# veg_dissolve <- st_make_valid(veg_dissolve)
# pent_sub <- st_make_valid(pent_sub)

ggplot() +
  geom_sf(data = veg_dissolve, aes(fill = BIOME_18)) +
  geom_sf(data = pent_sub, fill = NA) 

#### INTERSECTION
st_intersection(veg_dissolve, pent_sub) %>% 
  dplyr::mutate(intersect_area = st_area(.)) %>%
  dplyr::select(BIOME_18, PENTADE, intersect_area) %>%
  st_drop_geometry() -> intersect_df

# Create a fresh area variable for pentads
pent_sub %>% mutate(pent_area = st_area(pent_sub)) -> pent_sub

# Merge by pentad name
pent_sub %>% select(PENTADE, pent_area) %>% merge(intersect_df, by = 'PENTADE') -> pent_merge

# Calculate coverage
pent_merge %>% 
  st_drop_geometry() %>%
  mutate(coverage = as.numeric(intersect_area/pent_area)*100) %>%
  dplyr::select(PENTADE, BIOME_18, coverage) %>%
  tidyr::pivot_wider(names_from = 'BIOME_18', values_from = 'coverage') -> pent_coverage

pent_coverage %>% glimpse()

