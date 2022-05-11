library(sf)
library(tidyverse)
library(ggsci)

#### load data ----
pent <- read_sf('data/pentads/original/pentad_sa.shp')
perc_cov <- read_csv('data/percent_coverage.csv')

perc_cov$...1 <- NULL
names(perc_cov)[6] <- 'unknown1'
names(perc_cov)[12] <- 'unknown2'

summary(perc_cov)

#### check to see if sum to 100
perc_cov %>%
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) -> perc_cov

#### Calculate new percentage based on sum across
perc_cov %>% 
  mutate(across(`Azonal Vegetation`:`Albany Thicket`, ~./total*100)) -> perc_cov

#### See if now sums to 100
perc_cov %>%
  mutate(total = rowSums(across(`Azonal Vegetation`:`Albany Thicket`), na.rm = TRUE)) -> perc_cov

#### Check summary for total column
summary(perc_cov)

# remove total column
perc_cov$total <- NULL

#### manipulate perc_cov
# Convert to long df
perc_cov %>% pivot_longer(cols = `Azonal Vegetation`:`Albany Thicket`, names_to = 'Biome', values_to = 'perc_cov') -> cov_long

# Select row with max cover
cov_long %>% 
  group_by(PENTADE) %>%
  filter(perc_cov == max(perc_cov, na.rm = TRUE)) -> pent_max_cov

# If max cover > 90% then we keep that biome name, if less then it is a mixed cover biome
pent_max_cov %>%
  mutate(dom_biome = case_when(
    perc_cov >= 90 ~ Biome,
    perc_cov < 90 ~ 'Mixed'
  )) -> pent_dom_cov

#### join datasets ----
# clean pentad sf
pent %>% select(PENTADE) -> pent

# join
pent_cov <- pent %>% left_join(pent_dom_cov, by = 'PENTADE')

#### remove fringe veg types
pent_cov %>% mutate(dom_biome = case_when(
  dom_biome == 'Azonal Vegetation' ~ 'Other',
  dom_biome == 'Desert' ~ 'Other',
  dom_biome == 'unknown1' ~ 'Other',
  TRUE ~ dom_biome
)) -> pent_cov

write_sf(pent_cov, 'data/pent_biome_cover/pent_biome_cover.shp')


# Plot
ggplot() +
  geom_sf(data = pent_cov, aes(fill = dom_biome, col = dom_biome), lwd = 0) +
  scale_fill_d3(palette = 'category10', name = 'Dominant\nbiomes\n(>90% coverage)') +
  guides(colour = 'none') +
  theme_bw()

ggsave('output/dominant_biome.png', width = 9.54, height = 4, dpi = 'retina')
