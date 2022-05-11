library(sf)
library(tidyverse)
library(ggsci)
library(patchwork)

#### load pentads ----
pent_cov <- read_sf('data/pent_biome_cover/pent_biome_cover.shp')

#### CAPE SUGARBIRD ----
csb <- read_csv('data/sabap_cards_raw/749.csv')

#### view bird data
head(csb)

#### APPROACH 1 - just presences ----
#### filter to pentads
csb <- csb %>% filter(Pentad %in% pent_cov$PENTADE)
# remove absences
csb_pres <- csb %>% filter(!Spp == '-')

# filter the pentad dataset to only include pentads with bird presences
pent_cov %>% filter(PENTADE %in% csb_pres$Pentad) -> pent_w_pres

# summarise
freq_df <- as.data.frame((table(pent_w_pres$dom_biome)))
names(freq_df) <- c('biome', 'freq')
freq_df$perc <- freq_df$freq/sum(freq_df$freq) * 100
freq_df$perc <- round(freq_df$perc)

pal_d3('category10')(10)

# Map
map1 <- ggplot() +
  geom_sf(data = pent_cov, fill = 'gray80', col = 'gray80', lwd = 0.1) +
  geom_sf(data = pent_w_pres, aes(fill = dom_biome), lwd = 0.01) +
  scale_fill_manual(name = 'Biome \n(>90% coverage)',
                 labels = c(paste0('Albany Thicket (',freq_df$perc[1],'%)'),paste0('Fynbos (',freq_df$perc[2],'%)'),paste0('Mixed (',freq_df$perc[3],'%)')),
                 values = c("#1F77B4FF", "#2CA02CFF", "#8C564BFF")) +
  scale_x_continuous(limits = c(18, 27.5)) +
  scale_y_continuous(limits = c(-35, -31)) +
  guides(fill = 'none') +
  labs(title = 'METHOD 1: Cape sugarbird pentad presences by dominant biome', subtitle = 'i.e. if the bird was seen at least once in this biome representative pentad') +
  theme_bw() 

# Make barplot
col1 <- ggplot() +
  geom_col(data = freq_df, aes(x = biome, y = perc, fill = biome)) +
  scale_fill_manual(name = 'Biome \n(>90% coverage)',
                    labels = c(paste0('Albany Thicket (',freq_df$perc[1],'%)'),paste0('Fynbos (',freq_df$perc[2],'%)'),paste0('Mixed (',freq_df$perc[3],'%)')),
                    values = c("#1F77B4FF", "#2CA02CFF", "#8C564BFF")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = seq(0,100,10)) +
  labs(title = 'METHOD 1: Cape sugarbird pentad presences by dominant biome', subtitle = 'i.e. if the bird was seen at least once in this biome representative pentad', y = 'Percentage (%)', x = 'Biome') +
  theme_bw()


#### APPROACH 2 number cards per pentad ----
pent_cov %>% select(PENTADE, dom_biome) %>% rename(Pentad = PENTADE) %>% st_drop_geometry() -> pent_dom_biome

# join biome cover dataset onto bird presence dataset
csb_pres %>% left_join(pent_dom_biome, by = 'Pentad') -> csb_biome

#### Count biome presences
csb_biome_freq <- as.data.frame(table(csb_biome$dom_biome))
names(csb_biome_freq) <- c('biome', 'freq')
csb_biome_freq$perc <- csb_biome_freq$freq/sum(csb_biome_freq$freq) * 100
csb_biome_freq$perc <- round(csb_biome_freq$perc)

# Make barplot
col2 <- ggplot() +
  geom_col(data = csb_biome_freq, aes(x = biome, y = perc, fill = biome)) +
  scale_fill_manual(name = 'Biome \n(>90% coverage)',
                    labels = c(paste0(csb_biome_freq$biome[1],' (',csb_biome_freq$perc[1],'%)'),paste0(csb_biome_freq$biome[2],' (',csb_biome_freq$perc[2],'%)'),paste0(csb_biome_freq$biome[3],' (',csb_biome_freq$perc[3],'%)')),
                    values = c("#1F77B4FF", "#2CA02CFF", "#8C564BFF")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = seq(0,100,10)) +
  labs(title = 'METHOD 2: Cape sugarbird presence counts per biome type', subtitle = 'i.e. how many times has the bird been observed in each biome type', y = 'Percentage (%)', x = 'Biome') +
  theme_bw()

#### Summarise per pentad
csb_pentad_counts <- csb_biome %>% group_by(Pentad) %>% summarise(n = n())

pent_w_pres %>% rename(Pentad = PENTADE) %>% left_join(csb_pentad_counts, by = 'Pentad') -> pent_counts_csb

# Map
map2 <- ggplot() +
  geom_sf(data = pent_cov, fill = 'gray80', col = 'gray80', lwd = 0.1) +
  geom_sf(data = pent_counts_csb, aes(fill = log(n)), lwd = 0.01) +
  scale_fill_viridis_c(name = 'log(Presences \nper pentad)') +
  scale_x_continuous(limits = c(18, 27.5)) +
  scale_y_continuous(limits = c(-35, -31)) +
  labs(title = 'METHOD 2: The sum of Cape sugarbird presences per pentad', subtitle = 'log scale used to account for massive oversampling in Cape Town') +
  theme_bw() +
  theme(legend.position = 'bottom')

#### patchwork plots
combo <- (map1 + col1) / (map2 + col2) & plot_layout(widths = c(1,0.5)) & plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave('output/csb749_different_biome_methods.png', combo, width = 14, height = 10)


#### REDWING STARLING ----
rws <- read_csv('data/sabap_cards_raw/745.csv')

#### view bird data
head(rws)

#### APPROACH 1 - just presences ----
#### filter to pentads
rws <- rws %>% filter(Pentad %in% pent_cov$PENTADE)
# remove absences
rws_pres <- rws %>% filter(!Spp == '-')

# filter the pentad dataset to only include pentads with bird presences
pent_cov %>% filter(PENTADE %in% rws_pres$Pentad) -> pent_w_pres

# summarise
freq_df <- as.data.frame((table(pent_w_pres$dom_biome)))
names(freq_df) <- c('biome', 'freq')
freq_df$perc <- freq_df$freq/sum(freq_df$freq) * 100
freq_df$perc <- round(freq_df$perc)

pal_d3('category10')(10)

# Map
map1 <- ggplot() +
  geom_sf(data = pent_cov, fill = 'gray80', col = 'gray80', lwd = 0.1) +
  geom_sf(data = pent_w_pres, aes(fill = dom_biome), lwd = 0.01) +
  scale_fill_manual(name = 'Biome \n(>90% coverage)',
                    values = c("#1F77B4FF", "#2CA02CFF", "#D62728FF", "#9467BDFF", "#8C564BFF", "#E377C2FF","#7F7F7FFF", "#BCBD22FF", "#17BECFFF")) +
  guides(fill = 'none') +
  labs(title = 'METHOD 1: Red-wing starling pentad presences by dominant biome', subtitle = 'i.e. if the bird was seen at least once in this biome representative pentad') +
  theme_bw() 

# Make barplot
col1 <- ggplot() +
  geom_col(data = freq_df, aes(x = biome, y = perc, fill = biome)) +
  scale_fill_manual(name = 'Biome \n(>90% coverage)',
                    labels = c(paste0(freq_df$biome[1],' (',freq_df$perc[1],'%)'),paste0(freq_df$biome[2],' (',freq_df$perc[2],'%)'),paste0(freq_df$biome[3],' (',freq_df$perc[3],'%)'),paste0(freq_df$biome[4],' (',freq_df$perc[4],'%)'),paste0(freq_df$biome[5],' (',freq_df$perc[5],'%)'),paste0(freq_df$biome[6],' (',freq_df$perc[6],'%)'),paste0(freq_df$biome[7],' (',freq_df$perc[7],'%)'),paste0(freq_df$biome[8],' (',freq_df$perc[8],'%)'),paste0(freq_df$biome[9],' (',freq_df$perc[9],'%)')),
                    values = c("#1F77B4FF", "#2CA02CFF", "#D62728FF", "#9467BDFF", "#8C564BFF", "#E377C2FF","#7F7F7FFF", "#BCBD22FF", "#17BECFFF")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = seq(0,100,10)) +
  labs(title = 'METHOD 1: Red-wing starling pentad presences by dominant biome', subtitle = 'i.e. if the bird was seen at least once in this biome representative pentad', y = 'Percentage (%)', x = 'Biome') +
  theme_bw() +
  theme(axis.text.x = element_blank())
col1

#### APPROACH 2 number cards per pentad ----
pent_cov %>% select(PENTADE, dom_biome) %>% rename(Pentad = PENTADE) %>% st_drop_geometry() -> pent_dom_biome

# join biome cover dataset onto bird presence dataset
rws_pres %>% left_join(pent_dom_biome, by = 'Pentad') -> rws_biome

#### Count biome presences
rws_biome_freq <- as.data.frame(table(rws_biome$dom_biome))
names(rws_biome_freq) <- c('biome', 'freq')
rws_biome_freq$perc <- rws_biome_freq$freq/sum(rws_biome_freq$freq) * 100
rws_biome_freq$perc <- round(rws_biome_freq$perc)

# Make barplot
col2 <- ggplot() +
  geom_col(data = rws_biome_freq, aes(x = biome, y = perc, fill = biome)) +
  scale_fill_manual(name = 'Biome \n(>90% coverage)',
                    labels = c(paste0(rws_biome_freq$biome[1],' (',rws_biome_freq$perc[1],'%)'),paste0(rws_biome_freq$biome[2],' (',rws_biome_freq$perc[2],'%)'),paste0(rws_biome_freq$biome[3],' (',rws_biome_freq$perc[3],'%)'),paste0(rws_biome_freq$biome[4],' (',rws_biome_freq$perc[4],'%)'),paste0(rws_biome_freq$biome[5],' (',rws_biome_freq$perc[5],'%)'),paste0(rws_biome_freq$biome[6],' (',rws_biome_freq$perc[6],'%)'),paste0(rws_biome_freq$biome[7],' (',rws_biome_freq$perc[7],'%)'),paste0(rws_biome_freq$biome[8],' (',rws_biome_freq$perc[8],'%)'),paste0(rws_biome_freq$biome[9],' (',rws_biome_freq$perc[9],'%)')),
                    values = c("#1F77B4FF", "#2CA02CFF", "#D62728FF", "#9467BDFF", "#8C564BFF", "#E377C2FF","#7F7F7FFF", "#BCBD22FF", "#17BECFFF")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100), breaks = seq(0,100,10)) +
  labs(title = 'METHOD 2: Red-wing starling presence counts per biome type', subtitle = 'i.e. how many times has the bird been observed in each biome type', y = 'Percentage (%)', x = 'Biome') +
  theme_bw() +
  theme(axis.text.x = element_blank())

#### Summarise per pentad
rws_pentad_counts <- rws_biome %>% group_by(Pentad) %>% summarise(n = n())

pent_w_pres %>% rename(Pentad = PENTADE) %>% left_join(rws_pentad_counts, by = 'Pentad') -> pent_counts_rws

# Make map
map2 <- ggplot() +
  geom_sf(data = pent_cov, fill = 'gray80', col = 'gray80', lwd = 0.1) +
  geom_sf(data = pent_counts_rws, aes(fill = log(n)), lwd = 0.01) +
  scale_fill_viridis_c(name = 'log(Presences \nper pentad)') +
  labs(title = 'METHOD 2: The sum of Red-wing starling presences per pentad', subtitle = 'log scale used to account for oversampling around major city centres') +
  theme_bw() +
  theme(legend.position = 'bottom')

#### patchwork plots
combo <- (map1 + col1) / (map2 + col2) & plot_layout(widths = c(1,0.5)) & plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave('output/rws745_different_biome_methods.png', combo, width = 14, height = 10)
