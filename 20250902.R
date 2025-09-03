setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(sf)
library(scico)

tuesdata = tidytuesdayR::tt_load('2025-09-02')
frogID_data = tuesdata$frogID
frog_names = tuesdata$frog_names

frog_data_st = frogID_data %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

grid = sf::st_make_grid(frog_data_st, n = c(100, 100)) %>%
  sf::st_as_sf() %>%
  mutate(ID = row_number())

frog_data_grid = frog_data_st %>%
  select(scientificName) %>%
  sf::st_join(grid) %>%
  group_by(ID) %>%
  summarise(n_obs = n(),
            n_spec = n_distinct(scientificName)) %>%
  sf::st_drop_geometry() %>%
  right_join(grid, by = "ID") %>%
  sf::st_as_sf() %>%
  mutate(n_obs_log = log(n_obs)) %>%
  mutate(n_obs = ifelse(is.na(n_obs), 0, n_obs),
         n_spec = ifelse(is.na(n_spec), 0, n_spec),
         n_obs_log = ifelse(is.na(n_obs_log), 0, n_obs_log))

colors = scico(n = 256, palette = "lajolla", direction = 1, begin = .2)
start_color = colors[1]
end_color = colors[length(colors)]

(p = ggplot() + theme_void() +
    geom_sf(data = frog_data_grid, aes(fill = n_obs_log), color = NA, linewidth = 0.0) +
    scico::scale_fill_scico(palette = 'lajolla', direction = 1, begin = .2,
                            name = 'Number of observations \n(log scale)') +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.ticks = element_blank(),
          text = element_text(family = 'Arial', color = colors[180]),
          plot.background = element_rect(fill = start_color, color = start_color)))
  
ggsave('20250902.png', p, width = 6, height = 4, dpi = 300)

