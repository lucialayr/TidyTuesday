setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

countries = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::select(iso_a3_eh) %>%
  rename(iso3c = iso_a3_eh)

tuesdata = tidytuesdayR::tt_load('2025-11-25')
spi_indicators = tuesdata$spi_indicators %>%
  left_join(countries) %>%
  sf::st_as_sf()

population_data = spi_indicators %>%
  filter(year == 2023) %>%
  dplyr::select(country, region, population) %>%
  unique() %>%
  sf::st_transform(crs = 4326) %>%
  mutate(area = sf::st_area(geometry),
         x = sf::st_coordinates(sf::st_centroid(geometry))[,1],
         y = sf::st_coordinates(sf::st_centroid(geometry))[,2],
         population = population/1000000) %>%
  arrange(desc(population)) 

colors = scico(n = 6, palette = "batlow", direction = 1, end = .8)
legend_color = colors[6]

ggplot() + theme_void() +
  geom_sf(data = spi_indicators, color = 'white',  fill = 'grey90', linewidth = .1) +
  geom_point(data = population_data, aes(x = x, y = y, size = population, fill = region),
             color = 'grey90', shape = 21, stroke = .5) +
  scale_size_continuous(range = c(0.5, 40),
                        breaks = c(1, 100, 1000),
                        limits = c(0, 1500)) +
  scico::scale_fill_scico_d(palette = 'batlow', end = .8) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.background = element_rect(fill = 'grey90', colour = NA)) +
  guides(fill = guide_none(),
         size = guide_legend(title = 'Population in Millions (2023)', title.position = 'top', title.hjust = .5,
                             override.aes = list(fill = legend_color)))

ggsave('viz/251125.png', dpi = 300, height = 6, width = 12)

