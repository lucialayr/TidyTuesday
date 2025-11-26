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
  dplyr::select(country, region, population) %>%
  sf::st_transform(crs = 4326) %>%
  mutate(area = sf::st_area(geometry),
         x = sf::st_coordinates(sf::st_centroid(geometry))[,1],
         y = sf::st_coordinates(sf::st_centroid(geometry))[,2])

ggplot() + theme_void() +
  geom_sf(data = spi_indicators, color = 'white',  fill = 'grey90', linewidth = .1) +
  geom_point(data = population_data, aes(x = x, y = y, size = population, fill = region),
             color = 'grey90', shape = 21, stroke = .5) +
  scale_size_continuous(range = c(0.5, 40)) +
  scico::scale_fill_scico_d(palette = 'batlow', end = .8) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.background = element_rect(fill = 'grey90', colour = NA)) +
  theme(legend.position = 'None')

