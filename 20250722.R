setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(sf)

tuesdata = tidytuesdayR::tt_load('2025-07-22')

stops = read_sf('ext/nyu_2451_34759.shp') %>%
  filter(GEOID != 36085) %>%
  mutate(stop_tag = str_squish(stop_name)) %>%
  mutate(stop_tag = tolower(str_replace_all(stop_tag, " ", "")))

borders = st_read('ext/doitt_shoreline_2012.gpkg') %>%
  st_simplify() %>%
  st_transform(st_crs(stops)) %>%
  st_crop(st_buffer(st_as_sfc(st_bbox(stops)), 800)) 

mta_art = tuesdata$mta_art %>%
  mutate(tiles = ifelse(grepl("tile", art_material, ignore.case = TRUE), 1, 0),
         mosaic = ifelse(grepl("mosaic", art_material, ignore.case = TRUE), 1, 0)) %>%
  mutate(art = ifelse(rowSums(across(c(tiles, mosaic))) == 0, 'Other Art', 'Mosaics*')) %>%
  filter(agency == 'NYCT') %>%
  mutate(stop_tag = str_squish(station_name)) %>%
  mutate(stop_tag = tolower(str_replace_all(stop_tag, " ", ""))) %>%
  select(-artist, -art_title, -art_date, -art_image_link, -art_description, -art_material, -agency, -station_name, -line, -tiles, -mosaic) %>%
  right_join(stops) %>%
  select(art, stop_name, geometry) %>%
  st_as_sf() %>%
  mutate(art = ifelse(is.na(art), "No Art", str_to_title(art))) %>%
  unique()


mta_art$art = factor(mta_art$art, 
                          levels = (c('No Art', 'Other Art', 'Mosaics*')))

mta_art = mta_art[order(mta_art$art), ]

ggplot() + theme_void() +
  geom_sf(data = borders, color = 'black', fill = '#f4f2f0', linewidth = .2) +
  geom_sf(data = mta_art, aes(fill = art), color = 'black', stroke = .3, shape = 23) +
  scale_fill_manual(values = c('Mosaics*' = 'sienna2', 'Other Art' = 'goldenrod2', 'No Art' = '#00000000'),
                     name = 'NYC metro stations that feature ... ') +
  theme(legend.background = element_rect(fill=NA, color = NA),
        legend.position = c(.3625, .905),
        legend.direction = 'vertical',
        text = element_text(size = 12),
        legend.box.background = element_rect(fill = NA, color = NA), 
        panel.background = element_rect(fill = '#f4f2f0', colour = NA)) +
  guides(fill = guide_legend(override.aes = list(size = 2, stroke = .5),
                             reverse = TRUE)) + 
    annotate(geom="text", x=-73.9, y=40.555, label="*Art descriptions that include the word 'mosaic' or 'tile' are categorized as mosaics.\nSource: MTA Arts & Design, NYC Open Data",
             color="black", size = 2, fontface = 'italic')

ggsave("20250722.png", dpi = 800, height = 7, width = 4)
