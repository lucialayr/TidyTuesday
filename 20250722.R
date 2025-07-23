setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(sf)

tuesdata = tidytuesdayR::tt_load('2025-07-22')

stops = read_sf('ext/nyu_2451_34759.shp') %>%
  mutate(stop_tag = str_squish(stop_name)) %>%
  mutate(stop_tag = tolower(str_replace_all(stop_tag, " ", "")))

borders = st_read('ext/doitt_shoreline_2012.gpkg') %>%
  st_simplify()

mta_art = tuesdata$mta_art %>%
  mutate(bronze = ifelse(grepl("bronze", art_material, ignore.case = TRUE), 1, 0),
         stone = ifelse(grepl("stone", art_material, ignore.case = TRUE), 1, 0),
         glass = ifelse(grepl("glass", art_material, ignore.case = TRUE), 1, 0),
         stone = ifelse(grepl("stone", art_material, ignore.case = TRUE), 1, 0),
         ceramic = ifelse(grepl("ceramic", art_material, ignore.case = TRUE), 1, 0),
         steel = ifelse(grepl("steel", art_material, ignore.case = TRUE), 1, 0)) %>%
  mutate(other = ifelse(rowSums(across(c(bronze, stone, glass, ceramic, steel))) == 0, 1, 0)) %>%
  filter(agency == 'NYCT') %>%
  mutate(stop_tag = str_squish(station_name)) %>%
  mutate(stop_tag = tolower(str_replace_all(stop_tag, " ", ""))) %>%
  pivot_longer(values_to = "present", 
               names_to = "material", 
               cols = c(bronze, stone, glass, ceramic, steel, other)) %>%
filter(present == 1) %>%
  select(-artist, -art_title, -art_date, -art_image_link, -art_description, -art_material, -agency, -station_name, -line, -present) %>%
  right_join(stops) %>%
  st_as_sf() %>%
  mutate(material = ifelse(is.na(material), "No Art", str_to_title(material))) %>%
  unique()

mta_art$material = factor(mta_art$material, 
                          levels = c('No Art', 'Other', 'Ceramic',  'Glass',  'Steel', 'Bronze' , 'Stone'))

mta_art = mta_art[order(mta_art$material), ]

ggplot() + theme_void() + 
  ggtitle(' What is NYC Metro Station Art made of?') +
  geom_sf(data = borders, color = 'seashell4', linewidth = .2) +
  geom_sf(data = mta_art, aes(color = material, shape = material), size = .5, stroke = .3) +
  scale_color_manual(values = c('Bronze' = 'goldenrod', 'Stone' = 'sienna2', 'Glass' = 'seagreen3', 
                                'Ceramic' = 'wheat2', 'Steel' = 'steelblue', 'Other' = 'seashell3', 'No Art' = 'black'),
                     name = '') +
  scale_shape_manual(values = c('Bronze' = 3, 'Stone' = 4, 'Glass' = 0, 
                                 'Ceramic' = 15, 'Steel' = 5, 'Other' = 18, 'No Art' = 18),
                     name = '') +
  theme(legend.background = element_rect(fill='#f4f2f0', color = NA),
        legend.position = c(.25, .75),
        text = element_text(size = 12),
        legend.box.background = element_rect(fill='#f4f2f0', color = NA),
        panel.background = element_rect(fill = '#f4f2f0', colour = NA),  
        plot.background = element_rect(fill = '#f4f2f0', colour = NA),
        strip.background = element_rect(fill = '#f4f2f0', color = NA)) +
  guides(shape = guide_legend(override.aes = list(size = 1, stroke = .75),
                              reverse = TRUE),
         color = guide_legend(reverse = TRUE))

ggsave("20250722.png", dpi = 600, width = 4, height = 4)

