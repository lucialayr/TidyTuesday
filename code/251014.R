setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(sf)
library(scico)

tuesdata = tidytuesdayR::tt_load('2025-10-14')
food_security = tuesdata$food_security

rail_data = food_security %>%
  filter(Item == "Rail lines density (total route in km per 100 square km of land area)",
         Area %in% c("Northern America", 
                     "Northern Europe", "Western Europe", "Eastern Europe", "Southern Europe", 
                     "Western Asia and Northern Africa", "Central Asia", "Southern Asia", 
                     "Eastern Asia"))

rail_data$Area = factor(rail_data$Area, levels = c("Northern America", 
                                                    "Northern Europe", "Western Europe", "Eastern Europe", "Southern Europe", 
                                                    "Western Asia and Northern Africa", "Central Asia", "Southern Asia", 
                                                    "Eastern Asia"))
ggplot() + theme_classic() +
  ggtitle('Railroad tracks around the world') + 
  geom_line(data = rail_data, aes(x = Year_Start, y = Value, color = Area)) +
  geom_point(data = rail_data, aes(x = Year_Start, y = Value, color = Area), shape = 21, size = 2, fill = '#f4f1ee') +
  geom_point(data = rail_data, aes(x = Year_Start, y = Value, color = Area), shape = 20, size = 1.5) +
  scale_x_continuous(expand = c(.05, .05), name = 'Year') +
  scale_y_continuous(expand = c(.05, .05), name = 'km per 100 square km of land area') +
  scale_color_manual(name = 'Region',
                     values = c("Northern America" ='#001219',
                                      "Northern Europe" = '#005f73',
                                      "Western Europe" = '#0a9396',
                                      "Eastern Europe" = '#94d2bd',
                                      "Southern Europe" = '#e9d8a6',
                                      "Western Asia and Northern Africa" = '#ee9b00',
                                      "Central Asia" = '#ca6702',
                                      "Southern Asia" = '#bb3e03',
                                      "Eastern Asia" = '#ae2012')) +
  theme(text = element_text(size = 15, color = 'grey20'),
        legend.background = element_rect(fill='#f4f1ee', color = NA),
        legend.box.background = element_rect(fill='#f4f1ee', color = NA),
        panel.background = element_rect(fill = '#f4f1ee', colour = NA),  
        plot.background = element_rect(fill = '#f4f1ee', colour = NA),
        strip.background = element_rect(fill = '#f4f1ee', color = NA),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = 'grey20', linewidth = 0.25),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(color = 'grey20'),
        panel.grid.major.y = element_line(color = 'grey20', linewidth = 0.15))

ggsave('viz/251014.png', width = 8, height = 5, dpi = 300)        
