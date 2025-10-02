setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(sf)
library(scico)
library(ggtext)

tuesdata = tidytuesdayR::tt_load('2025-09-30')

cranes = tuesdata$cranes %>%
  mutate(day = yday(date),
         month = month(date),
         year = year(date)) 

months = expand_grid(month = seq(1:12), year = unique(cranes$year), day = seq(1:365)) %>%
  filter(month %in% unique(cranes$month))

plot_data = right_join(cranes, months, by = c("day", "year", "month")) %>%
  mutate(season = case_when(month %in% c(3,4) ~ 'Spring',
                            month %in% c(8, 9, 10) ~ 'Fall'))

#https://commons.wikimedia.org/wiki/File:Avirgo_illustration.png
logo_file = "ext/cranes.png" 

radial_labels = data.frame(
  label = c('10000', '20000', '30000', 'Observations'),
  observations = c(10000, 20000, 30000, 33000), 
  day = 140 
)

(p = ggplot() + coord_polar() + theme_bw() +
    labs(title = "**Daily observation of cranes at Lake Hornborgasjön**",
         subtitle = ('Data: Hornborgasjön field station, \nGraphic: commons.wikimedia.org/wiki/File:Avirgo_illustration.png')) +
  geom_smooth(data = plot_data, aes(x = day, y = observations, group = interaction(year, season), color =year), 
              method = 'loess', se = FALSE, span = 0.2, alpha = 0.8, size = .5) +
  scale_y_continuous(name = '', expand = c(0,0), breaks = c(10000, 20000, 30000), limits = c(0, 33000)) +
  scale_x_continuous(expand = c(0,0),  limits = c(30, 330), labels = c('Spring', 'Fall'), 
                     breaks = c(yday('2010-03-30'), yday('2010-09-30'))) +
  scico::scale_color_scico(palette = 'devon', name = '        Year', breaks = c(1994, 2024), end = .8) +
  geom_text(data = radial_labels, 
            aes(x = day, y = observations, label = label),
            color = 'white',
            size = 3,
            angle = 230) +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12, color = 'white', vjust = 1.5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 14, color = 'white'),
        plot.title = element_markdown(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 'dotted', linewidth = .25, colour = c("white", "white", "white", NA)),
        panel.border = element_blank(),
        legend.position = c(0.5, 0.2),
        legend.direction = 'horizontal',
        legend.title.position = 'bottom',
        legend.background = element_rect(fill='grey20', color = NA),
        legend.box.background = element_rect(fill='grey20', color = NA),
        panel.background = element_rect(fill = 'grey20', colour = NA),  
        plot.background = element_rect(fill = 'grey20', colour = NA),
        strip.background = element_rect(fill = 'grey20', color = NA)))

ggdraw(p) + 
  draw_image(logo_file, x = 0.5, y = 0.49, hjust = 0.5, vjust = 0.5, width = 0.5, scale = 1.3)

ggsave("20250930.png")
