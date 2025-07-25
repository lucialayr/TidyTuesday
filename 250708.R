setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(ggridges)
library(ggtext)

tuesdata = tidytuesdayR::tt_load('2025-07-08')

users = tuesdata$users
color_ranks = tuesdata$color_ranks
answers = tuesdata$answers

df = full_join(answers, users) %>%
  filter(!is.na(colorblind), rank == 5) %>%
  select(colorblind, hex) %>%
  rowwise() %>%
  mutate(rgb = list(as.numeric(col2rgb(hex)))) %>%
  mutate(Red = rgb[1],
         Green = rgb[2],
         Blue = rgb[3]) %>%
  ungroup() %>%
  group_by(colorblind) %>%
  slice_sample(n = 3822) %>%
  mutate(colorblind = if_else(colorblind == 0, 'Normal vision', 'Colorblind'))

df$colorblind = factor(df$colorblind, levels = c('Normal vision', 'Colorblind'))

legend = data.frame(Red = c(255, 255, 0, 0, 0, 0, 255, 255),
                    Green = c(0, 0, 255, 255, 0, 0, 255, 255),
                    Blue = c(0, 0, 0, 0, 0, 0, 255, 255),
                    hex = c('#ff0000', '#ff0000', '#00ff00', '#00ff00', 
                            '#000000', '#000000', '#ffffff', '#ffffff'),
                    colorblind = c('Normal vision', 'Colorblind', 'Normal vision', 'Colorblind',
                                   'Normal vision', 'Colorblind', 'Normal vision', 'Colorblind'))

df$colorblind = factor(df$colorblind, levels = c('Normal vision', 'Colorblind'))
legend$colorblind = factor(legend$colorblind, levels = c('Normal vision', 'Colorblind'))

ggplot() +
  labs(title = "**All the colors we call <span style='color:#653700;'>brown</span>**")  +
  facet_wrap(~ colorblind, ncol = 2, strip.position="bottom") +
  #geom_hline(yintercept = 255/2, linewidth = .25) +
  #geom_vline(xintercept = 255/2, linewidth = .25) +
  geom_segment(data = legend, x = 220, y = 0, xend = 250, yend = 0,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, x = 35, y = 255, xend = 5, yend = 255,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, y = 220, x = 0, yend = 250, xend = 0,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, y = 35, x = 255, yend = 5, xend = 255,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, x = 220, y = 255, xend = 250, yend = 255,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, x = 35, y = 0, xend = 5, yend = 0,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, y = 220, x = 255, yend = 250, xend = 255,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_segment(data = legend, y = 35, x = 0, yend = 5, xend = 0,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               linewidth = .25) +
  geom_point(data  = df, shape = 21, stroke = .2,
             aes(x = Red, y = Green, fill = hex), size = 2) +
  geom_point(data  = legend, shape = 23, stroke = .2,
             aes(x = Red, y = Green, fill = hex), size = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(breaks = c(0, 255), expand = c(.05, .05)) +
  scale_y_continuous(breaks = c(0, 255), expand = c(.05, .05)) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 20),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.title = element_markdown(lineheight = 1.1),
        legend.background = element_rect(fill='#f4f2f0', color = NA),
        legend.box.background = element_rect(fill='#f4f2f0', color = NA),
        panel.background = element_rect(fill = '#f4f2f0', colour = NA),  
        plot.background = element_rect(fill = '#f4f2f0', colour = NA),
        strip.background = element_rect(fill = '#f4f2f0', color = NA))
ggsave("250708.png", width = 10, height = 6, dpi = 600)


