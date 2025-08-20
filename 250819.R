setwd("~/Desktop/TidyTuesday")

library(tidyverse)
library(zoo)
library(stabledist)

tuesdata = tidytuesdayR::tt_load('2025-08-19')

munros = tuesdata$scottish_munros

munros_per_year = munros %>%
  pivot_longer(cols = c("1891", "1921", "1933", "1953", "1969", "1974", "1981", "1984",
                        "1990", "1997","2021")) %>%
  filter(!is.na(value)) %>%
  filter(value == 'Munro') %>%
  group_by(name) %>%
  count(value) %>%
  mutate(Year = as.numeric(name),
         point_type = 'data') %>%
  ungroup() %>%
  select(Year, point_type, n)

all_year = data.frame(Year = seq(1850, 2050))

munros_added_point = munros_per_year %>%
  ungroup() %>%
  add_row(Year = 1850, n = 275, point_type = 'no data') %>%
  add_row(Year = 2050, n = 273, point_type = 'no data') %>%
  right_join(all_year, by = 'Year') %>%
  mutate(bottom = 270,
         n = na.approx(n, x = Year, na.rm = FALSE) + rstable(n, beta = -1, alpha = 1.5, gamma = .1) ,
         top_treeline = 274 + rnorm(n, mean = 0, sd = .15) + sin(((Year)*.02)),
         bottom_snow = 280 + rnorm(n, mean = 0, sd = .15)) %>%
  mutate(bottom_snow = ifelse(bottom_snow < n, bottom_snow, n),
         top_treeline = ifelse(top_treeline > n, n, top_treeline))

annotations_right = munros_per_year %>%
  filter(Year %in% c(1891, 1921, 1997, 2021)) %>%
  filter(point_type == 'data') %>%
  mutate(label = paste0(Year, ": ", n, " Munros"),
         ymin = n + .2,
         ymax = n + 2) %>%
  select(Year, label, n, ymin, ymax)

annotations_left = munros_per_year %>%
  filter(Year %in% c(1974)) %>%
  filter(point_type == 'data') %>%
  mutate(label = paste0(Year, ": ", n, " Munros"),
         ymin = n + .2,
         ymax = n + 2) %>%
  select(Year, label, n, ymin, ymax)

ggplot() + theme_void() +
  geom_ribbon(data = munros_added_point, aes(x = Year, ymax = n, ymin = bottom), fill = '#cbd6da') +
  geom_ribbon(data = munros_added_point, aes(x = Year, ymax = top_treeline, ymin = bottom), fill = '#8b9b74') +
  geom_ribbon(data = munros_added_point, aes(x = Year, ymax = n, ymin = bottom_snow), fill = '#f7ffff') +
  geom_line(data = munros_added_point, aes(x = Year, y = n), color = 'black', linewidth = 0.5) +
  geom_line(data = munros_added_point, aes(x = Year, y = bottom_snow), color = 'black', linewidth = 0.25) +
  geom_line(data = munros_added_point, aes(x = Year, y = top_treeline), color = 'black', linewidth = 0.25) +
  geom_segment(data = annotations_left, aes(x = Year, y = ymin, xend = Year, yend = ymax)) +
  geom_segment(data = annotations_right, aes(x = Year, y = ymin, xend = Year, yend = ymax)) +
  geom_text(data = annotations_right, aes(x = Year, y = n + 2.25, label = label), 
            size = 3.5, hjust = 0.01, vjust = 0, color = 'black') +
  geom_text(data = annotations_left, aes(x = Year, y = n + 2.25, label = label), 
            size = 3.5, hjust = 1, vjust = 0, color = 'black') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(270, 287), expand = c(0,0)) +
  theme(panel.background = element_rect(fill = 'ivory1', colour = NA))

ggsave("250819.png", width = 8, height = 3, dpi = 600)
