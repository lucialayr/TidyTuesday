setwd("~/Desktop/TidyTuesday")

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2025-06-24')
cases_year = tuesdata$cases_year
cases_month = tuesdata$cases_month

cases_seasonal_year = cases_month %>%
  group_by(month, year) %>%
  summarize(measles_per_month = mean(measles_total, na.rm = T))

padding = cases_seasonal_year %>%
  filter(month == 1) %>%
  mutate(month = 13)

cases_seasonal_year = bind_rows(cases_seasonal_year, padding) 

cases_seasonal_year$year = factor(cases_seasonal_year$year, levels = rev(seq(2012, 2025)))

ggplot() + theme_classic() +
  ggtitle('Yearly increment in global measles cases') +
  coord_polar() +
  geom_area(data = cases_seasonal_year, aes(x = month, y = measles_per_month, color = year, fill = year),
            linewidth = .4, color = '#f4f2f0') +
  scale_x_continuous(expand = c(0,0), limits = c(1,13), breaks = seq(1:12),
                     labels = c('January', "*", "*", "April", '*', '*', 'July', '*', '*', '  October', '*', '*')) +
  scale_y_continuous(expand = c(0,0)) +
  scico::scale_color_scico_d(palette = 'bilbao', name = '') +
  scico::scale_fill_scico_d(palette = 'bilbao', name = '') +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 14),
        legend.background = element_rect(fill='#f4f2f0', color = NA),
        legend.box.background = element_rect(fill='#f4f2f0', color = NA),
        panel.background = element_rect(fill = '#f4f2f0', colour = NA),  
        plot.background = element_rect(fill = '#f4f2f0', colour = NA),
        strip.background = element_rect(fill = '#f4f2f0', color = NA))
ggsave('20250624.png', width = 8, height = 6, dpi = 300)


