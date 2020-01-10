

# Load libraries ----------------------------------------------------------

library(ggplot2)
library(lubridate)
library(tidylog)
library(dplyr)
library(readr)
library(showtext)
library(ggforce)



# Load data ---------------------------------------------------------------


temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')



# Create visualization of average max temperatures ------------------------

# 2019 data only includes Jan-May, so let's look at YTD average max temperatures up unitl end of May for each year

annual_max_temp <- temperature %>% 
  filter(temp_type == 'max' & month(date) < 6) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(avg_max = mean(temperature, na.rm = TRUE))


# Use Roboto font from Google fonts
font_add_google("Roboto", "roboto")
showtext_auto()


ggplot(annual_max_temp, aes(x = year, y = avg_max)) +
  geom_line(colour = "darkgray") +
  geom_point(size = 3, colour = "firebrick") +
  scale_x_continuous(breaks = seq(1910, 2019, 5)) +
  scale_y_continuous(breaks = seq(20, 27, 1)) +
  geom_mark_rect(aes(filter = year == 2019, 
                     fill = 'firebrick', 
                     alpha = 0.1
                     )
                 ) +
  annotate(geom = "text", x = 2010, y = 26.2, fontface = 'bold', size = 5, colour = 'firebrick',
           label = paste0(round(max(annual_max_temp$avg_max),1), " celcius")) +
  theme_minimal() +
  labs(title = "Max Temperatures in Australia have been Steadily Increasng In Recent Years",
       subtitle = "YTD (Jan-May) Average Max Temperatures Over Time in Australia",
       x = "Year",
       y = "Average Max Temperature (Jan-May)",
       caption = "Source: TidyTuesday | Created By: Jason Skelton") +
  theme(
    legend.position = "none",
    text = element_text(family = 'roboto'),
    plot.title = element_text(size = 26, face = 'bold', colour = 'firebrick'),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(colour = '#8585ad'),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_line(),
    axis.title = element_text(face = 'bold'),
    axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 15, 0, 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line()
  )
       
ggsave('aus_temperature_trend.png', dpi = 150)
