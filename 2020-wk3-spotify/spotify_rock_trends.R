

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidylog)
library(lubridate)
library(showtext)

# Load data ---------------------------------------------------------------

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


font_add_google("Roboto", "roboto")
showtext_auto()

spotify_songs %>% 
  mutate(release_year = ymd(track_album_release_date),
         year = year(release_year)) %>% 
  filter(playlist_genre == "rock") %>% 
  group_by(year) %>% 
  summarise_at(vars(acousticness, danceability, duration_ms, energy, loudness, tempo), mean) %>% 
  pivot_longer(
    cols = -year
  ) %>% 
  ggplot(aes(x = year, y = value, 
             color = ifelse(name %in% c('acousticness', 'energy', 'loudness'), "highlight", "don't highlight"))) +
  geom_smooth(se = FALSE) +
  geom_point(size = 4, alpha = 0.8, fill = "white") +
  scale_color_manual(values = c("grey", "darkorange")) +
  labs(title = "Rock Songs Getting Louder, Heavier and More Energetic Over Time",
       subtitle = "Rock songs from 1960-2020 from Spotify Playlist Data",
       y = "",
       x = "",
       caption = "Plot: Jason Skelton | Data: Spotify") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "roboto"),
    plot.title = element_text(size = 30, face = "bold", color = "slategrey"),
    plot.subtitle = element_text(size = 20, margin = margin(0,0,15,0), color = "slategrey"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "slategrey", fill = NA),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.ticks.x = element_line(color = "slategrey"),
    strip.background = element_rect(fill = "snow2"),
    strip.text = element_text(color = "slategrey", face = "bold", size = 14),
    plot.caption = element_text(size = 12, color = "slategrey")
  )

ggsave('spotify_rock_trends.png', dpi = 150)

