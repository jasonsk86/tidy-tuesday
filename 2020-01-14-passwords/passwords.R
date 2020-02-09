

# Load libraries ---------------------------------------------------------

library(tidyverse)
library(tidylog)
library(gghighlight)
library(ggforce)
library(glue)
library(showtext)

# Load data ---------------------------------------------------------------

passwords_orig <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
passwords <- passwords_orig


# Data prep ---------------------------------------------------------------


# remove NAs and get time to hack online onto same scale (seconds)
passwords <- passwords %>% 
  filter(!is.na(password)) %>% 
  mutate(online_crack_seconds = case_when(
    time_unit == "years" ~ value * 3.154e+7,
    time_unit == "months" ~ value * 2.628e+6,
    time_unit == "weeks" ~ value * 604800,
    time_unit == "days" ~ value * 86400,
    time_unit == "hours" ~ value * 3600,
    time_unit == "minutes" ~ value * 60,
    TRUE ~ value
    ),
    online_crack_days = online_crack_seconds / 86400
  )


fluffy_worst <- passwords %>% 
  filter(category == "fluffy") %>% 
  arrange(online_crack_days) %>% 
  head(3) %>% 
  pull(password)


# Plot data ---------------------------------------------------------------


# load lato font from google
font_add_google("Lato", family = "lato")
showtext_auto()

passwords %>% 
  group_by(category) %>% 
  summarise(online_time = mean(online_crack_days)) %>% 
  ggplot() +
  geom_bar(stat = "identity", fill = "firebrick",
           aes(x = reorder(category, online_time), y = online_time)) +
  gghighlight(category == "fluffy") +
  annotate(geom = "text", x = "fluffy", y = 650, 
           label = glue("Worst offenders include {fluffy_worst[1]}, {fluffy_worst[2]} and {fluffy_worst[3]}"),
           color = "firebrick", size = 5) +
  labs(title = "Stop Using 'Fluffy' Passwords If You Don't Want to Be Hacked",
       subtitle = "Average Time to Crack Password Online by Password Categorisation",
       y = "Average Time to Crack Password Online (days)",
       caption = "Plot: Jason Skelton | Data: Information is Beautiful") +
  scale_y_continuous(breaks = seq(0, 2500, 250), expand = c(0,0)) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "lato"),
    plot.title = element_text(size = 26, face = "bold", margin = margin(0,0,10,0)), 
    plot.subtitle = element_text(size = 18, margin = margin(0,0,10,0)),
    plot.caption = element_text(size = 13, color = "#706968", face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 16),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(15,0,0,0), face = "bold", size = 16),
    axis.line = element_line(color = "darkgray"),
    axis.ticks = element_line(color = "darkgray")
  )


ggsave('passwords.png', dpi = "retina")
            