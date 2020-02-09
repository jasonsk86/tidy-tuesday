

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(fmsb)
library(showtext)


# Load data ---------------------------------------------------------------

standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

# evaluate the last fiver super bowl winning teams across four metrics 
# offensive ranking
# defensive ranking
# strength of schedule - how hard/easy was the route to the super bowl
# points differential 


sb_data <- standings %>% 
  filter(sb_winner == 'Won Superbowl' & year >= 2015) %>% 
  select(team_name, year, offensive_ranking, defensive_ranking, strength_of_schedule, points_differential) %>% 
  mutate(team_year = paste0(team_name, "-", year))



# Plot Data ------------------------------------------------------------


# > Offensive ranking -----------------------------------------------------
off <- sb_data %>% 
  select(team_year, offensive_ranking) %>% 
  pivot_wider(names_from = team_year,
              values_from = offensive_ranking)

off_rat <- rbind(rep(7, 5), rep(0.3, 5), off)


# > Defensive ranking -----------------------------------------------------
def <- sb_data %>% 
  select(team_year, defensive_ranking) %>% 
  pivot_wider(names_from = team_year,
              values_from = defensive_ranking)

def_rat <- rbind(rep(6,5), rep(2,5), def)


# > Schedule strength -----------------------------------------------------
sched <- sb_data %>% 
  select(team_year, strength_of_schedule) %>% 
  pivot_wider(names_from = team_year,
              values_from = strength_of_schedule)

sched_rat <- rbind(rep(3,5), rep(-3,5), sched)


# > Points Differential -----------------------------------------------------
diff <- sb_data %>% 
  select(team_year, points_differential) %>% 
  pivot_wider(names_from = team_year,
              values_from = points_differential)

diff_rat <- rbind(rep(200,5), rep(50,5), diff)



# Plots  ----------------------------------------------------------

par(mar=c(5.1, 4.1, 4.1, 2.1))

par(mfrow = c(2, 2)) 


radarchart(off_rat, axistype=1 ,
           #custom polygon
           pcol=rgb(0.2,0.2,0.5,0.9) , pfcol=rgb(0.2,0.2,0.5,0.5) , plwd=4 ,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1, 7, 2), cglwd=0.8,
           #custom labels
           vlcex=1.2,
           title=paste("Offensive Rating"),
           cex.main = 2)

radarchart(def_rat, axistype=1 ,
           #custom polygon
           pcol=rgb(0.1,0.2,0.5,0.9) , pfcol=rgb(0.1,0.2,0.5,0.5) , plwd=4 ,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(2, 6, 1), cglwd=0.8,
           #custom labels
           vlcex=1.2,
           title=paste("Defensive Rating"),
           cex.main = 2)


radarchart(sched_rat, axistype=1 ,
           #custom polygon
           pcol=rgb(0.1,0.6,0.4,0.9) , pfcol=rgb(0.1,0.6,0.4,0.5) , plwd=4 ,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(-3, 3, 1.8), cglwd=0.8,
           #custom labels
           vlcex=1.2,
           title=paste("Strength of Schedule"),
           cex.main = 2)


radarchart(diff_rat, axistype=1,
                      #custom polygon
                      pcol=rgb(0.9,0.6,0.4,0.9) , pfcol=rgb(0.9,0.6,0.4,0.5) , plwd=4 ,
                      #custom the grid
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(50, 200, 40), cglwd=0.8,
                      #custom labels
                      vlcex=1.2,
                      title=paste("Points Differential"),
                      cex.main = 2)



