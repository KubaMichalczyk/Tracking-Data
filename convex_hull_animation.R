library(tidyverse)
library(gganimate)
library(tweenr)
source('my_pitch_plot.R')

data <- read_csv('csv_files\\sequence_2.csv') 
data <- data %>% select(-X1)
names(data) <- c(paste('team A_', rep(1:11, each = 2), '_', c('x', 'y'), sep = ''), 
                 paste('team B_', rep(1:11, each = 2), '_', c('x', 'y'), sep = ''), 
                 paste('ball_0_', c('x', 'y'), sep = ''))

data <- data %>% 
  mutate(time = row_number()) %>% 
  gather(key, value, -time) %>% 
  extract(key, into = c('team', 'player', 'coordinate'), regex = '(.+)_(.+)_([xy])') %>% 
  spread(coordinate, value)

# coordinates are recorded every 0.1 second. To ensure we have a smooth animation we can interpolate the
# coordinates with tweenr package
data <- data %>% 
  arrange(time, team, player) %>% 
  unite(group, team, player) %>% 
  mutate(ease = 'linear') %>%
  tween_elements('time', 'group', 'ease', nframes = 2 * max(.$time)) %>%
  separate(col = .group, into = c('team', 'player'), sep = '_')

# calculating convex hull for each team and .frame. I exclude the goalkeepers as the convex hull with
# only outfield players gives a nice visualisation of defence line disposure and therefore is more informative
data_hull <- data %>%
  filter(player != 1) %>% 
  group_by(team, .frame) %>%
  nest() %>%
  mutate(
    hull = map(data, ~ with(.x, chull(x, y))),
    out = map2(data, hull, ~ .x[.y,,drop=FALSE])
  ) %>%
  select(-data) %>%
  unnest()

p <- pitch_plot(68, 105) +
  geom_polygon(data = filter(data_hull, team == 'team A'), aes(x, y, frame = .frame), fill = 'red', alpha = 0.4) +
  geom_polygon(data = filter(data_hull, team == 'team B'), aes(x, y, frame = .frame), fill = 'blue', alpha = 0.4) +
  geom_point(data = filter(data, str_detect(team, 'team')), aes(x, y, group = player, fill = team, frame = .frame), shape = 21, size = 6, stroke = 2) +
  geom_point(data = filter(data, team == 'ball'), aes(x, y, frame = .frame), shape = 21, fill = 'dark orange', size = 4) +
  scale_fill_manual(values = c('team A' = 'red', 'team B' = 'blue')) +
  geom_text(data = filter(data, str_detect(team, 'team')), aes(x, y, label = player, frame = .frame), color = 'white') +
  guides(fill = FALSE)

animation::ani.options(ani.width = 735, ani.height = 476)
gganimate(p, 'sequence_2.gif', interval = 0.05, title_frame = FALSE)