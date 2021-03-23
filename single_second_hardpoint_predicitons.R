### Predicting expected value of each hill
### Using gaem-state predictors

library(tidyverse)
library(odbc)
library(DBI)
library(tidyverse)

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "callofduty",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)
ref_q <- tbl(con, "REFERENCE")
series_q <- tbl(con, 'SERIES')
game_q <- tbl(con, 'GAME')
score_q <- tbl(con, 'SCORE')
map_q <- tbl(con, "MAP")
team_q <- tbl(con, "TEAM")
mode_q <- tbl(con, "MODE")

## Get data to train/test models
data <- score_q %>%
  left_join(game_q %>%
              select(MAP_ID, MODE_ID, GAME_ID), by = "GAME_ID") %>%
  collect() %>%
  pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
  group_by(GAME_ID) %>%
  filter(score >= 0,
         # TIME_S > 4, 
         score < 250,
         # MAP_ID < 46,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249),
         MODE_ID == 1,
         score == cummax(score)
  ) %>%
  # left_join(hill_num, by = "")
  mutate(hill = pmax((TIME_S-5) %/% 60, 0)+1,
         seconds_into_hill = ((TIME_S-5) %% 60)+1,
         total_hills = ifelse(MAP_ID %in% c(43,44, 46, 49), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills))

data %>%
  group_by(GAME_ID, team) %>%
  mutate(rotation = cumsum(hill_no < lag(hill_no, default = 0)),
         first_rot = total_hills*60) %>%
  group_by(rotation, .add = T) %>%
  mutate(rot_score = max(score)-min(score),
         gain = score > lag(score)) ->
  gain_by_rotation

gain_by_rotation %>%
  filter(MAP_ID != 0,MAP_ID != 50, !is.na(gain), TIME_S < first_rot+5) %>%
  group_by(MAP_ID, TIME_S,  gain) %>%
  summarise(points = mean(rot_score)) %>%
  group_by(MAP_ID, gain) %>%
  mutate(points = zoo::rollmeanr(points, k = 5, fill = NA)) %>%
  # group_by(MAP_ID) %>%
  # mutate(points = points/max(points, na.rm = T)) %>%
  left_join(map_q %>% collect()) %>%
  ggplot(aes(TIME_S, points, color = gain))+
  geom_vline(xintercept = seq(5, 300, 60), alpha = 0.3) + geom_line(size = 1)+
  facet_wrap(~MAP_NAME, scales = 'free') +
  theme_bw() +
  scale_color_discrete(labels = c("FALSE" = "DIDNT SCORE", "TRUE" = "SCORED"),
                       name = element_blank()) +
  ggtitle("Predicted Full Rotation Score", "if you scored at any specific second")
  
gain_by_rotation %>%
  filter(MAP_ID != 0,MAP_ID != 50, !is.na(gain), TIME_S < first_rot+5) %>%
  group_by(MAP_ID, TIME_S,  gain) %>%
  summarise(points = mean(rot_score)) %>%
  group_by(MAP_ID, gain) %>%
  mutate(points = zoo::rollmeanr(points, k = 5, fill = NA)) %>%
  group_by(MAP_ID,TIME_S) %>%
  mutate(points = points-(sum(points)-points)) %>%
  filter(gain) %>%
  left_join(map_q %>% collect()) %>%
  ggplot(aes(TIME_S, points))+
  geom_vline(xintercept = seq(5, 300, 60), alpha = 0.3) +
  geom_hline(yintercept = 0,) +
  geom_line(size = 1, color = "blue")+
  facet_wrap(~MAP_NAME, scales = 'free') +
  theme_bw() +
  scale_color_manual(labels = c("FALSE" = "DIDNT SCORE", "TRUE" = "SCORED"),
                       name = element_blank()) +
  ggtitle("Difference in Predicted Rotation Score", "if you scored at any specific second")

