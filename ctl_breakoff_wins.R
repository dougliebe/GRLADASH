# functions for records dash
## Trying to recreate the excel from last year
library(odbc)
library(DBI)
library(tidyverse)
source("helper_get_hillside_data.R")
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
killfeed_q <- tbl(con, "KILLFEED")

## line for each engagement with pertinent info
killfeed_q %>%
  # filter(GAME_ID == 767) %>%
  # group_by(GAME_ID) %>%
  left_join(game_q %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE)) %>%
  filter(MODE_ID == 3) %>%
  collect() ->
  pbp
pbp %>%
  # filter(GAME_ID == 1023) %>%
  pivot_longer(c(PLAYER_A, PLAYER_B),
               names_to = "type",
               values_to = "player") %>%
  mutate(TEAM = ifelse(tolower(player) %in% c("vivid","assault", "silly", "apathy"),
                       "LAG", "THEM")) %>%
  mutate(type = ifelse(type == "PLAYER_A", 'kill', 'death')) %>%
  select(GAME_ID, TIME_S, TEAM, type, player) ->
  pbp_long_lag

ctl_games <- get_CTL_games(30)

ctl_games %>%
  filter(GAME_ID %in% unique(pbp_long_lag$GAME_ID)) %>%
  mutate(TEAM = ifelse(TEAM_ID == 6, "LAG", "THEM")) %>%
  select(GAME_ID, TEAM_ID, TEAM, MAP_ID, DATE, round, offense, win, TIME_S) %>%
  bind_rows(pbp_long_lag) %>%
  arrange(GAME_ID, TEAM, TIME_S) %>%
  group_by(GAME_ID, TEAM) %>%
  fill(offense, MAP_ID, round,.direction = 'up') %>%
  filter(!is.na(round)) ->
  # pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  ctl_combined

ctl_combined %>%
  # filter(GAME_ID == 1023) %>%
  group_by(GAME_ID, TEAM, round) %>%
  mutate(time_into_round = TIME_S - min(TIME_S),
         lives = 30-(cumsum(type == 'death'))) %>%
  filter(time_into_round <= 15) %>%
  group_by(GAME_ID, TEAM, round, MAP_ID, offense) %>%
  summarise(kill_diff = sum(type == "kill") - sum(type == "death")) %>%
  group_by(MAP_ID, offense, TEAM) %>%
  summarise(kill_diff_30 = mean(kill_diff, na.rm = T),
            win_pct = mean(kill_diff > 0, na.rm = T)) %>%
  filter(TEAM == "LAG") %>%
  left_join(map_q %>% collect()) %>%
  select(-MAP_ID)
  ggplot(aes(TIME_S, lives, color = TEAM))+
  geom_line() +
  facet_wrap(~GAME_ID)




