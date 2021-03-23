
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
  left_join(game_q %>% mutate(win = ifelse(TEAM_A_SCORE > TEAM_B_SCORE,1,0)) %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE) %>%
              left_join(series_q)) %>%
  filter(MODE_ID == 1, A_ID == 6 | B_ID == 6) %>%
  collect() ->
  killfeed_HP

killfeed_HP %>%
  right_join(score_q %>% filter(GAME_ID %in% !!killfeed_HP$GAME_ID) %>% collect()) ->
  kills_score_HP

kills_score_HP %>%
  group_by(GAME_ID) %>%
  fill(MAP_ID, win) %>%
  mutate(lag_score = case_when(
    A_ID == 6 ~ TEAM_A_SCORE,
    TRUE ~ TEAM_B_SCORE
  ),
  opp_score = case_when(
    A_ID == 6 ~ TEAM_B_SCORE,
    TRUE ~ TEAM_A_SCORE
  ),
  lag_win = case_when(
    A_ID == 6~win==1,
    TRUE ~ !(win==1)
  ),
  lag_diff = lag_score - opp_score,
  lag_diff = lag(lag_diff, 60),
  # score_30 = lead(lag_score, 120) > lead(opp_score, 120),
  ttw = 250-pmax(lag_score, opp_score),
  kills = !is.na(KILL_ID)) %>%
  arrange(GAME_ID, TIME_S) %>% 
  group_by(GAME_ID, MAP_ID) %>%
  mutate(rolling_epm = zoo::rollmeanr(kills, k = 60,fill = NA )*60,
         diff_breaks = cut(lag_diff, breaks = seq(-200, 200, 10)),
         time_to_win = cut(ttw, breaks = seq(0, 250, 25))) %>%
  group_by(MAP_ID) %>%
  # mutate(pct_epm = rolling_epm/mean(rolling_epm*(lag_diff == 0), na.rm = T)) %>%
  ungroup() %>%
  filter(abs(lag_diff)< 50, rolling_epm > 10, MAP_ID != 42) %>%
  left_join(map_q %>% collect()) %>%
  ggplot(aes( lag_diff, rolling_epm))+
  # geom_density_2d_filled()+
  # geom_point() +
  geom_smooth(color = 'black') +
  facet_wrap(~MAP_NAME, scales = 'free')+
  labs(x = "score differential",
       y = "Pace over next 60s")
