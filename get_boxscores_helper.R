## get boxscore stats from killfeed + games data
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
killfeed_q <- tbl(con, "KILLFEED")

## get killfeed data by game, player

killfeed_q %>%
  # filter(GAME_ID == 767) %>%
  group_by(GAME_ID) %>%
  collect() %>%
  pivot_longer(c(PLAYER_A, PLAYER_B),
               names_to = "type",
               values_to = "player") ->
  pbp_long

pbp_long %>%
  arrange(GAME_ID, player) %>%
  group_by(GAME_ID, player) %>%
  mutate(tsla =  pmin(15, TIME_S - lag(TIME_S, default = 0)),
         life = cumsum(lag(type, default = "PLAYER_B") == "PLAYER_B")) %>%
  group_by(life, .add = T) %>%
  mutate(streak = 1:n()-1,
         pdk = tsla < 5000 & streak == 0,
         last_kill_id = lag(KILL_ID),
         last_kill_s = TIME_S - lag(TIME_S, default = 0),
         in_trade_eng = streak >= 1 & last_kill_s < 5 & !pdk,
         win = type == "PLAYER_A") %>%
  ungroup() %>%
  select(GAME_ID, KILL_ID, type, last_kill_id, in_trade_eng, win) ->
  trade_kills

killfeed_q %>%
  # filter(GAME_ID == 767) %>%
  collect() %>%
  group_by(GAME_ID) %>%
  left_join(trade_kills %>%
              arrange(GAME_ID, KILL_ID) %>%
              filter(in_trade_eng) %>%
              select(-KILL_ID),
            by = c("GAME_ID", 'KILL_ID' = 'last_kill_id')) %>%
  replace_na(list('in_trade_eng' = FALSE)) ->
  was_traded


was_traded %>%
  pivot_longer(c(PLAYER_A, PLAYER_B),
               values_to = "player") %>%
  mutate(TEAM_ID = ifelse(name == "PLAYER_A", TEAM_A_ID, TEAM_B_ID),
         player = tolower(parse_character(player))) %>%
  group_by(GAME_ID, player, name, TEAM_ID, traded=in_trade_eng) %>%
  count() %>%
  mutate(name = recode(name, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  pivot_wider( names_from = c(name, traded), values_from = n) %>%
  mutate(across(starts_with(c("DEATH", "KILL")), 
                ~replace_na(.x, replace = 0))) %>%
  mutate(kills = (KILL_FALSE+KILL_TRUE),
         deaths = (DEATH_TRUE+DEATH_FALSE),
         kd = kills/deaths,
         engs = kills+deaths,
         corrected_pm = KILL_FALSE + (0.2*KILL_TRUE - 0.2*DEATH_TRUE) +- DEATH_FALSE) ->
  all_games_trade_box 

## now get GAMES data, duration, winning team, map, mode
game_q %>%
  left_join(series_q) %>%
  collect() %>%
  mutate(WINNING_TEAM = ifelse(TEAM_A_SCORE > TEAM_B_SCORE, A_ID, B_ID)) %>%
  select(GAME_ID,DATE, MODE_ID, MAP_ID, DURATION, WINNING_TEAM) ->
  all_games_win_box

all_games_trade_box %>%
  left_join(all_games_win_box) %>%
  mutate(WIN = TEAM_ID == WINNING_TEAM) ->
  all_players_box

