# functions for records dash
## Trying to recreate the excel from last year
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



## line for each engagement with pertinent info
killfeed_q %>%
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

## get score data with side/hill for every second


score_q %>%
  inner_join(game_q %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE),
             by = "GAME_ID") %>%
  left_join(series_q,
            by = "SERIES_ID") %>%
  
  collect() ->
  scores



scores %>%
  select(MODE_ID, MAP_ID, GAME_ID,
         TEAM_A_SCORE, TEAM_B_SCORE,
         TIME_S,
         A_ID, B_ID) %>%
  group_by(GAME_ID) %>%
  mutate(round = 1:n()) %>%
  pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
  mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID)) %>%
  select(-A_ID, -B_ID) %>%
  filter(TEAM_ID != 6) %>%
  group_by(GAME_ID) %>%
  # mutate(TIME_S = TIME_S - 5) %>%
  filter(score >= 0,
         TIME_S >= 6,
         score < 250,
         # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
         !cumany(score == 249)
  ) %>%
  group_by(team, .add = T) %>%
  filter(score == cummax(score)) %>%
  # left_join(hill_num, by = "")
  mutate(hill = pmax((TIME_S-6) %/% 60, 0)+1,
         seconds_into_hill = ((TIME_S-6) %% 60)+1,
         new_hill = ifelse(hill > lag(hill), hill, NA),
         total_hills = ifelse(MAP_ID %in% c(43,44, 46, 49,51), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills),
         side = case_when(
           team == "TEAM_A_SCORE" & round%%2==1 ~ "OFF",
           team == "TEAM_A_SCORE" & round%%2==0 ~ "DEF",
           team == "TEAM_B_SCORE" & round%%2==1 ~ "DEF",
           TRUE ~ "OFF"
           ) 
         ) %>%
  group_by(GAME_ID, team, hill) %>%
  mutate(rotated = (hill > 1 & 
                      score - min(score) >= 2 & 
                      seconds_into_hill == 5),
         rotated = cumany(rotated)) %>%
  group_by(GAME_ID, team) %>%
  mutate(gain = score - lag(score), 
         running_gain = zoo::rollsumr(gain, k = 10, fill = 0),
         running_120 = zoo::rollsumr(gain, k = 120, fill = 0),
         ended = (running_gain > 5 & 
                    seconds_into_hill == 58)) %>%
  filter(score == cummax(score)) %>%
  group_by(GAME_ID, TIME_S) %>%
  mutate(broken = running_gain == 0 & rotated & sum(running_gain) > 0) %>%
  group_by(GAME_ID, team, hill) %>%
  mutate(broken = cumany(broken),
         hold = !cumany(broken) & rotated) %>%
  group_by(GAME_ID, hill) %>%
  mutate(broke = cumany(broken) & !broken) %>%
  group_by(GAME_ID, team, hill, hill_no, broken) %>%
  mutate(first_broken = (row_number() == 1 & broken)*seconds_into_hill) %>%
  group_by(GAME_ID, team, hill, hill_no, broke) %>%
  mutate(first_broke = (row_number() == 1 & broke)*seconds_into_hill) ->
  pp_data


## now combine score info per second and kills
pp_data %>%
  inner_join(was_traded, by = c("GAME_ID", "TIME_S")) %>%
  inner_join(game_q %>% left_join(series_q) %>% filter(DATE >= "2021-04-15") %>%
               collect() %>% select(GAME_ID, SERIES_ID)) %>%
  # group_by(SERIES_ID) %>% count()
  # filter(SERIES_ID == 138) %>%
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(hill_side = ifelse(MODE_ID == 1, as.character(hill_no), side)) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name, traded=in_trade_eng) %>%
  summarise(n = n(), games = length(unique(GAME_ID))) %>%
  # left_join(was_traded %>% group_by(GAME_ID) %>% summarise(duration = max(TIME_S)-min(TIME_S)),
  #           by = "GAME_ID") %>%
  ungroup() %>%
  mutate(name = recode(name, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name, traded) %>%
  summarise(n = sum(n)) %>%
  pivot_wider(id_cols = c(hill_side, MAP_ID, MODE_ID, value),
              names_from = c(name, traded),
              values_from = n) %>%
  mutate(across(starts_with(c("DEATH", "KILL")), 
                ~replace_na(.x, replace = 0))) %>%
  mutate(untraded_KILL = KILL_FALSE/(KILL_FALSE+KILL_TRUE),
         traded_deaths = DEATH_TRUE/(DEATH_TRUE+DEATH_FALSE),
         kd = (KILL_FALSE+KILL_TRUE)/(DEATH_TRUE+DEATH_FALSE),
         # epm = (KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/duration*60,
         pm = KILL_FALSE + 0.2*KILL_TRUE - 0.2*DEATH_TRUE - DEATH_FALSE,
         value = tolower(value)) %>%
  filter((value %in% c('mjcheen','cheen',"vivid","assault", "silly", "apathy"))) ->
  all_games_trade_box 

## add durations and get season averages
pp_data %>%
  inner_join(was_traded, by = c("GAME_ID", "TIME_S")) %>%
  inner_join(game_q %>%
               left_join(series_q) %>%
               filter(DATE > "2021-03-10") %>%
               collect() %>%
               select(GAME_ID, SERIES_ID, DURATION)) %>%
  
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(value = tolower(value)) %>%
  group_by(GAME_ID, MAP_ID, MODE_ID, value, name) %>%
  count() %>%
  ungroup() %>%
  mutate(name = recode(name, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  # # group_by(GAME_ID, MAP_ID, MODE_ID, value, name, traded) %>%
  # # summarise(n = sum(n)) %>%
  pivot_wider(id_cols = c(GAME_ID, MAP_ID, MODE_ID, value),
              names_from = c(name),
              values_from = n) %>%
  mutate(across(starts_with(c("DEATH", "KILL")),
                ~replace_na(.x, replace = 0))) %>%
  # mutate(untraded_KILL = KILL_FALSE/(KILL_FALSE+KILL_TRUE),
  #        traded_deaths = DEATH_TRUE/(DEATH_TRUE+DEATH_FALSE),
  #        kd = (KILL_FALSE+KILL_TRUE)/(DEATH_TRUE+DEATH_FALSE),
  #        # epm = (KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/duration*60,
  #        pm = KILL_FALSE + 0.2*KILL_TRUE - 0.2*DEATH_TRUE - DEATH_FALSE,
  #        value = tolower(value)) %>%
  filter((value %in% c('mjcheen','cheen',"vivid","assault", "silly", "apathy"))) %>%
  left_join(was_traded %>% group_by(GAME_ID) %>% summarise(duration = max(TIME_S)-min(TIME_S)),
            by = "GAME_ID") %>%
  group_by(MAP_ID, value) %>%
  summarise(kpm = sum(KILL)/sum(duration)*600,
            dpm = sum(DEATH)/sum(duration)*600,
            epm = sum(DEATH+KILL)/sum(duration)*600,
            duration = sum(duration)) %>%
  filter(MAP_ID == 44)
  
  
## Try to get per hill numbers
pp_data %>%
  inner_join(was_traded, by = c("GAME_ID", "TIME_S")) %>%
  inner_join(game_q %>% left_join(series_q) %>% filter(DATE > "2021-02-15") %>% collect() %>% select(GAME_ID, SERIES_ID)) %>%
  # filter(SERIES_ID == 138) %>%
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(hill_side = ifelse(MODE_ID == 1, as.character(hill_no), side),
         value = tolower(value)) %>%
  group_by(GAME_ID, hill_side, hill,  MAP_ID, MODE_ID, value, name, traded=in_trade_eng) %>%
  count() %>%
  # left_join(was_traded %>% group_by(GAME_ID) %>% summarise(duration = max(TIME_S)-min(TIME_S)),
  #           by = "GAME_ID") %>%
  ungroup() %>%
  mutate(name = recode(name, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name) %>%
  summarise(n = sum(n), hill_times = n()) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value) %>%
  mutate(hill_times = max(hill_times)) %>%
  pivot_wider(id_cols = c(hill_side, MAP_ID, MODE_ID, value, hill_times),
              names_from = c(name),
              values_from = n) %>%
  mutate(across(starts_with(c("DEATH", "KILL")), 
                ~replace_na(.x, replace = 0))) %>%
  filter((value %in% c('mjcheen','cheen',"vivid","assault", "silly", "apathy"))) 
  group_by(MAP_ID,hill_side ,value) %>%
  summarise(kpm = sum(KILL)/sum(hill_times),
            dpm =  sum(DEATH)/sum(hill_times),
            epm = sum(DEATH+KILL)/sum(hill_times),
            n = n()) %>%
  filter(MAP_ID == 44, hill_side == 4)
  


all_games_trade_box %>%
  ungroup() %>%
  
  group_by(value, MAP_ID) %>%
  summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
            traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),) %>%
  filter(value == 'silly')
## games in set
pp_data %>%
  inner_join(was_traded, by = c("GAME_ID", "TIME_S")) %>%
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(hill_side = ifelse(MODE_ID == 1, as.character(hill_no), side)) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name, traded=in_trade_eng) %>%
  summarise(n= n(),
            games = length(unique(GAME_ID))) %>%
  filter(tolower(value) %in% c("vivid","assault", "silly", "apathy"), hill_side == 1) 

## pm by hill
all_games_trade_box %>%
  filter(MODE_ID %in% c(1)) %>%
  left_join(map_q %>% collect() , by = "MAP_ID") %>%
  ggplot(aes(as.numeric(hill_side), pm, color = value))+
  # geom_col(position = position_dodge(0.5)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~MAP_NAME,scales = 'free_x') +
  theme_bw()+
  labs(x = "HILL", y = "adj. +/-",
       title = "Adjusted Plus-Minus",
       subtitle = "Less weight to net-zero engaegments: \ntraded deaths & tradable kills")
# traded_deaths
all_games_trade_box %>%
  filter(MODE_ID %in% c(1)) %>%
  left_join(map_q %>% collect() , by = "MAP_ID") %>%
  ggplot(aes(as.numeric(hill_side), traded_deaths, color = value))+
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~MAP_NAME,scales = 'free') +
  theme_bw()+
  labs(x = "HILL", y = "Deaths Traded, %")
# kd
all_games_trade_box %>%
  filter(MODE_ID %in% c(1)) %>%
  left_join(map_q %>% collect() , by = "MAP_ID") %>%
  ggplot(aes(as.numeric(hill_side), kd/(1+kd), color = value))+
  geom_hline(yintercept = 0.5, alpha = 0.5) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~MAP_NAME,scales = 'free_x') +
  theme_bw()+
  labs(x = "HILL", y = "KD")


## now combine score info per second and kills
## Fix for CTL
pp_data %>%
  filter(MODE_ID == 3) %>%
  bind_rows(was_traded) %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, TIME_S) %>%
  fill(side, MAP_ID, MODE_ID, round, .direction = 'up') %>%
  # select(GAME_ID,MODE_ID, TIME_S, side, PLAYER_A) %>% tail
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(hill_side = ifelse(MODE_ID == 1, as.character(hill_no), side)) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name, traded=in_trade_eng) %>%
  count() %>%
  # left_join(was_traded %>% group_by(GAME_ID) %>% summarise(duration = max(TIME_S)-min(TIME_S)),
  #           by = "GAME_ID") %>%
  ungroup() %>%
  mutate(name = recode(name, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name, traded) %>%
  summarise(n = sum(n)) %>%
  pivot_wider(id_cols = c(hill_side, MAP_ID, MODE_ID, value),
              names_from = c(name, traded),
              values_from = n) %>%
  mutate(across(starts_with(c("DEATH", "KILL")), 
                ~replace_na(.x, replace = 0))) %>%
  mutate(untraded_KILL = KILL_FALSE/(KILL_FALSE+KILL_TRUE),
         traded_deaths = DEATH_TRUE/(DEATH_TRUE+DEATH_FALSE),
         kd = (KILL_FALSE+KILL_TRUE)/(DEATH_TRUE+DEATH_FALSE),
         # epm = (KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/duration*60,
         pm = KILL_FALSE + 0.2*KILL_TRUE - 0.2*DEATH_TRUE - DEATH_FALSE,
         value = tolower(value)) %>%
  filter(value %in% c("vivid","assault", "silly", "apathy")) ->
  ctl_games_trade_box 

## How many games in dataset
pp_data %>%
  filter(MODE_ID == 3) %>%
  bind_rows(was_traded) %>%
  group_by(GAME_ID) %>%
  arrange(GAME_ID, TIME_S) %>%
  fill(side, MAP_ID, MODE_ID, .direction = 'up') %>%
  # select(GAME_ID,MODE_ID, TIME_S, side, PLAYER_A) %>% tail
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(hill_side = ifelse(MODE_ID == 1, as.character(hill_no), side)) %>%
  group_by(hill_side, MAP_ID, MODE_ID, value, name, traded=in_trade_eng) %>%
  summarise(n= n(),
            games = length(unique(GAME_ID))) %>%
  filter(tolower(value) %in% c("vivid","assault", "silly", "apathy")) 
# kd
ctl_games_trade_box %>%
  filter(MODE_ID %in% c(3)) %>%
  left_join(map_q %>% collect() , by = "MAP_ID") %>%
  ggplot(aes(hill_side, pm, fill = value))+
  geom_col(position = position_dodge(0.5)) +
  # geom_line() +
  # geom_point(size = 2) +
  facet_wrap(~MAP_NAME,scales = 'free_x') +
  theme_bw()+
  labs(x = element_blank(), y = "adj. +/-",
       title = "Adjusted Plus-Minus",
       subtitle = "Less weight to net-zero engaegments: \ntraded deaths & tradable kills")

## draw graphs of lives over time in ctl
killfeed_q %>%
  left_join(game_q, by = "GAME_ID") %>%
  filter(MODE_ID == 3) %>%
  collect() ->
  ctl_killfeed
  
ctl_killfeed %>%
  bind_rows(scores %>%
              filter(MODE_ID == 3) %>%
              group_by(GAME_ID) %>%
              # pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
              #              names_to = 'team', values_to = "score") %>%
              mutate(round = (1:n())-1) %>%
              mutate(
                team_off = ifelse(round%%2==1, A_ID, B_ID)
              )) %>%
  arrange(GAME_ID, TIME_S) %>%
  group_by(GAME_ID) %>%
  fill(team_off, MAP_ID, MODE_ID, round, .direction = 'up') %>%
  filter(!is.na(KILL_ID)) ->
  ctl_kill_with_rounds
  
ctl_kill_with_rounds %>%
  group_by(GAME_ID) %>%
  # filter(GAME_ID == 534) %>%
  group_by(GAME_ID,MAP_ID, round, team_off, TEAM_B_ID) %>%
  summarise(min_kills = min(30-(1:n()))) %>%
  group_by(GAME_ID,MAP_ID, round,) %>%
  mutate(round_diff = min_kills - (sum(min_kills)-min_kills)) %>%
  filter(team_off == TEAM_B_ID, round != 5,
         MAP_ID %in% c(43, 44,49)) %>%
  # ungroup() %>%
  # summarise(m = mean(round_diff))
  # group_by(MAP_ID)
  # mutate(kills_left = 30-(1:n())) %>%
  # view()
  ggplot(aes( round_diff)) +
  geom_density(size = 1, fill = 'grey')+
  theme_bw() +
  labs(x = "Kill Differential", y = element_blank(),
       title = "Offense Control Life Differentials")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        title = element_text(size = 18)) +
  geom_vline(xintercept = -1.06)


