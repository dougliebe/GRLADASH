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
  filter(MODE_ID == 1) %>%
  collect() %>%
  pivot_longer(c(PLAYER_A, PLAYER_B),
               names_to = "type",
               values_to = "player") ->
  pbp_long

pbp_long %>%
  mutate(PLAYER_TEAM = ifelse(type == "PLAYER_A", TEAM_A_ID, TEAM_B_ID)) %>%
  select(GAME_ID, TIME_S,PLAYER_TEAM, player, type) %>%
  mutate(type = recode(type, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  arrange(GAME_ID, player, TIME_S) %>%
  group_by(GAME_ID, player) %>%
  mutate(next_eng = ifelse(type == "KILL", lead(TIME_S)-TIME_S, NA),
         next_eng_win = ifelse(type == "KILL", lead(type) == "KILL", NA),
         player = tolower(player)) %>%
  filter(!is.na(next_eng)) ->
  next_eng

# pbp_long %>%
#   arrange(GAME_ID, TIME_S, player) %>%
#   group_by(GAME_ID, player) %>%
#   mutate(tsla =  pmin(15, TIME_S - lag(TIME_S, default = 0)),
#          life = cumsum(lag(type, default = "PLAYER_B") == "PLAYER_B")) %>%
#   group_by(life, .add = T) %>%
#   mutate(streak = 1:n()-1,
#          pdk = tsla < 5000 & streak == 0,
#          last_kill_id = lag(KILL_ID),
#          last_kill_s = TIME_S - lag(TIME_S, default = NA),
#          in_trade_eng = streak >= 1 & last_kill_s < 5 & !pdk,
#          win = type == "PLAYER_A") %>%
#   ungroup() %>%
#   select(GAME_ID, KILL_ID, type, last_kill_id,last_kill_s, in_trade_eng, win) ->
#   trade_kills
# 
# killfeed_q %>%
#   left_join(game_q) %>%
#   filter(MODE_ID == 3) %>%
#   collect() %>%
#   group_by(GAME_ID) %>%
#   left_join(trade_kills %>%
#               arrange(GAME_ID, KILL_ID) %>%
#               # filter(in_trade_eng) %>%
#               select(-KILL_ID),
#             by = c("GAME_ID", 'KILL_ID' = 'last_kill_id')) %>%
#   replace_na(list('in_trade_eng' = FALSE)) ->
#   was_traded

# get round data
get_HP_data(50) ->
  hp_games

# ctl_games %>% 
#   filter(GAME_ID %in% next_eng$GAME_ID) %>%
#   bind_rows(next_eng) %>%
#   group_by(GAME_ID) %>%
#   arrange(GAME_ID, TIME_S) %>%
#   fill(offense, MAP_ID, MODE_ID, round,.direction = 'up') -> 
#   # pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
#   ctl_combined

hp_games %>% 
  filter(GAME_ID %in% next_eng$GAME_ID) %>%
  left_join(next_eng, by = c("GAME_ID", "TIME_S")) -> 
  # pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  hp_combined

hp_combined %>%
  filter(PLAYER_TEAM == 6,next_eng < 30) %>%
  filter(player %in% c("vivid",'apathy','assault','silly') ) ->
  filtered_hp

tibble(
  x = density(filtered_hp$next_eng,from = 0, to = 20)$x,
  y = density(filtered_hp %>%
                 filter(next_eng_win) %>%
                 pull(next_eng),from = 0, to = 20)$y/
    density(filtered_hp %>%
              filter(!next_eng_win) %>%
              pull(next_eng),from = 0, to = 20)$y
) %>%
  ggplot(aes(x,y))+geom_line()+
  theme_bw() +
  labs(x = "Time to next eng", y = "KD", title = "KD by time after you get a kill", subtitle = "wait >5s for KD > 1")

hp_combined %>%
  # filter(PLAYER_TEAM == 6,next_eng < 30) %>%
  # mutate(player = ifelse(endsWith(player, "cheen"), "cheen", player)) %>%
  filter(player %in% c("vivid",'apathy','assault','silly') ) %>%
  filter(next_eng < 20) %>%
  select(GAME_ID, MAP_ID, PLAYER_TEAM, player, next_eng, next_eng_win ) %>%
  ggplot()+
  geom_line(aes(next_eng, color = next_eng_win), stat = 'density') +
  facet_wrap(~player) +
  geom_line(data = hp_combined %>%
              filter(next_eng < 20) %>%
              filter(!player %in% c("vivid",'apathy','assault','silly') ) %>%
              select( next_eng_win, next_eng),
            aes(next_eng), color = 'black', lwd = 1, stat = 'density') +
  geom_vline(xintercept = 0) +
  theme_bw() + 
  labs(x = "Time to next engagement",
       title = '')


## graph by date
hp_combined %>%
  left_join(game_q %>% select(GAME_ID, SERIES_ID) %>%
              left_join(series_q) %>% collect()) %>%
  # filter(PLAYER_TEAM == 6,next_eng < 30) %>%
  # mutate(player = ifelse(endsWith(player, "cheen"), "cheen", player)) %>%
  filter(player %in% c("vivid",'apathy','assault','silly') ) %>%
  filter(next_eng < 30) %>%
  select(DATE, GAME_ID, MAP_ID, PLAYER_TEAM, player, next_eng, next_eng_win ) %>%
  group_by(DATE, PLAYER_TEAM, player) %>%
  summarise(next_eng = mean(next_eng),
            next_eng_win = mean(next_eng_win)) %>%
  ggplot(aes(lubridate::as_datetime(DATE), next_eng_win, color = player))+
  geom_line()+
  geom_smooth(method = 'lm') +
  # geom_line(aes(next_eng,color = player), stat = 'density') +
  facet_wrap(~player, scales = 'free')+
  scale_y_continuous(label = scales::percent) +
  labs(x = element_blank(),
       y = "Getting Your Own Trade, %",
       title = "We are getting 2 more often!") +
  theme(legend.position = 'none')
  geom_line(data = hp_combined %>%
              filter(next_eng < 30) %>%
              select( next_eng_win, next_eng),
            aes(next_eng), color = 'black', lwd = 1, stat = 'density') +
  theme_bw() + 
  labs(x = "Time to next engagement")


ctl_combined %>%
  filter(TEAM_ID == 6, !is.na(offense), next_eng < 30) %>%
  filter(player %in% c("vivid",'apathy','assault','silly') ) %>%
    select(GAME_ID, MAP_ID, team, offense, player, next_eng, next_eng_win ) %>%
  ggplot()+
  geom_line(aes(next_eng, color = next_eng_win), stat = 'density') +
  facet_wrap(~player) +
  geom_line(data = ctl_combined %>%
                 filter(!is.na(offense), next_eng < 30) %>%
                 select( next_eng_win, next_eng),
               aes(next_eng), color = 'black', lwd = 1, stat = 'density') +
  theme_bw() + 
  labs(x = "Time to next engagement")
  
ctl_combined %>%
  group_by(GAME_ID, team, round) %>%
  mutate(lives = 30 - cumsum(type == "PLAYER_A")) %>%
  ggplot(aes(TIME_S, lives, color = team))+geom_line()+facet_wrap(~GAME_ID)+
  geom_line(aes(TIME_S, win*30))

## show fast engs for cheen
hp_combined %>%
  filter(PLAYER_TEAM == 14,next_eng < 5) %>%
  mutate(player = ifelse(endsWith(player, "cheen"), "cheen", player)) %>%
  filter(player == "cheen") %>%
  group_by(MAP_ID) %>%
  count() %>% arrange(desc(n))

## show league average ttne
hp_combined %>%
  # filter(PLAYER_TEAM == 6,next_eng < 30) %>%
  # mutate(player = ifelse(endsWith(player, "cheen"), "cheen", player)) %>%
  filter(player %in% c("vivid",'apathy','assault','silly') ) %>%
  filter(next_eng < 20) %>%
  select(GAME_ID, MAP_ID, PLAYER_TEAM, player, next_eng, next_eng_win ) %>%
  ggplot()+
  geom_line(aes(next_eng,color = player), stat = 'density', size = 1) +
  geom_line(data = hp_combined %>%
              filter(next_eng < 20) %>%
              filter(!player %in% c("vivid",'apathy','assault','silly') ) %>%
              select( next_eng_win, next_eng),
            aes(next_eng), color = 'black', lwd = 2, stat = 'density') +
  theme_bw() + 
  geom_vline(xintercept = 0, color = 'red')+
  annotate(geom="text", x=0.1, y=0, label="Time of first kill",
           color="red", hjust = 0)+
  geom_vline(xintercept = 5, color = 'black')+
  annotate(geom="text", x=5.1, y=0, label="KD > 1",
           color="black", hjust = 0)+
  labs(x = "Time to next engagement",
       title = "How long after you get a kill will you get in another engagement?",
       subtitle = "black = league average")
