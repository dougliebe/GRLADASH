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
killfeed <- tbl(con, 'KILLFEED')

killfeed %>%
  left_join(game_q) %>%
  left_join(series_q) %>%
  filter(A_ID == 8| B_ID == 8) %>%
  collect() %>%
  count(DATE)
  # filter(GAME_ID == 266) %>%
  # collect() %>%
  # pivot_longer(c(PLAYER_A, PLAYER_B),
  #              names_to = 'type',
  #              values_to = 'PLAYER') %>%
  # group_by(PLAYER) %>%
  # count()
  # group_by(GAME_ID, PLAYER_A) %>%
  # count()

### Figure out games in question 

game_q %>%
  left_join(series_q) %>%
  filter(A_ID == 8 | B_ID == 8) %>%
  arrange(desc(GAME_ID)) %>%
  collect() ->
  target_games

killfeed %>%
  # left_join(game_q) %>%
  # filter(GAME_ID %in% !!target_games$GAME_ID) %>%
  collect() %>%
  pivot_longer(c(PLAYER_A, PLAYER_B),
               names_to = 'type',
               values_to = 'PLAYER') %>%
  mutate(PLAYER_TEAM = ifelse(type == "PLAYER_A", TEAM_A_ID, TEAM_B_ID)) %>%
  select(GAME_ID,TIME_S, PLAYER, PLAYER_TEAM, type) %>%
  group_by(GAME_ID, PLAYER_TEAM) %>%
  summarise(.groups = 'keep',
            kills = sum(type == "PLAYER_A"),
            deaths = sum(type == "PLAYER_B")) %>%
  mutate(kd = kills/deaths) %>%
  left_join(game_q %>%
              left_join(series_q) %>%
              left_join(map_q) %>%
              collect()) %>%
  mutate(TEAM_SCORE = ifelse(A_ID == PLAYER_TEAM, TEAM_A_SCORE, TEAM_B_SCORE)) %>%
  filter(MODE_ID == 1) %>%
  filter(DATE > '2020-01-01') %>%
  select(DATE,MAP_NAME, GAME_ID ,PLAYER_TEAM, kills, deaths, TEAM_SCORE) %>%
  mutate(ppk = TEAM_SCORE/kills) ->
  all_teams_ppk

all_teams_ppk %>%
  ggplot(aes(ppk))+
  geom_density(alpha = 0.3)+
  facet_wrap(~MAP_NAME,ncol = 1)+
  geom_vline(data = all_teams_ppk %>% filter(PLAYER_TEAM == 8) %>%
               group_by(MAP_NAME) %>%
               summarise(ppk = mean(ppk,na.rm = T)),
             aes(xintercept = ppk), color = "navy", lwd = 1) +
  geom_vline(data = all_teams_ppk %>% filter(PLAYER_TEAM == 6) %>%
               group_by(MAP_NAME) %>%
               summarise(ppk = mean(ppk,na.rm = T)),
             aes(xintercept = ppk), color = "#60269E", lwd = 1) +
  theme_bw()+
  labs(x = "Points Per Kill",
       title = "Points per Kill by Map",
       subtitle = "Distance between lines is advantage\nif both teams had 1.0 KDs")


all_teams_ppk %>%
  filter(PLAYER_TEAM == 6, DATE >= "2021-04-15") %>%
  group_by(DATE, MAP_NAME) %>%
  summarise(ppk = sum(TEAM_SCORE)/sum(kills),
            DATE = lubridate::as_datetime(DATE)) %>%
  ggplot(aes(DATE, ppk, color = MAP_NAME))+
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~MAP_NAME, scales = 'free') +
  theme(legend.position = 'none')+
  labs(x = element_blank(),
       y = "PPK",
       title = "We are getting more points per kill on every map!")
