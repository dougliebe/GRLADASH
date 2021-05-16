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



# killfeed_q %>%
#   filter(GAME_ID == 767) %>%
#   collect() %>%
#   pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
#   group_by(value, name) %>%
#   count()

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
  pivot_longer(c(PLAYER_A, PLAYER_B)) %>%
  mutate(TEAM_ID = ifelse(name == "PLAYER_A", TEAM_A_ID, TEAM_B_ID)) %>%
  group_by(GAME_ID, value, name, TEAM_ID, traded=in_trade_eng) %>%
  count() %>%
  left_join(was_traded %>% group_by(GAME_ID) %>% summarise(duration = max(TIME_S)-min(TIME_S)),
            by = "GAME_ID") %>%
  ungroup() %>%
  mutate(name = recode(name, PLAYER_A = "KILL", PLAYER_B = "DEATH")) %>%
  pivot_wider( names_from = c(name, traded), values_from = n) %>%
  mutate(across(starts_with(c("DEATH", "KILL")), 
                            ~replace_na(.x, replace = 0))) %>%
  mutate(untraded_KILL = KILL_FALSE/(KILL_FALSE+KILL_TRUE),
         traded_deaths = DEATH_TRUE/(DEATH_TRUE+DEATH_FALSE),
         kd = (KILL_FALSE+KILL_TRUE)/(DEATH_TRUE+DEATH_FALSE),
         epm = (KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/duration*60,
         pm = KILL_FALSE + 0.2*KILL_TRUE - 0.2*DEATH_TRUE - DEATH_FALSE) ->
  all_games_trade_box 

# get relevant games form series
series_q %>%
  left_join(game_q, by = "SERIES_ID") %>%
  # filter(MAP_ID != 49) %>%
  filter(MODE_ID %in% c(1,3)) %>%
  # filter(DATE >= "2021-05-03") %>%
  filter(SERIES_ID %in% c(244,245)) %>%
  # filter(A_ID == 8 | B_ID == 8) %>%
  select(GAME_ID, MAP_ID, MODE_ID, TEAM_A_SCORE, TEAM_B_SCORE) %>%
  collect() ->
  games

all_games_trade_box %>%
  inner_join(games, by = "GAME_ID") %>%
  mutate(rounds = TEAM_A_SCORE + TEAM_B_SCORE) %>%
  # mutate(rounds_ctl = case_when(
  #   MODE_ID == 3 ~ TEAM_A_SCORE + TEAM_B_SCORE,
  #   TRUE ~ 1
  # )) %>%
  # select( -duration) %>%
  arrange(desc(pm)) ->
  out

out %>%
  mutate(value = tolower(value)) %>%
  mutate(value = ifelse(endsWith(value, 'cheen'), 'cheen',value)) %>%
   mutate(value = recode(value, "6" = "c6",
                        "m|cheen" = "mjcheen",
                        "m]cheen" = "mjcheen",
                        "[«" = "c6",
                        "asim/" = "asim",
                        "asim*y" = "asim",
                        "asimy" = "asim",
                        "yivid"='vivid'),
         team = case_when(value %in% c("vivid",'cheen',"assault", "silly", "apathy") ~ "LAG",
                          TRUE ~ "THEM")) %>%
  # filter(TEAM_ID == 14) %>%
  # filter(value == "dylan") %>%
  # filter(value %in% c("cheen", 'nero','mental','exceed')) %>%
  filter(value %in% c("cheen","assault", "silly", "apathy")) %>%
  group_by( value) %>%
  summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
          traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
          kills = sum(KILL_FALSE+KILL_TRUE),
          deaths = sum(DEATH_TRUE+DEATH_FALSE),
          kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
          pmpm = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration)*60,
          epm = sum(KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/sum(duration)*60,
          pm10 = sum(pm)/sum(duration)*600,
          games = length(unique(GAME_ID))
          ) %>%
          mutate(corr_epm = epm + (-0.3*pmpm)
         )%>%
  select(-pmpm, -epm)

### plot utk and td for each map player

annotations <- tibble(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Bad","More TDs"
                   ,"More UTKs","Good"),
  hjustvar = c(0,0,1,1) ,
  vjustvar = c(0,1,0,1)) #<- adjust

library(ggtext)
library(ggrepel)
out %>%
  mutate(value = tolower(value)) %>%
  mutate(value = ifelse(endsWith(value, 'cheen'), 'cheen',value)) %>%
  mutate(value = recode(value, "6" = "c6",
                        "m|cheen" = "mjcheen",
                        "m]cheen" = "mjcheen",
                        "[«" = "c6",
                        "asim/" = "asim",
                        "asim*y" = "asim",
                        "asimy" = "asim",
                        "yivid"='vivid'),
         team = case_when(value %in% c("vivid","assault", "silly", "apathy") ~ "LAG",
                          TRUE ~ "THEM")) %>%
  # filter(TEAM_ID == 14) %>%
  # filter(value %in% c("cheen", 'nero','mental','exceed')) %>%
  filter(value %in% c("cheen","assault", "silly", "apathy")) %>%
  group_by( MAP_ID, MODE_ID, value) %>%
  summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
            traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            kills = sum(KILL_FALSE+KILL_TRUE),
            deaths = sum(DEATH_TRUE+DEATH_FALSE),
            kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            pmpm = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration)*60,
            epm = sum(KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/sum(duration)*60,
            pm10 = sum(pm)/sum(duration)*600,
            games = length(unique(GAME_ID))
  ) %>%
  left_join(map_q %>% collect()) %>%
  left_join(mode_q %>% collect()) %>%
  mutate(corr_epm = epm + (-0.2*pmpm),
         map_mode = paste0(MODE_ALIAS,"\n", MAP_NAME),
         trade_death_pm = traded_deaths * corr_epm,
         untrade_kill_pm = untraded_KILL * corr_epm
  ) %>%
  arrange( desc(pm10)) %>%
  ggplot() +
  geom_abline(slope = -1, intercept = 1, lwd = 1, alpha = 0.5)+
  geom_abline(slope = -1, intercept = 1.1, lty = 2, alpha = 0.3)+
  geom_abline(slope = -1, intercept =0.9, lty = 2, alpha = 0.3) + 
  geom_label(data=annotations,
             aes(x=xpos,
                 y=ypos,
                 hjust=hjustvar,
                 vjust=vjustvar,
                 label=annotateText),
             alpha = 0.8,
             label.size = 0, size = 5)+
  geom_point(aes(untraded_KILL, traded_deaths,
                 group = MAP_NAME,
                 color = MODE_NAME)) +
  geom_label_repel(aes(untraded_KILL, traded_deaths,
                       label = MAP_NAME, 
                       color = MODE_NAME))+
  facet_wrap(~toupper(value))+
  theme_bw()+
  labs(x = "Untraded Kills, %", y = "Traded Deaths, %") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18))
  

## TEAM
out %>%
  mutate(value = endsWith(value, "cheen"), "cheen", tolower(value)) %>%
  # count(value)
  mutate(team = case_when(value %in% c("mjcheen","assault", "silly", "apathy") ~ "LAG",
                          TRUE ~ "THEM")) %>%
  # mutate(value = recode(value, "6" = "C6",
  #                       "M|Cheen" = "MJCheen",
  #                       "M]Cheen" = "MJCheen",)) %>%
  group_by(GAME_ID,MAP_ID, MODE_ID, TEAM_ID) %>%
  summarise(untraded_KILL = sum(KILL_FALSE),
            traded_deaths = sum(DEATH_TRUE),
            kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            deaths = sum(DEATH_FALSE + DEATH_TRUE),
            traded_deaths_pct = sum(DEATH_TRUE)/sum(DEATH_TRUE + DEATH_FALSE),
            traded_deaths_pct = sum(DEATH_TRUE)/sum(DEATH_TRUE + DEATH_FALSE),
            untraded_kills_pct = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
            # actual_pm_10 = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/mean(duration)*600,
            pm10 = sum(pm)/mean(duration)*600,
            games = length(unique(GAME_ID))) %>%
  group_by(TEAM_ID) %>%
  summarise(games = n(),
            pm10 = mean(pm10),
            tds = sum(traded_deaths)/sum(deaths)) %>%
  arrange(desc(pm10)) %>%
  left_join(team_q %>% collect()) %>%
  filter(TEAM_NAME == "Guerrillas")
filter(team == "LAG")

out %>%
  left_join(game_q %>%
              left_join(series_q) %>% 
              collect() %>%
              select(GAME_ID, DATE),
            by = "GAME_ID") %>%
  mutate(value = tolower(value),
         team = case_when(value %in% c("vivid","assault", "silly", "apathy") ~ "LAG",
                          TRUE ~ "THEM")) %>%
  mutate(value = recode(value, "6" = "c6",
                        "m|cheen" = "mjcheen",
                        "m]cheen" = "mjcheen",
                        "[«" = "c6")) %>%
  filter(team == "LAG") %>%
  group_by(DATE,value) %>%
  summarise( deaths_per_rd = sum(DEATH_TRUE+DEATH_FALSE)/sum(rounds),
    untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
    traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
    kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
    # actual_pm_10 = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration)*600,
    pm10 = sum(pm)/sum(duration)*600,
    games = length(unique(GAME_ID))) %>%
  ggplot(aes(lubridate::as_date(DATE), traded_deaths, color = value))+
  # geom_hline(yintercept = 0.5, lty = 2)+
  geom_line()+geom_point()+
  # facet_wrap(~value)+
  geom_smooth(method = 'lm', se = F)+
  # theme_bw()+
  labs(x = "Date", y = "adj. +/-")
# +
  # theme(legend.position = 'none')



out %>%
  left_join(game_q %>%
              left_join(series_q) %>% 
              collect() %>%
              select(GAME_ID, DATE),
            by = "GAME_ID") %>%
  mutate(value = tolower(value),
         team = case_when(value %in% c("vivid","assault", "silly", "apathy") ~ "LAG",
                          TRUE ~ "THEM")) %>%
  mutate(value = recode(value, "6" = "c6",
                        "m|cheen" = "mjcheen",
                        "m]cheen" = "mjcheen",
                        "[«" = "c6")) %>%
  filter(team == "LAG") %>%
  group_by(DATE,team,value) %>%
  summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
    traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
    kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
    # actual_pm_10 = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration)*600,
    pm10 = sum(pm)/sum(duration)*600,
    games = length(unique(GAME_ID))) %>%
  ggplot(aes(untraded_KILL, traded_deaths))+geom_point()+
  geom_abline(slope = -1, intercept = 1, lwd = 1, alpha = 0.5)+
  geom_abline(slope = -1, intercept = 1.1, lty = 2, alpha = 0.3)+
  geom_abline(slope = -1, intercept =0.9, lty = 2, alpha = 0.3)+
  geom_label_repel(data = out %>%
                       left_join(game_q %>%
                                   left_join(series_q) %>% 
                                   collect() %>%
                                   select(GAME_ID, DATE),
                                 by = "GAME_ID") %>%
                       mutate(value = tolower(value),
                              team = case_when(value %in% c("vivid","assault", "silly", "apathy") ~ "LAG",
                                               TRUE ~ "THEM")) %>%
                       mutate(value = recode(value, "6" = "c6",
                                             "m|cheen" = "mjcheen",
                                             "m]cheen" = "mjcheen",
                                             "[«" = "c6")) %>%
                       filter(team == "LAG") %>%
                       group_by(DATE,team,value) %>%
                       summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
                         traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
                         kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
                         # actual_pm_10 = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration)*600,
                         pm10 = sum(pm)/sum(duration)*600,
                         games = length(unique(GAME_ID))) %>%
                       group_by(value) %>%
                       slice(n()), aes(untraded_KILL, traded_deaths, label = value))+
  facet_wrap(~value, scales = 'free')+
  geom_label(data=annotations,
             aes(x=xpos,
                 y=ypos,
                 hjust=hjustvar,
                 vjust=vjustvar,
                 label=annotateText),
             alpha = 0.8,
             label.size = 0, size = 3)+
  theme_bw()


### Team pace by game
out %>%
  group_by( MAP_ID, MODE_ID, GAME_ID, TEAM_ID) %>%
  summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
            traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            kills = sum(KILL_FALSE+KILL_TRUE),
            deaths = sum(DEATH_TRUE+DEATH_FALSE),
            kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            pmpm = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration/4)*60,
            epm = sum(KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/sum(duration/4)*60,
            pm10 = sum(pm)/sum(duration/4)*600,
            games = length(unique(GAME_ID))
  ) %>%
  left_join(map_q %>% collect()) %>%
  left_join(mode_q %>% collect()) %>%
  mutate(corr_epm = epm + (-0.2*pmpm),
         map_mode = paste0(MODE_ALIAS,"\n", MAP_NAME),
         trade_death_pm = traded_deaths * corr_epm,
         untrade_kill_pm = untraded_KILL * corr_epm
  ) %>%
  left_join(game_q %>% select(GAME_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE) %>%
              collect()) %>%
  left_join(series_q %>% collect) %>%
  mutate(win = case_when(A_ID == TEAM_ID ~ (TEAM_A_SCORE > TEAM_B_SCORE),
                         TRUE ~ (TEAM_B_SCORE > TEAM_A_SCORE))) %>%
  filter(TEAM_ID == 6) %>%
  group_by(MAP_NAME, MODE_NAME) %>%
  mutate(pace_percentiles = percent_rank(corr_epm),
         pace_brks = cut(pace_percentiles, seq(0,1,0.2))) %>%
  filter(MAP_NAME == "Checkmate",MODE_ID == 3, corr_epm < 16)
  ggplot(aes(epm, kd))+geom_point()
  # filter(MAP_ID == 41, MODE_ID == 1, pace_brks == "(0.9,1]")
  group_by(MAP_NAME, MODE_NAME, pace_brks) %>%
  summarise(win_pct = mean(win),
            wins = sum(win),
            n = n()) %>%
  filter(MAP_NAME != "Express") %>%
  # filter(n > 5) %>%
    ggplot(aes(as.numeric(pace_brks)/5, win_pct, color = MAP_NAME))+
  geom_line(size = 1)+
  facet_wrap(~MODE_NAME)+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Pace Percentile\n0-slowest 1-fastest",
       y = "Map Win%")


out %>%
  group_by( MAP_ID, MODE_ID, GAME_ID, TEAM_ID) %>%
  summarise(untraded_KILL = sum(KILL_FALSE)/sum(KILL_FALSE+KILL_TRUE),
            traded_deaths = sum(DEATH_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            kills = sum(KILL_FALSE+KILL_TRUE),
            deaths = sum(DEATH_TRUE+DEATH_FALSE),
            kd = sum(KILL_FALSE+KILL_TRUE)/sum(DEATH_TRUE+DEATH_FALSE),
            pmpm = sum(KILL_FALSE+KILL_TRUE-DEATH_TRUE-DEATH_FALSE)/sum(duration/4)*60,
            epm = sum(KILL_FALSE+KILL_TRUE+DEATH_TRUE+DEATH_FALSE)/sum(duration/4)*60,
            pm10 = sum(pm)/sum(duration/4)*600,
            games = length(unique(GAME_ID))
  ) %>%
  left_join(map_q %>% collect()) %>%
  left_join(mode_q %>% collect()) %>%
  mutate(corr_epm = epm + (-0.2*pmpm),
         map_mode = paste0(MODE_ALIAS,"\n", MAP_NAME),
         trade_death_pm = traded_deaths * corr_epm,
         untrade_kill_pm = untraded_KILL * corr_epm
  ) %>%
  left_join(game_q %>% select(GAME_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE) %>%
              collect()) %>%
  left_join(series_q %>% collect) %>%
  mutate(win = case_when(A_ID == TEAM_ID ~ (TEAM_A_SCORE > TEAM_B_SCORE),
                         TRUE ~ (TEAM_B_SCORE > TEAM_A_SCORE))) %>%
  filter(TEAM_ID == 6) %>%
  group_by(MAP_NAME, MODE_NAME) %>%
  mutate(pace_percentiles = percent_rank(corr_epm),
         pace_brks = cut(pace_percentiles, seq(0,1,0.2))) %>%
  filter(SERIES_ID %in% c(234,235), MODE_ID == 1) %>%
  mutate(score = case_when(
    A_ID == TEAM_ID ~ paste0(TEAM_A_SCORE, "-",TEAM_B_SCORE),
    TRUE ~ paste0(TEAM_B_SCORE, "-", TEAM_A_SCORE)
  )) %>%
  select(GAME_ID, MAP_NAME, MODE_NAME, corr_epm, pace_percentiles, score) %>%
  arrange(pace_percentiles)
