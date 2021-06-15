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


## prepare the data
get_HP_data <- function(days_) {
  current_day = Sys.Date()
  hardpoint_games <- 
    game_q %>%
    filter(MODE_ID == 1) %>%
    left_join(series_q,
              by = "SERIES_ID") %>%
    collect() %>%
    filter(lubridate::as_date(DATE )> current_day - days_)
  
  hardpoint_scores <- 
    score_q %>%
    filter(GAME_ID %in% !!hardpoint_games$GAME_ID) %>%
    collect() %>%
    inner_join(hardpoint_games %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE),
               byb = "GAME_ID")
  
  hardpoint_scores %>%
    select(MAP_ID, GAME_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S, A_ID, B_ID) %>%
    pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
    mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID)) %>%
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
           total_hills = ifelse(MAP_ID %in% c(43,44, 46, 49, 51), 5, 4),
           hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
    group_by(GAME_ID, team, hill) %>%
    mutate(rotated = (hill > 1 & 
                        score - min(score) >= 5 & 
                        seconds_into_hill == 10),
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
    pp_data_hp
  return(pp_data_hp)
}


## ctl
get_CTL_games <- function(days_) {
  current_day <- Sys.Date()
  control_games <- 
    game_q %>%
    filter(MODE_ID == 3) %>%
    left_join(series_q,
              by = "SERIES_ID") %>%
    collect() %>%
    filter(lubridate::as_date(DATE )> current_day - days_)
  
  score_q %>%
    filter(TEAM_A_SCORE != 0 | TEAM_B_SCORE != 0, 
           GAME_ID %in% !!control_games$GAME_ID) %>%
    collect() %>%
    left_join(control_games %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
    group_by(GAME_ID) %>%
    mutate(round = 1:n()) %>%
    pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
                 names_to = 'team',
                 values_to = 'score') %>%
    mutate(offense = ifelse((team=="TEAM_A_SCORE" & round%%2 == 1 ) |
                              (team=="TEAM_B_SCORE" & round%%2 == 0 & round != 0), T, F),
           TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID))  %>%
    # filter(TEAM_ID == team_) %>%
    group_by(GAME_ID, TEAM_ID, MAP_ID) %>%
    mutate(gain = score - lag(score, default = 0),
           win = (gain>0)*1) %>%
    group_by(GAME_ID, MAP_ID, round) %>%
    mutate(opp_score = sum(gain)-gain)
}


