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
result_q <- game_q %>%
  select(GAME_ID,MAP_ID, MODE_ID, SERIES_ID,  TEAM_A_SCORE, TEAM_B_SCORE) %>%
  inner_join(series_q, by = "SERIES_ID") %>%
  select(GAME_ID,MAP_ID, MODE_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE, A_ID, B_ID, DATE)

hill_num <- tibble(
  MAP_NAME = c("Moscow", "Cartel", "Crossroads", "Garrison", "Checkmate", "Cartel"),
  HILLS = c(4,4,4,5,5, 5)
)
group_colors = c(
  "ATL"= '#e43d29',
  "CHI" = '#9dd66b',
  "DAL" = '#011e41',
  "DAL" = '#025157',
  "FLA" = "#3bbfad",
  "LON" = '#000f9f',
  "LAG" = '#60269e',
  "LAT" = '#EF3232',
  "MIN" = '#00B2E2',
  "NYC" = '#FFF000',
  "PAR" = '#131e29',
  "SEA" = '#00667D',
  "TOR" = '#773dbd',
  "LGA" = "#595959",
  "CHL" = "#1c1c1c"
   
)
# ref_list <- ref_q %>%
#     collect() %>%
#     split(.$REFERENCE_VALUE) %>%
#     map(~ .$REFERENCE_ID)
mode_list <- as.list(c(1,4,3)) %>%
  set_names(c("Hardpoint", "Search & Destroy", "Control"))
map_list <- map_q %>%
  collect() %>%
  filter(MAP_ID >= 41) %>%
  split(.$MAP_NAME) %>%
  map(~ .$MAP_ID)

clean_scores <- function(data) {
  data %>%
    select(MAP_ID, GAME_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S) %>%
    pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE), names_to = "team", values_to = "score") %>%
    # mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID)) %>%
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
           total_hills = ifelse(MAP_ID %in% c(43,44, 46, 49), 5, 4),
           hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
    group_by(GAME_ID, team, hill) %>%
    mutate(rotated = (hill > 1 & 
                        score - min(score) >= 5 & 
                        seconds_into_hill == 10),
           rotated = cumany(rotated)) %>%
    group_by(GAME_ID, team) %>%
    mutate(gain = score - lag(score), 
           running_gain = zoo::rollsumr(gain, k = 10, fill = 0),
           running_120 = zoo::rollsumr(gain, k = 120, fill = 0)) %>%
    filter(score == cummax(score)) %>%
    group_by(GAME_ID, TIME_S) %>%
    mutate(broken = running_gain == 0 & rotated & sum(running_gain) > 0) %>%
    group_by(GAME_ID, team, hill) %>%
    mutate(broken = cumany(broken),
           hold = !cumany(broken) & rotated) %>%
    group_by(GAME_ID, hill) %>%
    mutate(broke = cumany(broken) & !broken)
}
getRecords <- function(data) {
  data %>%
    select(GAME_ID, TEAM_ABV_A, TEAM_ABV_B, MODE_NAME, MAP_NAME, TEAM_A_SCORE, TEAM_B_SCORE) %>%
    mutate(TEAM_A_WIN = ifelse(TEAM_A_SCORE > TEAM_B_SCORE, T, F)) %>%
    pivot_longer(cols = c(TEAM_A_SCORE, TEAM_B_SCORE)) %>%
    mutate(TEAM_ABV = ifelse(name == "TEAM_A_SCORE", TEAM_ABV_A, TEAM_ABV_B),
           win = ifelse((name=="TEAM_A_SCORE" & TEAM_A_WIN) | 
                          (name=="TEAM_B_SCORE" & !TEAM_A_WIN), T, F),
           # win = ifelse(name == "TEAM_A_SCORE", TEAM_A_WIN, !TEAM_A_WIN),
           side = ifelse(name == "TEAM_A_SCORE", "blue - NATO", "red - WARSAW")) %>%
    group_by(GAME_ID) %>%
    mutate(opp_score = sum(value)-value) %>%
    select(TEAM_ABV, score = value, opp_score, MODE_NAME, MAP_NAME, win, side)
}
