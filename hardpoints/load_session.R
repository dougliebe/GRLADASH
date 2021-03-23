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
  select(GAME_ID,SERIES_ID,  TEAM_A_SCORE, TEAM_B_SCORE) %>%
  inner_join(series_q, by = "SERIES_ID") %>%
  select(GAME_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE, TEAM_A_ID, TEAM_B_ID)


group_colors = c(
  "ATL"= '#e43d29',
  "CHI" = '#9dd66b',
  "DAL" = '#011e41',
  "DAL" = '#025157',
  "FLA" = "#3bbfad",
  "LON" = '#000f9f',
  "LAG" = '#60269e',
  "LAT" = '#9dc73b',
  "MIN" = '#351f68',
  "NYC" = '#29304e',
  "PAR" = '#131e29',
  "SEA" = '#001231',
  "TOR" = '#773dbd',
  "LGA" = "#595959",
  "CHL" = "#1c1c1c"
)

mode_list <- as.list(c(1,4,3)) %>%
  set_names(c("Hardpoint", "Search & Destroy", "Control"))
map_list <- map_q %>%
  collect() %>%
  filter(MAP_ID %in% c(41,42,43,44)) %>%
  split(.$MAP_NAME) %>%
  map(~ .$MAP_ID)

clean_scores <- function(data) {
  data %>%
    select(MAP_ID, GAME_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S) %>%
    pivot_longer(c(-TIME_S,-GAME_ID, -MAP_ID), names_to = "team", values_to = "score") %>%
    group_by(GAME_ID) %>%
    filter(score >= 0,
           TIME_S > 4, 
           score < 250,
           # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
           !cumany(score == 249),
    ) %>%
    # left_join(hill_num, by = "")
    mutate(hill = pmax((TIME_S-5) %/% 60, 0)+1,
           seconds_into_hill = ((TIME_S-5) %% 60)+1,
           new_hill = ifelse(hill > lag(hill), hill, NA),
           total_hills = ifelse(MAP_ID %in% c(43,44, 46), 5, 4),
           hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
    group_by(GAME_ID, team, hill) %>%
    mutate(rotated = (hill > 1 & 
                        score - min(score) >= 5 & 
                        seconds_into_hill == 10),
           rotated = cumany(rotated)) %>%
    group_by(GAME_ID, team) %>%
    mutate(gain = score - lag(score), 
           running_gain = zoo::rollsumr(gain, k = 10, fill = 0)) %>%
    filter(score == cummean(score)) %>%
    group_by(GAME_ID, TIME_S) %>%
    mutate(broken = seconds_into_hill > 10 & running_gain < max(mean(running_gain),5) & rotated,
           broke = any(broken) & !broken)
}