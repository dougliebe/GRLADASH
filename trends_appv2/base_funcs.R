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
result_q <- game_q %>%
  select(GAME_ID,MAP_ID, MODE_ID, SERIES_ID,  TEAM_A_SCORE, TEAM_B_SCORE) %>%
  inner_join(series_q, by = "SERIES_ID") %>%
  select(GAME_ID,MAP_ID, MODE_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE,
         A_ID, B_ID, DATE)

group_colors = c(
  "ATL"= '#e43d29',
  "CHI" = '#9dd66b',
  "DAL" = '#011e41',
  "DAL" = '#025157',
  "FLA" = "#3bbfad",
  "LON" = '#000f9f',
  "LAG" = '#60269e',
  "LAT" = '#EF3232',
  "MIN" = '#351f68',
  "NYC" = '#29304e',
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
team_list <- team_q %>%
  collect() %>%
  split(.$TEAM_ABV) %>%
  map(~ .$TEAM_ID)

getFilteredGames <- function(team_, date_, mode_) {
  series_ids <- series_q %>%
    filter(A_ID == !!team_ | B_ID == !!team_,
           DATE >= !!date_[1],
           DATE <= !!date_[2]) %>%
    collect()
  #filter out games in those series only
  game_q %>%
    left_join(series_q, by = "SERIES_ID") %>%
    filter(SERIES_ID %in% !!series_ids$SERIES_ID, MODE_ID %in% !!mode_) %>%
    collect()
}

getRecords <- function(games_, team_) {
  games_ %>% 
    mutate(TEAM_A_WIN = TEAM_A_SCORE > TEAM_B_SCORE) %>%
    pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
                 names_to = 'team',
                 values_to = 'score') %>%
    mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", A_ID, B_ID),
           win = (team == "TEAM_A_SCORE" & TEAM_A_WIN) |
             (team == "TEAM_B_SCORE" & !TEAM_A_WIN),
           side = ifelse(team == "TEAM_A_SCORE", 'BLUE', "RED")) %>%
    filter(TEAM_ID == !!team_)
    
}
