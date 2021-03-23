library(odbc)
library(DBI)
library(tidyverse)
select <- dplyr::select

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
spawn_q <- tbl(con, "SPAWN")
# result_q <- game_q %>%
#   select(GAME_ID,SERIES_ID,  TEAM_A_SCORE, TEAM_B_SCORE) %>%
#   inner_join(series_q, by = "SERIES_ID") %>%
#   select(GAME_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE, TEAM_A_ID, TEAM_B_ID)

hill_num <- tibble(
  MAP_NAME = c("Moscow", "Cartel", "Crossroads", "Garrison", "Checkmate"),
  HILLS = c(4,4,4,5,5)
)
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

addContext <- function(data) {
  data %>%
    left_join(game_q %>% 
                filter(MAP_ID != 46) %>%
                mutate(TEAM_A_WIN = TEAM_A_SCORE > TEAM_B_SCORE) %>%
                select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
    mutate(TIME_S = TIME_S - 7) %>%
    filter(TIME_S >= 5) %>%
    collect() %>%
    mutate(
      hill = (TIME_S-5) %/% 60+1,
       seconds_into_hill = ((TIME_S-5) %% 60)+1,
       total_hills = ifelse(MAP_ID %in% c(43,44, 46,49), 5, 4),
       hill_no = hill-(((hill-1)%/%total_hills)*total_hills)
      ) 
}
