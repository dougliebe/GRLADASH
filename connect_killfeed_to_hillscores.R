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
spawn_q <- tbl(con, "SPAWN")

# get scores per second and 
# positions for players at those times
game_of_interest <- 310

game_score <- 
  game_q %>%
  filter(GAME_ID == !!game_of_interest)

game_score_by_sec <- 
  score_q %>%
  filter(GAME_ID == !!game_of_interest) %>%
  mutate(TIME_S = TIME_S - 10) %>%
  filter(TIME_S >= 5) %>%
  collect() %>%
  distinct(across(!contains("SCORE_ID")), .keep_all = T)

game_spawns <-
  spawn_q %>%
  filter(GAME_ID == !!game_of_interest) %>%