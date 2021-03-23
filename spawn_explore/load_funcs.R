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
result_q <- game_q %>%
  select(GAME_ID,SERIES_ID,  TEAM_A_SCORE, TEAM_B_SCORE) %>%
  inner_join(series_q, by = "SERIES_ID") %>%
  select(GAME_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE, A_ID, B_ID)

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


library(reshape2) # For melt function
library(MASS)

getDensityDiff<- function(data1, data2) {
  # Calculate the common x and y range for data1 and data2
  xrng = range(c(data1$pos_x_opp, data2$pos_x_opp))
  yrng = range(c(data1$pos_y_opp, data2$pos_y_opp))
  
  # Calculate the 2d density estimate over the common range
  d1 = kde2d(data1$pos_x_opp, data1$pos_y_opp, lims=c(xrng, yrng), n=100)
  d2 = kde2d(data2$pos_x_opp, data2$pos_y_opp, lims=c(xrng, yrng), n=100)
  d1$z <- d1$z/max(d1$z)
  d2$z <- d2$z/max(d2$z)
  
  # Confirm that the grid points for each density estimate are identical
  identical(d1$x, d2$x) # TRUE
  identical(d1$y, d2$y) # TRUE
  
  # Calculate the difference between the 2d density estimates
  diff12 = d1 
  diff12$z = d1$z - d2$z
  
  ## Melt data into long format
  # First, add row and column names (x and y grid values) to the z-value matrix
  rownames(diff12$z) = diff12$x
  colnames(diff12$z) = diff12$y
  
  # Now melt it to long format
  diff12.m = melt(diff12$z, id.var=rownames(diff12))
  names(diff12.m) = c("x","y","z")
  diff12.m$z = ifelse(diff12.m$z > 0, diff12.m$z/max(diff12.m$z), -diff12.m$z/min(diff12.m$z))
  return(diff12.m)
}

getDensitySingle <- function(data) {
  # Calculate the common x and y range for data1 and data2
  xrng = range(data$pos_x_opp)
  yrng = range(data$pos_y_opp)
  
  # Calculate the 2d density estimate over the common range
  d1 = kde2d(data$pos_x_opp, data$pos_y_opp, lims=c(xrng, yrng), n=100)
  d1$z <- d1$z/max(d1$z)
  rownames(d1$z) = d1$x
  colnames(d1$z) = d1$y
  d1.m = melt(d1$z, id.var=rownames(d1))
  names(d1.m) = c("x","y","z")
  d1.m$z = d1.m$z/max(d1.m$z)
  return(d1.m)
}
