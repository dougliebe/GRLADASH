library(tidyverse)

# load the different data sources

####### Boxscores ##############
path <- "C:/Users/liebe/OneDrive/Documents/GameTracker/output"

filenames <- list.files(paste0(path, "/results"), full.names=TRUE)[200]
results <- data.frame()

for (i in 1:(length(filenames))) {
  data <- read.table(filenames[i],sep = ",")
  # data <- subset(data, data$mode == 'Search & Destroy') # sort by mode
  results <- rbind(results, data)
}
colnames(results) <- c("match.id","team",'opp','score', 'opp.score','side')

# read in the csv
filenames <- list.files(path = paste0(path, "/players"),pattern=".txt", full.names=TRUE)[200]
players <- data.frame()
replacing <- data.frame(V2 = c("ACHES", "NATO", "AQUA", "LACEFIELD","DECEMATE"), 
                        name = c("BLAZT", "VIVID", "AQUA", "NATO","DECEMATE"))
for (i in 1:(length(filenames))) {
  data <- read.table(filenames[i],sep = ",")
  data <- data %>%
    inner_join(replacing, by = "V2") %>%
    select(V1, name, V3, V4)
  # data <- subset(data, data$mode == 'Search & Destroy') # sort by mode
  players <- rbind(players, data)
}
colnames(players) <- c("match.id","player",'kills','deaths')
players$team = "LAG Guerrillas"

# read in the csv
filenames <- list.files(path = paste0(path,"/general"),pattern=".txt", full.names=TRUE)[200]
general <- data.frame()

for (i in 1:(length(filenames))) {
  data <- read.table(filenames[i],sep = ",")
  # data <- subset(data, data$mode == 'Search & Destroy') # sort by mode
  general <- rbind(general, data)
}
colnames(general) <- c("match.id",'date',"duration","map","mode","notes")
general$date <- as.Date(general$date, "%Y-%m-%d")


####### HP score OCR
here::here('sample_data')

killfeed <- read_csv(here::here('sample_data','kills_1603845185641.csv'),
         col_types = cols(),
         col_names = c('killer', 'victim', 'time_f'))

score_feed <- read_csv(here::here('sample_data','scores_1606090361253.csv'),
         col_types = cols(),
         col_names = c('score_1', 'score_2', 'time_f')) %>%
  mutate(GAME_ID = 1)

score_feed %>%
  mutate(TIME_S = time_f) %>%
  select(GAME_ID, TIME_S, TEAM_A_SCORE = score_1, TEAM_B_SCORE = score_2) %>%
  clean_scores() %>%
  group_by(GAME_ID, team, hill) %>%
  summarise(scored = max(score)-min(score),
            broke_scored = sum(gain*broke,na.rm =T),
            hold_scored = sum(gain*rotated, na.rm = T),
            rotated = any(rotated)) %>%
  mutate(note = case_when(
    hold_scored < 20 & scored < 30 & rotated ~ paste0(team, " rotates and only scores ", scored),
    broke_scored > 20 ~ paste0(team, " breaks for ", broke_scored)
  ))
    
  # select(GAME_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S) %>%
  # filter(TEAM_A_SCORE >= 0, TEAM_B_SCORE >= 0, TIME_S > 4) %>%
  # pivot_longer(c(-TIME_S,-GAME_ID), names_to = "team", values_to = "score") %>%
  # # left_join(hill_num, by = "")
  # mutate(hill = pmax((TIME_S-5) %/% 60, 0)+1,
  #        seconds_into_hill = ((TIME_S-5) %% 60)+1,
  #        new_hill = ifelse(hill > lag(hill), hill, NA),
  #        total_hills = 4) %>%
  # group_by(GAME_ID, team, hill) %>%
  # mutate(gain = score - lag(score),
  #        running_gain = zoo::rollapply(gain, width = 10, FUN = sum, partial = TRUE, align = "right"), 
  #         rotated = (hill > 1 & 
  #          sum(gain*(seconds_into_hill <= 10), na.rm =T) >= 5 & 
  #          seconds_into_hill == 10),
  #        rotated = any(rotated)) %>%
  # group_by(GAME_ID, TIME_S) %>%
  # mutate(broken = seconds_into_hill > 10 & running_gain < max(mean(running_gain),5) & rotated,
  #        broke = any(broken) & !broken) %>%
  group_by(GAME_ID, team, hill) %>%
  summarise(scored = max(score)-min(score),
            broke_scored = sum(gain*broke,na.rm =T),
            hold_scored = sum(gain*rotated, na.rm = T)) %>%
  # group_by(GAME_ID, team) %>%
  # summarise(points = sum(scored),
  #           broke_points = sum(broke_scored),
  #           hold_points = sum(hold_scored))
  # 
  # 

  ggplot()+geom_line(aes(TIME_S, score, color = team),size = 2)+
  geom_line( aes(TIME_S, broken*250, color = team))+
  theme_classic() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 18),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(5,score_feed %>%
                                          mutate(TIME_S = time_f) %>%
                                          summarise(m = max(TIME_S)) %>%
                                          pull(m), 60),
                     labels = seq(1,score_feed %>%
                                          mutate(TIME_S = time_f) %>%
                                          summarise(m = max(TIME_S)) %>%
                                          pull(m)%/%60 + 1, 1)) +
  geom_vline(xintercept = seq(5,score_feed %>%
                                mutate(TIME_S = time_f) %>%
                                summarise(m = max(TIME_S)) %>%
                                pull(m), 60))
