source("load_data_rds.R")

data <- spawn_q %>%
  left_join(game_q, by = 'GAME_ID') %>%
  mutate(TIME = ifelse(abs(TIME - START_SEC) < 10, TIME - START_SEC, TIME)) %>%
  dplyr::select(1,2,3,4,5,6,8,9) %>%
  # group_by(GAME_ID) %>%
  # count()
  filter(MAP_ID == 42, GAME_ID > 100) %>%
  collect()

clean <- data %>%
  filter(TIME > 5) %>%
  mutate(hill = TIME %/% 60+1,
         seconds_into_hill = ((TIME-5) %% 60)+1,
         # new_hill = ifelse(hill > lag(hill), hill, NA),
         total_hills = ifelse(MAP_ID %in% c(43,44, 46,49), 5, 4),
         hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
  rename(SPAWN = LOCATION) %>%
  # separate(LOCATION, into = c('spawn_x', 'spawn_y')) %>%
  separate(FRIENDLY_LOCS,
           into = c('loc_1', 'loc_2', 'loc_3'),
           extra = 'drop',
           fill = 'right', 
           sep = "\\|") %>%
  separate(ENEMY_LOCS,
           into = c('opp_1', 'opp_2', 'opp_3', 'opp_4'),
           extra = 'drop',
           fill = 'right', 
           sep = "\\|") %>%
  pivot_longer(c("SPAWN",
                 starts_with("opp"),
                 starts_with('loc')),
               names_to = "type",
               values_to = "pos") %>%
  filter(!pos %in% c(NA,"")) %>%
  separate(pos, into = c('pos_x', 'pos_y')) %>%
  mutate(across(starts_with('pos'), as.integer))

clean %>%
  filter(pos_x %in% seq(250,700), pos_y %in% seq(100,350)) %>%
  ggplot(aes(pos_x, pos_y, color = (type=="SPAWN")))+geom_point()+
  scale_y_reverse() +
  scale_color_manual(values = c("TRUE"= 'black', "FALSE" = 'pink'))+
  facet_wrap(~hill_no)

## create spawn - opp location pairs
clean %>%
  filter(pos_x %in% seq(250,700), pos_y %in% seq(100,350)) %>%
  filter(type == "SPAWN") %>%
  # filter(SPAWN_ID == 1419) %>%
  left_join(clean %>%
              filter(pos_x %in% seq(250,700), pos_y %in% seq(100,350)) %>%
              filter(type %in% c("opp_1", "opp_2", "opp_3", "opp_4")) %>%
              dplyr::select(SPAWN_ID, GAME_ID, pos_x, pos_y),
            by = c("SPAWN_ID", "GAME_ID"),
            suffix = c("_spawn", "_opp")) %>%
  mutate(PAIR_ID = 1:n(),
         dist = sqrt((pos_x_spawn - pos_x_opp)^2 + (pos_y_spawn - pos_y_opp)^2)) %>%
  ggplot()+
  geom_point(aes(pos_x_spawn, pos_y_spawn), alpha = 0.5, color = 'pink')+
  geom_point(data = clean %>%
               filter(pos_x %in% seq(250,700), pos_y %in% seq(100,350)) %>%
               filter(type == "SPAWN") %>%
               # filter(SPAWN_ID == 1419) %>%
               left_join(clean %>%
                           filter(type %in% c("opp_1", "opp_2", "opp_3", "opp_4")) %>%
                           dplyr::select(SPAWN_ID, GAME_ID, pos_x, pos_y),
                         by = c("SPAWN_ID", "GAME_ID"),
                         suffix = c("_spawn", "_opp")) %>%
               left_join(map_hill_locs, by = c("MAP_ID", 'hill_no')) %>%
               mutate(PAIR_ID = 1:n(),
                      dist = sqrt((pos_x_spawn - pos_x_opp)^2 + (pos_y_spawn - pos_y_opp)^2),
                      dist_hill = sqrt((hill_x - pos_x_spawn)^2 + (hill_y - pos_y_spawn)^2)) %>%
               group_by(SPAWN_ID) %>%
               mutate(mid_dist = median(dist)) %>%
               filter(mid_dist > 100) %>%
               dplyr::select(-pos_x_opp, -pos_y_opp, -PAIR_ID, -dist, -mid_dist) %>%
               distinct() %>%
               filter(dist_hill > 100),
             aes(pos_x_spawn, pos_y_spawn)) +
  scale_y_reverse() +
  facet_wrap(~hill_no)
  ggplot() + geom_histogram(aes(dist))
  ggplot()+geom_segment(aes(x = pos_x_spawn, y = pos_y_spawn,
                            xend = pos_x_opp, yend = pos_y_opp, color = -dist), alpha = 0.1)

clean %>%
  group_by(SPAWN_ID, GAME_ID) %>%
  mutate(spawn_x = sum(pos_x*(type == "SPAWN")),
         spawn_y = sum(pos_y*(type == "SPAWN"))) %>%
  filter(startsWith(type, "opp_"))


clean %>%
  filter(SPAWN_ID == 1419) %>%
  ggplot(aes(pos_x, pos_y, group = SPAWN_ID))+
  geom_line()















