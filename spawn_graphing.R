
map_hill_locs <- tibble(
  MAP_ID = c(41,41,41,41, #moscow
             43,43,43,43,43, # garrison
             44,44,44,44,44, # checkmate
             42,42,42,42,  # crossroads
             49,49,49,49,49), # Raid
  hill_no = c(1,2,3,4,
              1,2,3,4,5,
              1,2,3,4,5,
              1,2,3,4,
              1,2,3,4,5),
  hill_x = c(400,525,250,325,
             350,325,500,400,275,
             375,550,375,350,275,
             450,500,350,250,
             450,400,600,375,450),
  hill_y = c(180,100,100,325,
             275,100,320,175,200,
             200,75,200,75,300,
             120,300,150,100,
             300,225,200,150,240)
)


filtered <- clean %>%
  filter(type == "SPAWN") %>%
  # filter(SPAWN_ID == 1419) %>%
  left_join(clean %>%
              filter(type != "SPAWN") %>%
              dplyr::select(type, SPAWN_ID, GAME_ID, pos_x, pos_y),
            by = c("SPAWN_ID", "GAME_ID"),
            suffix = c("_spawn", "_opp")) %>%
  left_join(map_hill_locs, by = c("MAP_ID", 'hill_no')) %>%
  mutate(PAIR_ID = 1:n(),
         dist = sqrt((pos_x_spawn - pos_x_opp)^2 + (pos_y_spawn - pos_y_opp)^2),
         dist_hill = sqrt((hill_x - pos_x_spawn)^2 + (hill_y - pos_y_spawn)^2),
         opp_dist_hill = sqrt((hill_x - pos_x_opp)^2 + (hill_y - pos_y_opp)^2)
         ) %>%
  group_by(SPAWN_ID) %>%
  mutate(mid_dist = median(dist)) %>%
  filter(mid_dist > 200) %>%
  dplyr::select( -PAIR_ID, -dist, -mid_dist) %>%
  distinct() %>%
  filter(dist_hill > 100, opp_dist_hill > 100) 
# %>%
    # filter(hill_no == 2)
  # filter(pos_x_spawn > 500, pos_y_spawn > 200, hill_no %in% c(1), seconds_into_hill < 50)
  

##
library(png)
img = readPNG("C:/Users/liebe/Documents/cod-minimap-tracker/template/maps/garrison.png")
w = dim(img)[1] #max(filtered$pos_y_spawn)
h = dim(img)[2]#max(filtered$pos_x_spawn)
ggplot(filtered %>% filter(str_detect(type_opp, "opp"))) +
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, h, 0, -w) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank()) +
  coord_equal() +
  stat_density2d_filled(aes(x = pos_x_opp, y = pos_y_opp),
                        # contour_var = "ndensity",
                        alpha = 0.5,
                        n = 100) + 
  scale_color_viridis_c() + 
  scale_y_reverse() +
  geom_point(aes(x = pos_x_opp, y = pos_y_opp), shape= 4, color = 'red')+
  lims(x = c(0, h), y = c(w,0))+
  theme(legend.position = 'none')+
  annotate("rect",xmin = 500, xmax = h, ymin = 200, ymax = w, fill = 'red', alpha = 0.1)+
  annotate('text', x = 500, y = 350, label = "  Spawn Back Here", color = "white", hjust = 0)+
  annotate('text', x = 500, y = 350, label = "Opponent Locations  ", color = "white", hjust = 1)+
  facet_wrap(~hill_no,nrow = 4)+
  ggtitle("AFTER")
  
new <- filtered %>%
    filter(pos_x_spawn > 500)

new_team_loc <- new %>% filter(str_detect(type_opp, "loc"), type_spawn == "SPAWN")
new_opp_loc <- new %>% filter(str_detect(type_opp, "opp"), type_spawn == "SPAWN")

library(reshape2) # For melt function
library(MASS)
# Calculate the common x and y range for new_team_loc and new_opp_loc
xrng = range(c(new_team_loc$pos_x_opp, new_opp_loc$pos_x_opp))
yrng = range(c(new_team_loc$pos_y_opp, new_opp_loc$pos_y_opp))

# Calculate the 2d density estimate over the common range
d1 = kde2d(new_team_loc$pos_x_opp, new_team_loc$pos_y_opp, lims=c(xrng, yrng), n=200)
d2 = kde2d(new_opp_loc$pos_x_opp, new_opp_loc$pos_y_opp, lims=c(xrng, yrng), n=200)
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


# Plot difference between geyser2 and geyser1 density
ggplot()+
  annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, h, 0, -w)+
  geom_tile( data = diff12.m, aes(x, y, fill=z,alpha = abs(z))) +
  scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0,
                       labels = c("Opponent", "Teammate"), breaks = c(-0.8,0.8)) +
  scale_y_reverse() +
  coord_equal()+
  guides(alpha = F)
