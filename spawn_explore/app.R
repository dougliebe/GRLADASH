library(shiny)
library(shinydashboard)
library(png)
library(hexbin)
source("load_funcs.R")

# setwd(here::here('spawn_explore'))
onStop(function() {
    dbDisconnect(con)
})
ui <- dashboardPage(
    dashboardHeader(title = "Spawn Explorer"),
    dashboardSidebar(
        radioButtons(
            inputId = "map_id",
            label = "Map:",
            choices = map_list[map_list %in% c(41,42,43,44,49)],
            selected = 42,
            inline = T
            # selectize = FALSE
        ),
        radioButtons(
            'hill_id',
            label = "Hill no.",
            choices = c(1,2,3,4,5),
            selected = 1
        ),
        radioButtons(
            'spawn_loc',
            label = "See spawns or player locations",
            choices = list('spawn' = 1, 
                           'locations' = 2,
                           'popular spawns' = 3),
            selected = 1
        )
        
    ),
    dashboardBody(
        fluidRow(
            box(width = 12,
                plotOutput("map_graph", click = "plot_click")
            )
        ),
        fluidRow(
            box(tableOutput('info'))
        )
    )
)

server <- function(input, output) {
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
                   190,125,340,75,190),
        hill_y = c(180,100,100,325,
                   275,100,320,175,200,
                   200,75,200,75,300,
                   120,300,150,100,
                   240,150,175,50,150)
    )
    
    spawn_location <- reactiveValues(x = 200, y = 200)
    observe({
        req(input$plot_click)
        spawn_location$x <- input$plot_click$x
        spawn_location$y <- input$plot_click$y
    })
    filtered <- reactive({
        # filter the spawn data
        temp_ <- spawn_q %>%
            left_join(game_q, by = 'GAME_ID') %>%
            # mutate(TIME = ifelse(abs(TIME - START_SEC) < 10, TIME - START_SEC, TIME)) %>%
            dplyr::select(1,2,3,4,5,6,8,9) %>%
            mutate(TIME = TIME - 7) %>%
            filter(MAP_ID == !!input$map_id, GAME_ID > 100) %>%
            filter(TIME > 5, TIME < 60*13) %>%
            collect() %>%
            mutate(hill = TIME %/% 60+1,
                   seconds_into_hill = ((TIME-5) %% 60)+1,
                   # new_hill = ifelse(hill > lag(hill), hill, NA),
                   total_hills = ifelse(MAP_ID %in% c(43,44, 46, 49), 5, 4),
                   hill_no = hill-(((hill-1)%/%total_hills)*total_hills)) %>%
            filter(hill_no == !!input$hill_id) %>%
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
            mutate(across(c(starts_with('pos')), as.integer))
        temp2 <- temp_
        hill_x <- temp_ %>%
        left_join(map_hill_locs, by = c("MAP_ID", 'hill_no')) %>%
            slice(1) %>% pull(hill_x)
        hill_y <- temp_ %>%
            left_join(map_hill_locs, by = c("MAP_ID", 'hill_no')) %>%
            slice(1) %>% pull(hill_y)
        # generate the output data for both plots
        data <- temp2 %>%
            filter(type == "SPAWN") %>%
            # filter(SPAWN_ID == 1419) %>%
            left_join(temp_ %>%
                          filter(type != "SPAWN") %>%
                          dplyr::select(type, SPAWN_ID, GAME_ID, pos_x, pos_y),
                      by = c("SPAWN_ID", "GAME_ID"),
                      suffix = c("_spawn", "_opp")) %>%
            # mutate(across(c("MAP_ID", 'hill_no'), as.integer)) %>%
            left_join(map_hill_locs, by = c("MAP_ID", 'hill_no')) %>%
            mutate(dist = sqrt((pos_x_spawn - pos_x_opp)^2 + (pos_y_spawn - pos_y_opp)^2),
                   dist_hill = sqrt((hill_x - pos_x_spawn)^2 + (hill_y - pos_y_spawn)^2),
                   opp_dist_hill = sqrt((hill_x - pos_x_opp)^2 + (hill_y - pos_y_opp)^2),
                   
            ) %>%
            group_by(SPAWN_ID) %>%
            mutate(mid_dist = median(dist)) %>%
            filter(mid_dist > 200) %>%
            dplyr::select( -dist, -mid_dist) %>%
            distinct() %>%
            filter(dist_hill > 50, opp_dist_hill > 50) 
        map_png <- readPNG(here::here(
            'spawn_explore',
            'maps',
          paste0(tolower(map_q %>%
                             filter(MAP_ID == !!input$map_id) %>%
                             pull(MAP_NAME)),".png")))
        w = dim(map_png)[1]
        h = dim(map_png)[2]
        if(input$spawn_loc == 1) {
            density_data <- getDensityDiff(data1 = data %>%
                                               mutate(click_dist = sqrt((spawn_location$x - pos_x_spawn)^2 +
                                                                            (spawn_location$y - pos_y_spawn)^2)) %>%
                                               filter(str_detect(type_opp, "loc"), type_spawn == "SPAWN", click_dist < 150),
                                           data2 = data %>%
                                               mutate(click_dist = sqrt((spawn_location$x - pos_x_spawn)^2 +
                                                                            (spawn_location$y - pos_y_spawn)^2)) %>%
                                               filter(str_detect(type_opp, "opp"), type_spawn == "SPAWN", click_dist < 150)
            )
            # density_data <- getDensitySingle(data = data %>%
            #     mutate(click_dist = sqrt((spawn_location$x - pos_x_spawn)^2 +
            #                                  (spawn_location$y - pos_y_spawn)^2)) %>%
            #     filter(str_detect(type_opp, "opp"), type_spawn == "SPAWN", click_dist < 150))
            Plot <- ggplot()+
                annotation_custom(grid::rasterGrob(map_png, width=unit(1,"npc"), height=unit(1,"npc")), 0, h, 0, -w)+
                geom_tile( data = density_data, aes(x, y, fill=z,alpha = abs(z))) +
                scale_fill_gradient2(low="red",mid="white", high="blue", midpoint=0,
                                     labels = c("Opponent", "Teammate"), breaks = c(-0.8,0.8)) +
                scale_y_reverse(limits = c(w,0)) +
                coord_equal()+
                lims(x = c(0,h))+
                guides(alpha = F)
        } else if(input$spawn_loc == 2) {
            Plot <- data %>%
                ggplot() +
                annotation_custom(grid::rasterGrob(map_png, width=unit(1,"npc"), height=unit(1,"npc")), 0, h, 0, -w) +
                theme_bw() +
                theme(legend.position = 'none',
                      axis.title = element_blank(),
                      axis.text = element_blank()) +
                coord_equal() +
                stat_bin_hex(aes(x = pos_x_opp, y = pos_y_opp),
                             # contour_var = "ndensity",
                             alpha = 0.5,
                             binwidth = 20) +
                scale_fill_viridis_c() +
                scale_y_reverse(limits = c(w,0)) +
                geom_point(aes(x = !!hill_x, y = !!hill_y), shape= 15,
                           color =  'red', size = 7,
                           alpha = 0.3)+
                lims(x = c(0, h))+
                theme(legend.position = 'none')
        } else {
            Plot <- data %>%
                ggplot() +
                annotation_custom(grid::rasterGrob(map_png, width=unit(1,"npc"), height=unit(1,"npc")), 0, h, 0, -w) +
                theme_bw() +
                # geom_point(aes(pos_x_spawn, pos_y_spawn), color = 'red')
                # theme(legend.position = 'none',
                #       axis.title = element_blank(),
                #       axis.text = element_blank()) +
                # coord_equal() +
                stat_bin_hex(aes(x = pos_x_spawn, y = pos_y_spawn),
                             # contour_var = "ndensity",
                             alpha = 0.5,
                             binwidth = 30) +
                # scale_fill_viridis_c() +
                scale_y_reverse(limits = c(w,0)) +
                # geom_point(aes(x = !!hill_x, y = !!hill_y), shape= 15,
                #            color =  'red', size = 7,
                #            alpha = 0.3)+
                # lims(x = c(0, h))+
                theme(legend.position = 'none')
        }
        
        
        
        
        # return all object as a list
        list(Plot = Plot, data = data, hill_x = hill_x)
        
    })
    
    # filtered <- reactive({
    #     temp_ <- data()
    #     data() %>%
    #         filter(type == "SPAWN") %>%
    #         # filter(SPAWN_ID == 1419) %>%
    #         left_join(temp_ %>%
    #                       filter(type != "SPAWN") %>%
    #                       dplyr::select(type, SPAWN_ID, GAME_ID, pos_x, pos_y),
    #                   by = c("SPAWN_ID", "GAME_ID"),
    #                   suffix = c("_spawn", "_opp")) %>%
    #         mutate(across(c("MAP_ID", 'hill_no'), as.integer)) %>%
    #         left_join(map_hill_locs, by = c("MAP_ID", 'hill_no')) %>%
    #         mutate(dist = sqrt((pos_x_spawn - pos_x_opp)^2 + (pos_y_spawn - pos_y_opp)^2),
    #                dist_hill = sqrt((hill_x - pos_x_spawn)^2 + (hill_y - pos_y_spawn)^2),
    #                opp_dist_hill = sqrt((hill_x - pos_x_opp)^2 + (hill_y - pos_y_opp)^2),
    #                
    #         ) %>%
    #         group_by(SPAWN_ID) %>%
    #         mutate(mid_dist = median(dist)) %>%
    #         filter(mid_dist > 200) %>%
    #         dplyr::select( -dist, -mid_dist) %>%
    #         distinct() %>%
    #         filter(dist_hill > 100, opp_dist_hill > 100) 
    #     # %>%
    #     #     filter(dist_click < 100, seconds_into_hill < 50)
    # 
    # })
    
    
    output$map_graph <- renderPlot({


        # filtered() %>%
        #     mutate(click_dist = sqrt((click_x - pos_x_spawn)^2 +
        #                                  (click_y - pos_y_spawn)^2)) %>%
        #     filter(str_detect(type_opp, "loc"), type_spawn == "SPAWN", click_dist < 50) %>%
        #     ggplot(aes(pos_x_spawn, pos_y_spawn))+geom_point()
        # filtered() %>%
        #     ggplot() +
        #         annotation_custom(grid::rasterGrob(map_png, width=unit(1,"npc"), height=unit(1,"npc")), 0, h, 0, -w) +
        #         theme_bw() +
        #         theme(legend.position = 'none',
        #               axis.title = element_blank(),
        #               axis.text = element_blank()) +
        #         coord_equal() +
        #         stat_bin_hex(aes(x = pos_x_opp, y = pos_y_opp),
        #                           # contour_var = "ndensity",
        #                           alpha = 0.5,
        #                           binwidth = 20) +
        #         scale_fill_viridis_c() +
        #         scale_y_reverse() +
        #         geom_point(aes(x = hill_x, y = hill_y), shape= 15,
        #                    color =  'red', size = 7,
        #                    alpha = 0.3)+
        #         lims(x = c(0, h), y = c(w,0))+
        #         theme(legend.position = 'none')
        #         # annotate("rect",xmin = 500, xmax = h, ymin = 200, ymax = w, fill = 'red', alpha = 0.1)+
        #         # annotate('text', x = 500, y = 350, label = "  Spawn Back Here", color = "white", hjust = 0)+
        #         # annotate('text', x = 500, y = 350, label = "Opponent Locations  ", color = "white", hjust = 1)
        filtered()$Plot
    })
    
    output$info <- renderTable({
        # paste0("Spawn Choice: ",input$plot_click$x,", ",input$plot_click$y)
        filtered()$hill_x
    })
}

shinyApp(ui, server)
