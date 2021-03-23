#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(odbc)
library(DBI)
library(tidyverse)

source('all_sessions.R', local = TRUE)
onStop(function() {
    dbDisconnect(con)
})
function(input, output, session) {
    
    data_with_date <- reactive({
        ## depending on the tab selected, apply certain filters to the data
        ## pre-collect
        if(input$tabs == "series") {
            series_q %>%
                filter(DATE == lubridate::as_date(!!input$date)) 
            # %>%
            #     filter(TEAM_ABV_A %in% c("LAG",!!input$teams),
            #            TEAM_ABV_B %in% c("LAG",!!input$teams)
            #     )
        } else if (input$tabs == "trends") {
            series_q %>%
                filter(DATE >= lubridate::as_date(!!input$dateRange[1]),
                       DATE <= lubridate::as_date(!!input$dateRange[2])
                ) 
        } 
    })
    
    ## Update filters to only show teams and games
    ## from higher level filters
    observe({
        ## Update certain filters based on the current tab
        if(input$tabs == "series") {
            #update teams for only the day
            teams_unq <- data_with_date() %>%
                                      left_join(team_q,
                                                by = c("A_ID" = "TEAM_ID")) %>%
                                      left_join(team_q,
                                                by = c("B_ID" = "TEAM_ID"),
                                                suffix = c("_A", "_B")) %>%
                                      select(TEAM_ABV_A, TEAM_ABV_B, A_ID, B_ID) %>%
                                      collect()
            team_choices <- as.list(teams_unq %>%
                                        select(A_ID, B_ID) %>%
                                        unlist(use.names = F) %>%
                                        unique()) %>%
                set_names(teams_unq %>%
                              select(TEAM_ABV_A, TEAM_ABV_B) %>%
                              unlist(use.names = F) %>%
                              unique())
            
            # team_choices <- list(list(teams_unq %>%
            #                             pull(A_ID)) %>%
            #     set_names(teams_unq %>%
            #                   pull(TEAM_ABV_A)),
            # list(teams_unq %>%
            #          pull(B_ID)) %>%
            #     set_names(teams_unq %>%
            #                   pull(TEAM_ABV_B)))
            updateSelectInput(session, 'teams',
                              choices = team_choices %>% unlist())
            
        }
    })
    
    
    ## Create base info about all applicable games from selection
    ## at any time
    base_info <- reactive({
        temp_ <- data_with_date() %>%
            inner_join(game_q,
                       by = "SERIES_ID") %>%
            left_join(map_q, by = "MAP_ID") %>%
            left_join(mode_q, by = "MODE_ID") %>%
            left_join(team_q,
                      by = c("A_ID" = "TEAM_ID")) %>%
            left_join(team_q,
                      by = c("B_ID" = "TEAM_ID"),
                      suffix = c("_A", "_B")) %>%
            mutate(info = paste0(TEAM_ABV_A, " ", TEAM_A_SCORE, "-",TEAM_B_SCORE, " ", TEAM_ABV_B, " on ",
                                 MAP_NAME,", ",MODE_ALIAS))
        if(length(input$teams) == 1 & input$tabs == "series") {
            temp_ <- temp_ %>%
                filter(A_ID == !!input$teams | B_ID == !!input$teams)
        } else if(input$tabs == "trends") {
            temp_ <- temp_ %>%
                filter(MODE_ID %in% !!input$mode_id)
        }
        temp_
    })

    ## once we have a date to work with + the series
    ## update the games we care about after team series has been selected
    observeEvent(input$teams, {
        # update games with only this team
        game_choices <- base_info() %>%
            filter(A_ID == !!input$teams |
                       B_ID == !!input$teams) %>%
            select(MAP_NAME, MODE_ALIAS, TEAM_ABV_A, TEAM_ABV_B, TEAM_A_SCORE, TEAM_B_SCORE, GAME_ID) %>%
            mutate(info = paste0(MODE_ALIAS, " - ",MAP_NAME,", ",case_when(
                TEAM_A_SCORE > TEAM_B_SCORE & TEAM_ABV_A == "LAG" ~ "W",
                TEAM_B_SCORE > TEAM_A_SCORE & TEAM_ABV_B == "LAG" ~ "W",
                TRUE ~ "L"
                
            ) ))
        
        updateSelectInput(session, 'game',
                          choices = c("ALL" = 0) %>%
                              append(as.list(game_choices %>%
                                                 pull(GAME_ID)) %>%
                                         set_names(game_choices %>%
                                                       pull(info))))
    })

    output$score_graph <- renderPlot({
        # create a df to match TEAM_X_SCORE
        # to the right team abv
        team_ref_ <- tibble(
            GAME_ID = c(base_info() %>% pull(GAME_ID),base_info() %>% pull(GAME_ID)),
            team = c(rep('TEAM_A_SCORE', length(base_info() %>% pull(TEAM_ABV_A))),
                     rep('TEAM_B_SCORE', length(base_info() %>% pull(TEAM_ABV_B)))),
            TEAM_ABV = c(base_info() %>% pull(TEAM_ABV_A),base_info() %>% pull(TEAM_ABV_B))
        )
        ## If only one game has been chosen
        if(!!input$game != 0) {
            ## get the meta data for the game in question
            current_game <- base_info() %>%
                filter(GAME_ID == !!input$game)
            team_a = current_game %>% pull(TEAM_ABV_A)
            team_b = current_game %>% pull(TEAM_ABV_B)
            score_a = current_game %>% pull(TEAM_A_SCORE)
            score_b = current_game %>% pull(TEAM_B_SCORE)
            map_name = current_game %>% pull(MAP_NAME)
            mode_name = current_game %>% pull(MODE_NAME)
            game_id <- base_info() %>%
                filter(GAME_ID == !!input$game) %>% pull(GAME_ID)
            # opp_name <- c(team_a, team_b)[c(team_a, team_b) != "LAG"]
            ## Pull score data and calculate holds and breaks
            notations <- score_q %>%
                filter(GAME_ID == game_id[1]) %>%
                left_join(game_q %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE),
                          by = "GAME_ID") %>%
                collect() %>%
                clean_scores() %>%
                left_join(team_ref_, by = c('team', "GAME_ID")) %>%
                group_by(GAME_ID, TEAM_ABV, hill, hill_no) %>%
                summarise(scored = max(score)-min(score),
                          broke_scored = sum(gain*broke,na.rm =T),
                          hold_scored = sum(gain*hold, na.rm = T),
                          rotated = any(rotated),
                          tgt_score = max(score),
                          running = max(running_120,na.rm = T)) %>%
                group_by(GAME_ID, hill, hill_no) %>%
                mutate(opp_score =  sum(scored - scored)) %>%
                group_by(GAME_ID, TEAM_ABV) %>%
                mutate(pm = scored + lag(scored) - opp_score - lag(opp_score)) %>%
                mutate(note = case_when(
                    # hold_scored < 20 &
                    #     scored < 20 & rotated ~ paste0(TEAM_ABV,
                    #                                    " rotates to P",
                    #                                    hill_no,
                    #                                    " and only scores ",
                    #                                    scored),
                    hold_scored > 1 & rotated ~ paste0(TEAM_ABV,
                                                       " rotates to P",
                                                       hill_no,
                                                       " and scores ",
                                                       scored),
                    # broke_scored > 20 ~ paste0(TEAM_ABV,
                    #                            " breaks P",
                    #                            hill_no," for ",
                    #                            broke_scored),
                    # pm > 60 ~ paste0(TEAM_ABV,
                    #                            " chains for +",
                    #                             pm,
                    #                       " points"
                    #                            )
                )) %>%
                filter(!is.na(note)) %>%
                select(hill, note,tgt_score)
            
            ## Take score data and plot score graph to be returned
            score_plot <- score_q %>% 
                filter(GAME_ID == game_id[1]) %>%
                left_join(game_q %>% select(-TEAM_A_SCORE, -TEAM_B_SCORE),
                          by = "GAME_ID") %>%
                collect() %>%
                clean_scores() %>%
                left_join(team_ref_, by = c('team', "GAME_ID"))
            if(mode_name == "Hardpoint") {
                # if its HP only
                score_breaks = seq(5,max(score_plot$TIME_S),60)
                score_labels = seq(1, length(score_breaks))
                score_plot %>%
                    ggplot(aes(TIME_S, score, color = TEAM_ABV))+geom_line(size = 2)+
                    theme_classic() +
                    theme(
                        legend.position = 'none',
                        axis.text = element_text(size = 18),
                        title = element_text(size = 20)) +
                    scale_color_manual(values = group_colors)+
                    scale_x_continuous(breaks = score_breaks,
                                       labels = score_labels) +
                    ggtitle(paste(team_a, score_a, "-", team_b, score_b),
                            subtitle = paste(map_name, mode_name))+
                    xlab("HILL")+ylab("SCORE")+
                    ggrepel::geom_label_repel(data = notations,
                                              aes((hill - 1)*60 + 5,
                                                  tgt_score, label = note),
                                              hjust = 1.0,
                                              nudge_x = -25,
                                              # nudge_y = 100,
                                              angle = -20,
                                              box.padding = 0.3,fontface = 'bold')
            } else {
                ## if its control or snd instead of HP
                score_plot %>%
                    # mutate(note = ifelse())
                    ggplot(aes(TIME_S, score, fill = TEAM_ABV))+geom_col(position = 'dodge')+
                    xlim(c(1,NA))+xlab("ROUND")
            }
            
        }
        else if(!!input$game == 0){
            team_abvs <- team_q %>%
                filter(TEAM_ID %in% !!input$teams) %>%
                  pull(TEAM_ABV)
                                
            score_q %>%
                right_join(base_info() %>%
                               select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
                collect() %>%
                clean_scores() %>%
                left_join(team_ref_, by = c('team', "GAME_ID")) %>%
                # filter(TEAM_ABV %in% team_abvs) %>%
                group_by(GAME_ID, TEAM_ABV, hill) %>%
                summarise(scored = max(score)-min(score),
                          broke_scored = sum(gain*broke,na.rm =T),
                          hold_scored = sum(gain*hold, na.rm = T)) %>%
                group_by(GAME_ID, hill) %>%
                mutate(opp_score = sum(scored)-scored,
                       opp_break = sum(broke_scored)-broke_scored,
                       lost_on_holds = sum(hold_scored)-hold_scored) %>%
                ungroup() %>%
                # mutate(TEAM_ABV = ifelse(TEAM_ABV == "LAG", "LAG", "OPP")) %>%
                # filter(TEAM_ABV == "LAG") %>%
                    group_by(TEAM_ABV) %>%
                summarise(points = sum(scored),
                          broke_points = sum(broke_scored),
                          hold_points = sum(hold_scored),
                          broke_pct  = broke_points/points,
                          hold_pct = hold_points/points) %>%
                # ungroup() %>%
                # mutate(point_pct = points/sum(points)) %>%
                select(TEAM_ABV, points, broke_points, hold_points) %>%
                # mutate(hold)
                pivot_longer(-TEAM_ABV, names_to = 'stat', values_to = "value") %>%
                ggplot(aes(stat, value,
                           fill = TEAM_ABV,
                           label = value)) +
                geom_col( position= 'dodge')+ 
                geom_text(position = position_dodge(width = .9),    # move to center of bars
                          vjust = 1.5,    # nudge above top of bar
                          size = 5,
                          color = 'white') +
                scale_fill_manual(values = group_colors) +
                # scale_y_continuous(labels = scales::percent) +
                theme_classic() +
                theme(
                    legend.position = 'none',
                    axis.title = element_blank(),
                      axis.text = element_text(size = 18),
                      title = element_text(size = 20),
                    axis.line.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.y = element_blank())
                
        }

    })
    
    output$link <- renderUI({
        if(!!input$game != 0) {
            link_base <- base_info() %>%
                inner_join(ref_q, by = "REF_ID") %>%
                filter(GAME_ID == !!input$game) %>%
                select(REF_VALUE, START_SEC) %>%
                collect()
            # time_stamp <- base_info() %>%
            #     filter(GAME_ID == !!input$game) %>% pull(START_SEC)
            url = a("VOD LINK", href = paste0("https://youtu.be/",
                                              link_base$REF_VALUE[1],
                                              "?t=",
                                              link_base$START_SEC[1]),
                    target="_blank")
            tagList("Click here to go to YT video!", url)
        } else {
            ""
        }
    })
    
    output$records <- renderTable({
        base_info() %>%
            collect() %>%
            getRecords() %>%
            group_by(TEAM_ABV, MODE_NAME, MAP_NAME) %>%
            summarise(wins = sum(win),
                      losses = n() - wins,
                      score = mean(score),
                      opp_score = mean(opp_score)) %>%
            filter(TEAM_ABV == "LAG") %>%
            ungroup() %>%
            select(-TEAM_ABV)
            
    })
    
    output$sideRecord <- renderTable({
        base_info() %>%
            collect() %>%
            getRecords() %>%
            group_by(side, MODE_NAME, MAP_NAME) %>%
            summarise(wins = sum(win),
                      losses = n() - wins) %>%
            filter(side == "blue - NATO") %>%
            ungroup()
        
    })

    
    # 
    # output$mtcars <- renderTable(head(single_game_info()))
}