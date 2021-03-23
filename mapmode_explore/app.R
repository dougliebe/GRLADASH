library(shiny)
library(shinydashboard)
library(zoo)
library(scales)
library(DT)
library(lubridate)
source('base_funcs.R', local = T)
onStop(function() {
    dbDisconnect(con)
})

sidebar <- dashboardSidebar(
    sidebarMenu(
        selectInput('team',
                    label = "Choose Team:",
                    choices = team_list,
                    multiple = F,
                    selected = 6),
        dateRangeInput('dateRange',
                       label = 'Date Range:',
                       start = Sys.Date()-7,
                       end = Sys.Date()
        ),
        menuItem("Overall", tabName = 'overall', icon = icon("dashboard")),
        menuItem("Hardpoint", tabName = 'hardpoint', icon = icon("angle-double-down")),
        menuItem("Control", tabName = 'control', icon = icon("flag")),
        menuItem("Search & Destroy", tabName = 'search', icon = icon("bomb"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "overall",
                fluidRow(
                    valueBoxOutput('overallBox'),
                    tableOutput('overall')
                ),
                fluidRow(
                    box(
                        dataTableOutput('all_series_table')
                    )
                )
        ),
        
        ## HARDPOINT
        tabItem(tabName = "hardpoint",
                fluidRow(
                    box(
                        selectInput(inputId = 'hp_select',
                                    label = "Choose Specific Map",
                                    choices = hp_list %>% append(list("All"=1)),
                                    selected = 1
                        ),width = 3
                    ),
                    valueBoxOutput('hardpoint_score'),
                    tableOutput('all_hp_maps_table')
                ),
            fluidRow(
                box(
                    dataTableOutput('hp_general_table')
                ),
                box(plotOutput("rolling_score_plot_hp"), width = 6)
            ),
            fluidRow(
                box(
                    dataTableOutput("recent_hp")
                )
            )
        ),
        
        ## CONTROL
        tabItem(tabName = "control",
                fluidRow(
                    box(
                        selectInput(inputId = 'ctl_select',
                                    label = "Choose Specific Map",
                                    choices = control_list %>% append(list("All"=1)),
                                    selected = 1
                        ),width = 2
                    ),
                    valueBoxOutput('ctl_score',width = 3),
                    tableOutput("all_ctl_maps_table")
                ),
                fluidRow(
                    valueBoxOutput("ctl_off_box",width = 3),
                    valueBoxOutput("ctl_def_box",width = 3)
                ),
                fluidRow(
                    box(
                        dataTableOutput("ctl_general_table")
                    ), box(
                        plotOutput("control_round_scores")
                    )
                ),
                fluidRow(
                    box(
                        dataTableOutput("recent_ctl")
                    )
                )
            ),
        
        ## SEARCH AND DESTROY
        tabItem(tabName = "search",
                box(
                    selectInput(inputId = 'snd_select',
                                label = "Choose Specific Map",
                                choices = snd_list %>% append(list("All"=1)),
                                selected = 1
                    ),width = 3
                ),
                valueBoxOutput('snd_score'),
                tableOutput("all_snd_maps_table")
        )
    )
    
)

ui <- dashboardPage(
    dashboardHeader(title = "Team Scores"),
    sidebar,
    body,
    skin = 'purple'
)

server <- function(input, output) {

    ## Overall box
    output$overall <- renderTable({
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(1,3,4)) %>%
            getRecords(team_ = input$team) %>%
            group_by(MODE_ID) %>%
            summarise(w = sum(win),
                      l = n() - w) %>%
            left_join(mode_q %>% collect(), by = "MODE_ID") %>%
            select(MODE_NAME, w, l)
    })
    output$overallBox <- renderValueBox({
        ## Need win% overall and selected time
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(1,3,4)) %>%
            getRecords(team_ = input$team) %>%
            ungroup() %>%
            summarise(win_pct = mean(win)) %>%
            pull(win_pct) ->
            inRangeWinPct
        getFilteredGames(team_ = input$team,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(1,3,4)) %>%
            getRecords(team_ = input$team) %>%
            ungroup() %>%
            summarise(win_pct = mean(win)) %>%
            pull(win_pct) ->
            overallWinPct
        diff = inRangeWinPct - overallWinPct
        overallbox_col = color_scale(diff)
        valueBox(value = scales::label_percent()(inRangeWinPct),
                 subtitle = paste0("Map Win (",ifelse(diff >= 0, "+", ""),
                                   scales::label_percent(accuracy = 0.01)(diff)," vs YTD)"),
                 icon = icon(case_when(
                     diff > 0 ~ "angle-up",
                     diff == 0 ~ "grip-lines",
                     diff < 0 ~ "angle-down"
                 )),
                 color = overallbox_col
        )
    })
    
    output$all_series_table <- renderDT({
        series_q %>%
            filter(A_ID %in% !!input$team | B_ID %in% !!input$team,
                   DATE >= !!input$dateRange[1],
                   DATE <= !!input$dateRange[2]) %>%
            left_join(ref_q, by = "REF_ID") %>%
            left_join(team_q, by = c("A_ID" = "TEAM_ID"))%>%
            left_join(team_q, by = c("B_ID" = "TEAM_ID")) %>%
            mutate(MATCH = paste0(TEAM_ABV.x," vs ", TEAM_ABV.y),
                   LINK = case_when(
                       !is.na(REF_VALUE) ~ paste0('<a href=\"',
                                                        "https://youtu.be/",
                                                        REF_VALUE,
                                                        '\"> LINK </a>'
                                                        ),
                       TRUE ~ NA
                       )) %>%
            select(DATE, MATCH, LINK) %>%
            collect() %>%
            mutate(DATE = lubridate::as_date(as.Date(DATE))) %>%
            arrange(desc(DATE)) %>%
            DT::datatable(escape = FALSE,
                          filter = 'top',
                          options = list(
                lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                searching = FALSE,
                pageLength = 5
            ))
    })
    
    
    
    
    # HARDPOINT TAB
    data_for_hp <- reactive({
        if(input$hp_select==1) {
            map_selected <- c(41,42,43,44,49,51)
        } else {
            map_selected <- input$hp_select
        }
        
        all_hp_maps_table <- 
            getFilteredGames(team_ = input$team,
                             date_ = input$dateRange,
                             mode_ = c(1)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            left_join(map_q %>% collect(), by = 'MAP_ID') %>%
            group_by(MAP_NAME) %>%
            summarise(w = sum(win, na.rm = T),
                      l = n() - w)
        
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(1)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(win_pct = mean(win, na.rm = T)) %>%
            pull(win_pct) ->
            inRangeWinPct
        getFilteredGames(team_ = input$team,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(1)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(win_pct = mean(win, na.rm = T)) %>%
            pull(win_pct) ->
            overallWinPct
        
        
        
        diff = inRangeWinPct - overallWinPct
        hp_win_box_col <- color_scale(diff)
        valueBox(value = scales::label_percent()(inRangeWinPct),
                 subtitle = paste0("Map Win (",ifelse(diff >= 0, "+", ""),
                                   scales::label_percent(accuracy = 0.01)(diff)," vs YTD)"),
                 icon = icon(case_when(
                     diff > 0 ~ "angle-up",
                     diff == 0 ~ "grip-lines",
                     diff < 0 ~ "angle-down"
                 )),
                 color = hp_win_box_col
        ) ->
            hp_win_box
        
        ## 10 game rolling score
        getFilteredGames(team_ = input$team,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(1)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            left_join(map_q %>% collect(), by = "MAP_ID") %>%
            ungroup() %>%
            group_by(MAP_NAME) %>%
            mutate(game_no = 1:n(),
                   roll_score = zoo::rollmeanr(score, k = 5, fill = NA)) %>%
            slice_tail(n = 50) ->
            rolling_score
        
        ggplot(data = rolling_score) +
            geom_line(aes(GAME_ID, roll_score, color = MAP_NAME), size = 1) +
            labs(x = "Game #", y = "Avg. Score", title = "10-game rolling average scores")+
            lims(y = c(100, 250))+
            theme_bw() +
            theme(panel.border = element_rect(color = '#3d1866', fill = NA),
                  axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
                  axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
                  legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
                  legend.title = element_text(face = 'bold', color = '#3d1866', size = 14),
                  title = element_text(face = 'bold', color = '#3d1866', size = 17),
                  text = element_text(family = 'mono')) ->
            rolling_score_plot
        
        #### General Table of records ######
        
        ## Get W-L of all selected
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(1)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(
                wl = paste0(sum(win),"-", n() - sum(win)),
                avg_score = mean(score),
                avg_opp_score = mean(opp_score),
                median_diff = median(score-opp_score)
            ) %>%
            ungroup() %>%
            mutate(variable = "Overall") ->
            hp_general_table_p1
        ## by side
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(1)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            group_by(side) %>%
            summarise(
                wl = paste0(sum(win),"-", n() - sum(win)),
                avg_score = mean(score),
                avg_opp_score = mean(opp_score),
                median_diff = median(score-opp_score)
            ) %>%
            rename(variable = side) ->
            hp_general_table_p2
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(1)) %>%
            getHardpointScores() %>%
            filter(MAP_ID %in% map_selected) %>%
            group_by(GAME_ID, TEAM_ID, hill, hill_no) %>%
            summarise(score = max(score)-min(score)) %>%
            group_by(GAME_ID, hill, hill_no) %>%
            mutate(opp_score = sum(score)-score,
                   win = score > opp_score) %>%
            group_by(TEAM_ID, hill_no) %>%
            summarise(wl = paste0(sum(win),"-", n() - sum(win)),
                      avg_score = mean(score),
                      avg_opp_score = mean(opp_score),
                      median_diff = median(score-opp_score)) %>%
            filter(TEAM_ID == input$team) %>%
            ungroup() %>%
            mutate(hill_no = paste0("P",as.integer(hill_no))) %>%
            rename(variable = hill_no) ->
            hp_general_table_p3
        
        hp_general_table_p1 %>%
            bind_rows(hp_general_table_p2) %>%
            bind_rows(hp_general_table_p3) %>%
            select(variable, wl, avg_score, avg_opp_score, median_diff) %>%
            mutate(across(c(3,4), signif, 3)) %>%
            DT::datatable(options = list(
                                paging = FALSE,
                              searching = FALSE,
                              dom = 't'
                          )) ->
            hp_general_table
        
        #### Recent games by map ####
        series_q %>%
            filter(A_ID %in% !!input$team | B_ID %in% !!input$team,
                   DATE >= !!input$dateRange[1],
                   DATE <= !!input$dateRange[2]) %>%
            left_join(ref_q, by = "REF_ID") %>%
            left_join(team_q, by = c("A_ID" = "TEAM_ID"))%>%
            left_join(team_q, by = c("B_ID" = "TEAM_ID")) %>%
            left_join(game_q, by = "SERIES_ID") %>%
            filter(MAP_ID %in% map_selected, MODE_ID == 1) %>%
            mutate(MATCH = paste0(TEAM_ABV.x ,
                                  " (", TEAM_A_SCORE,")",
                                  " vs ",
                                  TEAM_ABV.y,
                                  " (", TEAM_B_SCORE,")"),
                   LINK = case_when(
                       !is.na(REF_VALUE) ~ paste0('<a href=\"',
                                                        "https://youtu.be/",
                                                        REF_VALUE,
                                                        "?t=",
                                                        START_SEC,
                                                        '\"> LINK </a>'
                       ),
                       TRUE ~ NA
                   )) %>%
            select(DATE, MATCH, LINK) %>%
            collect() %>%
            mutate(DATE = lubridate::as_date(as.Date(DATE))) %>%
            arrange(desc(DATE)) %>%
            DT::datatable(escape = FALSE,
                          options = list(
                              searching = FALSE,
                              pageLength = 5
                          )) ->
            recent_hp
        
        ### Return the reactives ####
        
        return(list('hp_win_box' = hp_win_box,
                    'rolling_score_plot' = rolling_score_plot,
                    hp_general_table = hp_general_table,
                    recent_hp = recent_hp,
                    all_hp_maps_table = all_hp_maps_table))
    })
    
    output$all_hp_maps_table <- renderTable({
        data_for_hp()$all_hp_maps_table
    })
    
    output$hardpoint_score <- renderValueBox({
        data_for_hp()$hp_win_box
    })
    
    output$hp_general_table <- renderDT({
        data_for_hp()$hp_general_table
    })
    
    output$rolling_score_plot_hp <- renderPlot({
        data_for_hp()$rolling_score_plot
    })
    
    output$recent_hp <- renderDataTable({
        if(input$hp_select != 1) {
            data_for_hp()$recent_hp
        }
    })
    
    
    
    
    ## CONTROL TAB
    data_for_ctl <- reactive({
        if(input$ctl_select==1) {
            map_selected <- c(43,44,49)
        } else {
            map_selected <- input$ctl_select
        }
        
        all_ctl_maps_table <- 
            getFilteredGames(team_ = input$team,
                             date_ = input$dateRange,
                             mode_ = c(3)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            left_join(map_q %>% collect(), by = 'MAP_ID') %>%
            group_by(MAP_NAME) %>%
            summarise(w = sum(win, na.rm = T),
                      l = n() - w)
        
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(3)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(win_pct = mean(win, na.rm = T)) %>%
            pull(win_pct) ->
            inRangeWinPct
        getFilteredGames(team_ = input$team,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(3)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(win_pct = mean(win, na.rm = T)) %>%
            pull(win_pct) ->
            overallWinPct
        
        diff = inRangeWinPct - overallWinPct
        ctl_win_box_col = color_scale(diff)
        valueBox(value = scales::label_percent()(inRangeWinPct),
                 subtitle = paste0("Map Win (",ifelse(diff >= 0, "+", ""),
                                   scales::label_percent(accuracy = 0.01)(diff)," vs YTD)"),
                 icon = icon(case_when(
                     diff > 0 ~ "angle-up",
                     diff == 0 ~ "grip-lines",
                     diff < 0 ~ "angle-down"
                 )),
                 color = ctl_win_box_col
        ) ->
            ctl_win_box
        
        ## Get win% by side
        getFilteredGames(team_ = NA,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(3)) %>%
            getControlScores() %>%
            filter(round < 5, MAP_ID %in% map_selected) %>%
            ungroup() %>%
            group_by(TEAM_ID, offense) %>%
            summarise(wins = sum(gain),
                      n = n()) ->
                off_def_win
        o_win <- off_def_win %>%
            ungroup() %>%
            filter(offense == TRUE, TEAM_ID == input$team) %>%
            mutate(win_pct = wins/n) %>%
            pull(win_pct)
        league_o_win <- off_def_win %>%
            ungroup() %>%
            filter(offense == TRUE, TEAM_ID != 6) %>%
            summarise(win_pct = sum(wins)/sum(n)) %>%
            pull(win_pct)
        off_diff = o_win - league_o_win
        ctl_off_box_col = color_scale(off_diff)
        valueBox(value = scales::label_percent()(o_win),
                 subtitle = paste0("Off Win% (",ifelse(off_diff >= 0, "+", ""),
                                   scales::label_percent(accuracy = 0.01)(off_diff)," vs League)"),
                 icon = icon(case_when(
                     off_diff > 0 ~ "angle-up",
                     off_diff == 0 ~ "grip-lines",
                     off_diff < 0 ~ "angle-down"
                 )),
                 color = ctl_off_box_col
        ) ->
            ctl_off_box
        
        ## Def win rates
        d_win <- off_def_win %>%
            ungroup() %>%
            filter(offense == F, TEAM_ID == input$team) %>%
            mutate(win_pct = wins/n) %>%
            pull(win_pct)
        league_d_win <- off_def_win %>%
            ungroup() %>%
            filter(offense == F, TEAM_ID != 6) %>%
            summarise(win_pct = sum(wins)/sum(n)) %>%
            pull(win_pct)
        def_diff = d_win - league_o_win
        ctl_def_box_col = color_scale(def_diff)
        valueBox(value = scales::label_percent()(d_win),
                 subtitle = paste0("Def Win% (",ifelse(def_diff >= 0, "+", ""),
                                   scales::label_percent(accuracy = 0.01)(def_diff)," vs League)"),
                 icon = icon(case_when(
                     def_diff > 0 ~ "angle-up",
                     def_diff == 0 ~ "grip-lines",
                     def_diff < 0 ~ "angle-down"
                 )),
                 color = ctl_def_box_col
        ) ->
            ctl_def_box
        
        ## off/def plot 10-game
        getFilteredGames(team_ = NA,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(3)) %>%
            getControlScores() %>%
            filter(round < 5, MAP_ID %in% map_selected) %>%
            ungroup() %>%
            filter(TEAM_ID == input$team) %>%
            group_by(TEAM_ID, offense) %>%
            mutate(rn = 1:n(),
                   win_10 = zoo::rollmeanr(gain, k = 10, fill = NA)) %>%
            slice_tail(n = 50) ->
            rolling_off_def
        rolling_off_def %>%
            ggplot() +
            geom_line(aes(rn, win_10, color = offense), size = 1)+
            scale_color_discrete(labels = c("TRUE"="OFF","FALSE" = "DEF"))+
            lims(y = c(0,1)) +
            labs(x = 'Round', y = "Win% (no round 5s)")+
            theme_bw() +
            theme(panel.border = element_rect(color = '#3d1866', fill = NA),
                  axis.text = element_text(face = 'bold', color = '#3d1866', size = 16),
                  axis.title = element_text(face = 'bold', color = '#3d1866', size = 17),
                  legend.text = element_text(face = 'bold', color = '#3d1866', size = 12),
                  title = element_text(face = 'bold', color = '#3d1866', size = 17),
                  text = element_text(family = 'mono'),
                  legend.title = element_blank()) ->
            control_round_scores
        
        #### General Table of records #####
        
        ## Get W-L of all selected
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(3)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(
                wl = paste0(sum(win),"-", n() - sum(win)),
                avg_score = mean(score),
                avg_opp_score = mean(opp_score),
                median_diff = mean(score-opp_score)
            ) %>%
            ungroup() %>%
            mutate(variable = "Overall") ->
            ctl_general_table_p1
        ## by side
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(3)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            group_by(side) %>%
            summarise(
                wl = paste0(sum(win),"-", n() - sum(win)),
                avg_score = mean(score),
                avg_opp_score = mean(opp_score),
                median_diff = mean(score-opp_score)
            ) %>%
            rename(variable = side) ->
            ctl_general_table_p2
        
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(3)) %>%
            getControlScores() %>%
            filter(round < 5, MAP_ID %in% map_selected) %>%
            ungroup() %>%
            filter(TEAM_ID == input$team) %>%
            group_by(TEAM_ID, offense)%>%
            summarise(wl = paste0(sum(win),"-", n() - sum(win)),
                      avg_score = mean(gain),
                      avg_opp_score = mean(opp_score),
                      median_diff = mean(gain-opp_score)) %>%
            ungroup() %>%
            mutate(offense = ifelse(offense, "OFF", "DEF")) %>%
            rename(variable = offense) ->
            ctl_general_table_p3
        
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(3)) %>%
            getControlScores() %>%
            filter(round == 5, MAP_ID %in% map_selected) %>%
            ungroup() %>%
            filter(TEAM_ID == input$team) %>%
            ungroup() %>%
            summarise(wl = paste0(sum(win),"-", n() - sum(win)),
                      avg_score = mean(gain),
                      avg_opp_score = mean(opp_score),
                      median_diff = mean(gain-opp_score)) %>%
            ungroup() %>%
            mutate(variable = "Round 5") ->
            ctl_general_table_p4
        
        ctl_general_table_p1 %>%
            bind_rows(ctl_general_table_p2) %>%
            bind_rows(ctl_general_table_p3) %>%
            bind_rows(ctl_general_table_p4) %>%
            select(variable, wl, avg_score, avg_opp_score, median_diff) %>%
            mutate(across(c(3,4,5), signif, 3)) %>%
            DT::datatable(options = list(
                paging = FALSE,
                searching = FALSE,
                dom = 't'
            )) ->
            ctl_general_table
        
        #### Recent games by map ####
        series_q %>%
            filter(A_ID %in% !!input$team | B_ID %in% !!input$team,
                   DATE >= !!input$dateRange[1],
                   DATE <= !!input$dateRange[2]) %>%
            left_join(ref_q, by = "REF_ID") %>%
            left_join(team_q, by = c("A_ID" = "TEAM_ID"))%>%
            left_join(team_q, by = c("B_ID" = "TEAM_ID")) %>%
            left_join(game_q, by = "SERIES_ID") %>%
            filter(MAP_ID %in% map_selected, MODE_ID == 3) %>%
            mutate(MATCH = paste0(TEAM_ABV.x ,
                                  " (", TEAM_A_SCORE,")",
                                  " vs ",
                                  TEAM_ABV.y,
                                  " (", TEAM_B_SCORE,")"),
                   LINK = case_when(
                       !is.na(REF_VALUE) ~ paste0('<a href=\"',
                                                        "https://youtu.be/",
                                                        REF_VALUE,
                                                        "?t=",
                                                        START_SEC,
                                                        '\"> LINK </a>'
                       ),
                       TRUE ~ NA
                   )) %>%
            select(DATE, MATCH, LINK) %>%
            collect() %>%
            mutate(DATE = lubridate::as_date(as.Date(DATE))) %>%
            arrange(desc(DATE)) %>%
            DT::datatable(escape = FALSE,
                          options = list(
                              searching = FALSE,
                              pageLength = 5
                          )) ->
            recent_ctl
        ## what to return reactive #####
        return(list(
                ctl_win_box = ctl_win_box,
                ctl_off_box = ctl_off_box,
                ctl_def_box = ctl_def_box,
                control_round_scores = control_round_scores,
                ctl_general_table = ctl_general_table,
                recent_ctl = recent_ctl,
                all_ctl_maps_table = all_ctl_maps_table
                )
            )
    })
    
    
    output$all_ctl_maps_table <- renderTable({
        data_for_ctl()$all_ctl_maps_table
    })
    
    output$ctl_score <- renderValueBox({
        data_for_ctl()$ctl_win_box
    })
    
    output$ctl_off_box <- renderValueBox({
        data_for_ctl()$ctl_off_box
    })
    
    output$ctl_def_box <- renderValueBox({
        data_for_ctl()$ctl_def_box
    })
    
    output$ctl_general_table <- DT::renderDT({
        data_for_ctl()$ctl_general_table
    })
    
    output$control_round_scores <- renderPlot({
        data_for_ctl()$control_round_scores
    })
    
    output$recent_ctl <- DT::renderDT({
        if(input$ctl_select != 1) {
            data_for_ctl()$recent_ctl
        }
    })
    
    
    
    ## SND TAB
    data_for_snd <- reactive({
        if(input$snd_select==1) {
            map_selected <- c(41,43,44,45,49,52)
        } else {
            map_selected <- input$snd_select
        }
        
        all_snd_maps_table <- 
            getFilteredGames(team_ = input$team,
                             date_ = input$dateRange,
                             mode_ = c(4)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            left_join(map_q %>% collect(), by = 'MAP_ID') %>%
            group_by(MAP_NAME) %>%
            summarise(w = sum(win, na.rm = T),
                      l = n() - w)
        
        
        getFilteredGames(team_ = input$team,
                         date_ = input$dateRange,
                         mode_ = c(4)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(win_pct = mean(win, na.rm = T)) %>%
            pull(win_pct) ->
            inRangeWinPct
        getFilteredGames(team_ = input$team,
                         date_ = c("2020-10-01", as.character(Sys.Date())),
                         mode_ = c(4)) %>%
            getRecords(team_ = input$team) %>%
            filter(MAP_ID %in% map_selected) %>%
            ungroup() %>%
            summarise(win_pct = mean(win, na.rm = T)) %>%
            pull(win_pct) ->
            overallWinPct
        
        diff = inRangeWinPct - overallWinPct
        valueBox(value = scales::label_percent()(inRangeWinPct),
                 subtitle = paste0("Map Win (",ifelse(diff >= 0, "+", ""),
                                   scales::label_percent(accuracy = 0.01)(diff),")"),
                 icon = icon(case_when(
                     diff > 0 ~ "angle-up",
                     diff == 0 ~ "grip-lines",
                     diff < 0 ~ "angle-down"
                 )),
                 color = "purple"
        ) ->
            snd_win_box
        return(list('snd_win_box' = snd_win_box,
                    all_snd_maps_table = all_snd_maps_table))
    })
    
    output$all_snd_maps_table <- renderTable({
        data_for_snd()$all_snd_maps_table
    })
    output$snd_score <- renderValueBox({
        data_for_snd()$snd_win_box
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
