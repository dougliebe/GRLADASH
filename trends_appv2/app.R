## app.R ##
library(shiny)
library(shinydashboard)
source('base_funcs.R', local = T)
onStop(function() {
    dbDisconnect(con)
})
ui <- dashboardPage(
    dashboardHeader(title = "Team Scores"),
    dashboardSidebar(
        sidebarMenu(
            selectInput('team',
                        label = "Choose Team:",
                        choices = team_list,
                        multiple = F,
                        selected = 6),
            dateRangeInput('dateRange',
                           label = 'Date Range:',
                           start = "2020-10-01",
                           end = Sys.Date()
            )
        )
    ),
    dashboardBody(
        fluidRow(
            ## Overall records
            box(
                title = "Overall",
                tableOutput('overall')
            ),
            ## Records by mode
            tabBox(
                title = "By Mode",
                id = "modeTabset",
                tabPanel("HP",tableOutput('byMode_HP')),
                tabPanel("CTL",tableOutput('byMode_CTL')),
                tabPanel("SND",tableOutput('byMode_SND'))
            )
        ),
        fluidRow(
            box(
                title = "Recent Games",
                tableOutput('recents')
            )
        )
    )
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
    ## mode tabs
    output$byMode_SND <- renderTable({
        getFilteredGames(team_ = input$team, date_ = input$dateRange, mode_ = 4) %>%
            getRecords(team_ = input$team) %>%
            group_by(MAP_ID) %>%
            summarise(w = sum(win),
                      l = n() - w,
                      score = mean(score)) %>%
            left_join(map_q %>% collect(), by = "MAP_ID") %>%
            select(MAP_NAME, w, l, score)
    })
    output$byMode_CTL <- renderTable({
        getFilteredGames(team_ = input$team, date_ = input$dateRange, mode_ = 3) ->
            games
        games %>%
            getRecords(team_ = input$team) %>%
            group_by(MAP_ID) %>%
            summarise(w = sum(win),
                      l = n() - w,
                      score = mean(score)) %>%
            left_join(map_q %>% collect(), by = "MAP_ID") %>%
            select(MAP_NAME, w, l, score) ->
            game_records
        # games %>%
        #     select(TEAM_A_ID)
        games %>%
            select(-TEAM_A_SCORE, -TEAM_B_SCORE) %>%
            left_join(score_q %>%
                          filter(TEAM_A_SCORE != 0 | TEAM_B_SCORE != 0) %>%
                          filter(GAME_ID %in% !!game$GAME_ID) %>%
                          collect(), by = "GAME_ID") %>%
            # left_join(games %>% select(GAME_ID, TEAM_A_ID, TEAM_B_ID), by = "GAME_ID")
        # %>%
            group_by(GAME_ID) %>%
            mutate(round = 1:n()) %>%
            pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
                         names_to = 'team',
                         values_to = 'score') %>%
        mutate(offense = ifelse((team=="TEAM_A_SCORE" & round%%2 == 1 ) |
                         (team=="TEAM_B_SCORE" & round%%2 == 0 & round != 0), T, F),
               TEAM_ID = ifelse(team == "TEAM_A_SCORE", TEAM_A_ID, TEAM_B_ID))  %>%
        filter(TEAM_ID == input$team) %>%
            group_by(GAME_ID, TEAM_ID, MAP_ID, offense) %>%
            mutate(gain = score - lag(score, default = 0))
        # %>%
        #     group_by(MAP_ID, offense) %>%
        #     summarise(win_rate = mean(gain, na.rm = T), 
        #               n = n())
    })
    output$byMode_HP <- renderTable({
        getFilteredGames(team_ = input$team, date_ = input$dateRange, mode_ = 1) %>%
            getRecords(team_ = input$team) %>%
            group_by(MAP_ID) %>%
            summarise(w = sum(win),
                      l = n() - w,
                      score = mean(score)) %>%
            left_join(map_q %>% collect(), by = "MAP_ID") %>%
            select(MAP_NAME, w, l, score)
    })

    ## recent games
    output$recents <- renderTable({
        
    })
}

shinyApp(ui, server)