#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)

library(shiny)
library(odbc)
library(DBI)
library(tidyverse)

source('load_session.R', local = TRUE)
onStop(function() {
    dbDisconnect(con)
})

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(
                        title = "Quick Example"
                    ),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                        tabPanel(
                            value = "map",
                            title = "Map Insights",
                            fluidRow(
                                box(height = "100px",
                                    dateRangeInput('dateRange2',
                                                   label = 'Date range input: yyyy-mm-dd',
                                                   start = Sys.Date() - 7,
                                                   end = Sys.Date()
                                    )
                                ),
                                box(height = "100px",
                                    radioButtons(
                                        inputId = "maps",
                                        label = "Examine Map:",
                                        choices = map_list,
                                        inline = T,
                                        selected = 42
                                    )
                                )
                            ),
                            fluidRow(
                                box(width = 12,
                                    plotOutput("map_insights")
                                )
                            )
                            
                        )
                    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data_with_date <- reactive({
        series_q %>%
            filter(DATE >= lubridate::as_date(!!input$dateRange2[1]),
                   DATE <= lubridate::as_date(!!input$dateRange2[2])
            )
    })
    
    ## Create base info about all applicable games from selection
    ## at any time
    base_info <- reactive({
        temp_ <- data_with_date() %>%
            inner_join(game_q,
                       by = "SERIES_ID") %>%
            filter(MODE_ID == 1) %>%
            left_join(map_q, by = "MAP_ID") %>%
            # left_join(mode_q, by = "MODE_ID") %>%
            left_join(team_q,
                      by = c("TEAM_A_ID" = "TEAM_ID")) %>%
            left_join(team_q,
                      by = c("TEAM_B_ID" = "TEAM_ID"),
                      suffix = c("_A", "_B"))
        temp_ <- temp_ %>%
            filter(MAP_ID == !!input$maps)
        temp_
    })
    
    score_info <- reactive({
        temp_ <- score_q %>%
            inner_join(base_info() %>% 
                          filter(MAP_ID != 46) %>%
                          mutate(TEAM_A_WIN = TEAM_A_SCORE > TEAM_B_SCORE) %>%
                          select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
            select(SERIES_ID, GAME_ID,MAP_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S, TEAM_A_WIN) %>%
            collect() %>%
            pivot_longer(c(-SERIES_ID, -GAME_ID, -MAP_ID, -TIME_S, -TEAM_A_WIN), 
                         names_to = 'team',
                         values_to = 'score') %>%
            left_join(series_q %>%
                          select(-REFERENCE_ID, -DATE) %>%
                          collect(), by = "SERIES_ID") %>%
            mutate(team_id = ifelse(team == "TEAM_A_SCORE", TEAM_A_ID, TEAM_B_ID)) %>%
            group_by(GAME_ID) %>%
            filter(score >= 0,
                   TIME_S > 4, 
                   score < 250,
                   # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
                   !cumany(score == 249),
                   !is.na(MAP_ID)
            ) %>%
            filter(max(score) > 200) %>%
            group_by(GAME_ID, team) %>%
            mutate(gain = score - lag(score),
                   hill = pmax((TIME_S-5) %/% 60, 0)+1,
                   seconds_into_hill = ((TIME_S-5) %% 60)+1,
                   new_hill = ifelse(hill > lag(hill), hill, NA),
                   total_hills = ifelse(MAP_ID %in% c(43,44, 46), 5, 4),
                   hill_no = hill-(((hill-1)%/%total_hills)*total_hills),
                   WIN = ifelse((team=="TEAM_A_SCORE" & TEAM_A_WIN) | 
                                    (team=="TEAM_B_SCORE" & !TEAM_A_WIN), T, F)) %>%
            filter(gain >= 0, gain < 5) %>%
            group_by(GAME_ID, team, hill) %>%
            mutate(rotated = (hill > 1 & 
                                  score - min(score) >= 5 & 
                                  seconds_into_hill == 10),
                   rotated = cumany(rotated)) %>%
            group_by(GAME_ID, team) %>%
            mutate(running_gain = zoo::rollsumr(gain, k = 10, fill = 0)) %>%
            group_by(GAME_ID, TIME_S) %>%
            mutate(broken = seconds_into_hill > 10 & running_gain < max(mean(running_gain),5) & rotated,
                   broke = any(broken) & !broken)
    })
    
    output$map_insights <- renderPlot({
        score_info() %>% 
            group_by(GAME_ID, MAP_ID,team, hill, hill_no) %>%
            summarise(score = sum(gain)) %>%
            group_by(GAME_ID, MAP_ID,  hill, hill_no) %>%
            summarise(top_score = max(score)) %>%
            group_by(MAP_ID, hill_no) %>%
            summarise(m= median(top_score),
                      n = n()) %>%
            left_join(map_q %>% collect(), by = "MAP_ID") %>%
            ggplot(aes(hill_no, m))+geom_col()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
