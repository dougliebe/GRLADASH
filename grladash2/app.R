#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shinydashboard)

library(shiny)
library(odbc)
library(DBI)
library(tidyverse)

source('all_sessions.R', local = TRUE)
onStop(function() {
    dbDisconnect(con)
})
ui <- dashboardPage(skin = "purple",
              dashboardHeader(
                  title = "GRLADASH"
              ),
              dashboardSidebar(
                  dateInput(inputId = 'date',
                           label = 'Date input: yyyy-mm-dd',
                           value = Sys.Date()-1
                  ),
                  selectInput(
                      inputId = "teams",
                      label = "Opponent:",
                      choices = "",
                      multiple = F,
                      # selected = "DL",
                      selectize = F
                  ),
                  radioButtons("mode_input",
                               "Mode:",
                               choices = mode_list,
                               selected = 1),
                  radioButtons(
                      "map_input",
                      "Map:",
                      choices = map_list,
                      selected = 43
                  )
              ),
              dashboardBody(
                  box(width = 6,
                        fluidRow(
                  #       tags$style("#link font-size:50px;"),
                  #       uiOutput('link'),
                  #       # ),
                  #       # fluidRow(
                  #       # column(cwidth = 6,
                        plotOutput("score_graph"),
                        tableOutput("test")
                  #       # )
                        )
                  )
              )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ## first, filter out only series' from the selected date
    
    series_with_date <- reactive({
        series_q %>%
            filter(DATE == lubridate::as_date(!!input$date))
    })
    # Update filters to only show teams and games
    # from higher level filters
    observeEvent(input$date, {
        # update teams for only the selected date
        teams_unq <- series_with_date() %>%
            left_join(team_q,
                      by = c("TEAM_A_ID" = "TEAM_ID")) %>%
            left_join(team_q,
                      by = c("TEAM_B_ID" = "TEAM_ID"),
                      suffix = c("_A", "_B")) %>%
            select(TEAM_ABV_A, TEAM_ABV_B, TEAM_A_ID, TEAM_B_ID) %>%
            collect()
        team_choices <- as.list(teams_unq %>%
                                    select(TEAM_A_ID, TEAM_B_ID) %>%
                                    unlist(use.names = F) %>%
                                    unique()) %>%
            set_names(teams_unq %>%
                          select(TEAM_ABV_A, TEAM_ABV_B) %>%
                          unlist(use.names = F) %>%
                          unique())
        updateSelectInput(session, 'teams',
                          choices = team_choices %>% unlist())

    })
    
    get_all_data <- reactive({
        # only data from sidebar
        map_mode_data <- 
            series_with_date() %>%
            inner_join(game_q, by = "SERIES_ID") %>%
            filter(
                MODE_ID == !!input$mode_input,
                MAP_ID == !!input$map_input,
                TEAM_A_ID == !!input$teams |
                    TEAM_B_ID == !!input$teams
                ) %>%
            collect() %>%
            pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
                         names_to = 'team',
                         values_to = 'score') %>%
            mutate(TEAM_ID = ifelse(team == "TEAM_A_SCORE", TEAM_A_ID, TEAM_B_ID)) %>%
            mutate(PERIOD = "GAME")
        # only LAG data
        map_mode_ytd <- 
            game_q %>%
            left_join(series_q, by = "SERIES_ID") %>%
            filter(
                MODE_ID == !!input$mode_input,
                MAP_ID == !!input$map_input,
                TEAM_A_ID == 6 |
                    TEAM_B_ID == 6
            ) %>%
            collect()
        avg_ytd <-
            map_mode_ytd %>%
            pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
                         names_to = 'team',
                         values_to = 'score') %>%
            mutate(opp = (team == "TEAM_A_SCORE" & TEAM_A_ID != 6) | 
                       (team == "TEAM_B_SCORE" & TEAM_B_ID != 6)) %>%
            group_by(MAP_ID, MODE_ID, opp) %>%
            summarise(score = mean(score,na.rm = T),
                      n = n(),
                      PERIOD = "YTD",
                      TEAM_ID = 6) %>% 
            pivot_longer(c(score), names_to = 'stat') %>%
            pivot_wider(names_from = c('stat','opp'), values_from = c('value'))
        return(list(map_mode_data = map_mode_data, 
                    avg_ytd = avg_ytd))
    })
    
    
    output$score_graph <-
        renderPlot({
            combined_data <-
                get_all_data()$map_mode_data %>%
                bind_rows(get_all_data()$avg_ytd) %>%
                mutate(score = coalesce(score, score_FALSE)) %>%
                left_join(team_q %>% collect(), by = "TEAM_ID") %>%
                select(PERIOD, TEAM_ABV, score)
            ggplot(data = combined_data) +
                geom_col(aes(TEAM_ABV, score, fill = TEAM_ABV, alpha = PERIOD),
                         position = "dodge")+
                scale_fill_manual(values = group_colors)
                
        })
    output$test <- renderTable({
        get_all_data()$map_mode_data %>%
            bind_rows(get_all_data()$avg_ytd) %>%
            mutate(score = coalesce(score, score_FALSE)) %>%
            left_join(team_q %>% collect(), by = "TEAM_ID") %>%
            select(DATE, TEAM_ABV, score)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
