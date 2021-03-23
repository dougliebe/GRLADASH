#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(odbc)
library(DBI)
library(tidyverse)
library(DT)

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
result_q <- game_q %>%
    select(GAME_ID,SERIES_ID,  TEAM_A_SCORE, TEAM_B_SCORE) %>%
    inner_join(series_q, by = "SERIES_ID") %>%
    select(GAME_ID, SERIES_ID, TEAM_A_SCORE, TEAM_B_SCORE, TEAM_A_ID, TEAM_B_ID)
map_list <- map_q %>%
    collect() %>%
    filter(MAP_ID %in% c(41,42,43,44)) %>%
    split(.$MAP_NAME) %>%
    map(~ .$MAP_ID)
createLink <- function(val) {
    sprintf('<a href="%s" target="_blank" class="btn btn-primary">Info</a>',val)
}
onStop(function() {
    dbDisconnect(con)
})
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Find Best/Worst Holds"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "map_id",
                label = "Map:",
                choices = map_list,
                selected = 41,
                inline = F
                # selectize = FALSE
            ),
            radioButtons(
                inputId = "hill_id",
                label = "Hill:",
                choices = c(1,2,3,4,5),
                selected = 1,
                inline = F
                # selectize = FALSE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("hold_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ## Create base info about all applicable games from selection
    ## at any time
    base_info <- reactive({
        temp_ <- series_q %>%
            inner_join(game_q,
                       by = "SERIES_ID") %>%
            left_join(map_q, by = "MAP_ID") %>%
            left_join(mode_q, by = "MODE_ID") %>%
            left_join(team_q,
                      by = c("TEAM_A_ID" = "TEAM_ID")) %>%
            left_join(team_q,
                      by = c("TEAM_B_ID" = "TEAM_ID"),
                      suffix = c("_A", "_B")) %>%
            left_join(ref_q, by = "REFERENCE_ID") %>%
            mutate(info = paste0(TEAM_ABV_A, " ", TEAM_A_SCORE, "-",TEAM_B_SCORE, " ", TEAM_ABV_B, " on ",
                                 MAP_NAME,", ",MODE_ALIAS)) %>%
            filter(MAP_ID == !!input$map_id, DATE >= "2020-12-01")
        temp_
    })
    output$hold_table <- DT::renderDataTable({
        # base_info() %>%
        #     collect()
        games <- base_info() %>% pull(GAME_ID)
        score_q %>%
            filter(GAME_ID %in% games) %>%
            left_join(base_info() %>%
                          select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
            # head() %>%
            collect() %>%
        # score_q %>%
        #     inner_join(base_info() %>%
        #                    select(-TEAM_A_SCORE, -TEAM_B_SCORE), by = "GAME_ID") %>%
        #     collect() %>%
        #     select(MAP_ID, GAME_ID, TEAM_A_SCORE, TEAM_B_SCORE, TIME_S) %>%
            pivot_longer(c(TEAM_A_SCORE, TEAM_B_SCORE),
                         names_to = "team",
                         values_to = "score") %>%
            group_by(GAME_ID) %>%
            filter(score >= 0,
                   # TIME_S > 4,
                   score < 250,
                   # !cumany(TEAM_A_SCORE == -1 & TEAM_B_SCORE == -1),
                   !cumany(score == 249),
                   score == cummax(score)
            ) %>%
            mutate(hill = pmax((TIME_S-5) %/% 60, 0)+1,
                   seconds_into_hill = ((TIME_S-5) %% 60)+1,
                   new_hill = ifelse(hill > lag(hill), hill, NA),
                   total_hills = ifelse(!!input$map_id %in% c(43,44, 46), 5, 4),
                   hill_no = hill-(((hill-1)%/%total_hills)*total_hills),
                   offense = ifelse((team=="TEAM_A_SCORE" & TIME_S%%2 == 1 ) |
                                        (team=="TEAM_B_SCORE" & TIME_S%%2 == 0 & TIME_S != 0), T, F)) %>%
            
            # group_by(GAME_ID, team, hill) %>%
            # mutate(rotated = (hill > 1 &
            #                       score - min(score) >= 2 &
            #                       seconds_into_hill == 10),
            #        rotated = cumany(rotated)) %>%
            filter(hill_no == !!input$hill_id) %>%
            # mutate(gain = score - lag(score),
            #        running_gain = zoo::rollsumr(gain, k = 10, fill = 0)) %>%
            mutate(TEAM = ifelse(team == "TEAM_A_SCORE", TEAM_ABV_A, TEAM_ABV_B)) %>%
            group_by(GAME_ID, DATE, TEAM, REFERENCE_VALUE, START_SEC,  hill, hill_no) %>%
            summarise(total_hill = max(score) - min(score), 
                      time_start = min(TIME_S)) %>%
            ungroup() %>%
            arrange(desc(total_hill)) %>%
            slice(1:5,(n()-4):n()) %>%
            mutate(LINK = createLink(paste0("https://youtu.be/",
                          REFERENCE_VALUE,
                          "?t=",
                          (START_SEC + time_start - 5)))) %>%
            select(DATE, TEAM, total_hill, LINK)
            # left_join(game_q %>%
            #               select(SERIES_ID, GAME_ID, START_SEC) %>% 
            #               collect(), by = "GAME_ID")
        #     group_by(GAME_ID, TIME_S) %>%
        #     mutate(broken = seconds_into_hill > 10 & running_gain < max(mean(running_gain),5) & rotated,
        #            broke = any(broken) & !broken)
    }, escape = FALSE, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
