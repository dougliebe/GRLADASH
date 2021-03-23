#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

source('all_sessions.R', local = TRUE)
onStop(function() {
    dbDisconnect(con)
})
dashboardPage(skin = "purple",
    dashboardHeader(
        title = "GRLADASH"
        ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        # textOutput("current"),
        # tableOutput("mtcars"),
        tabsetPanel(
            id = "tabs",
            tabPanel(
                value = 'series',
                title = "Series/Game",
                fluidRow( 
                    box(width = 12,
                        splitLayout(
                            dateInput('date',
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
                            selectInput(
                                inputId = "game",
                                label = "Select a specific game:",
                                choices = c("ALL" = 0),
                                selected = "ALL",
                                multiple = F,
                                selectize = FALSE
                            )
                        )
                    
                )
                ),box(width = 12,
                    # fluidRow(
                        tags$style("#link font-size:50px;"),
                        uiOutput('link'),
                    # ),
                    # fluidRow(
                        # column(cwidth = 6,
                            plotOutput("score_graph")
                        # )
                    # )
                )
            ),
            tabPanel(
                value = 'trends',
                title = "Trends",
                fluidRow(style="z-index:1002;",
                    box(width = 12,
                        splitLayout(
                            dateRangeInput('dateRange',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = Sys.Date() - 7,
                                           end = Sys.Date()
                            ),
                            checkboxGroupInput(
                                inputId = "mode_id",
                                label = "Modes:",
                                choices = mode_list,
                                selected = 1,
                                inline = T
                                # selectize = FALSE
                            )
                        )
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        tableOutput('records'),
                        tableOutput('sideRecord')
                    )
                )
            )
        )
    )
)
