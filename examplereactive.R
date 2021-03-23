library(shiny)
library(ggplot2)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click")
)

server <- function(input, output) {
  plot_data <- reactiveValues(trigger = 0, x = NA, y = NA)
  
  observe({
    req(input$plot_click)
    isolate(plot_data$trigger <- plot_data$trigger + 1)
    plot_data$x <- input$plot_click$x
    plot_data$y <- input$plot_click$y
  })
  
  output$plot1 <- renderPlot({
      ggplot() + geom_point(aes(x = plot_data$x, y = plot_data$y), size = 5, shape = 19)
  })
}

shinyApp(ui, server)

