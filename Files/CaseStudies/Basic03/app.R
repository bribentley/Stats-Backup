# Load the shiny package
library(shiny)
library(PASWR2)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h4("Plot parameters"),
      textInput("title", "Plot title", "Water hardness versus mortality"),
      numericInput("al", "Point transparency", 0.5, 0.001, 1),
      sliderInput("size", "Point size", 1, 5, 2, 0.5)
    ),
    # Add a main panel around the plot and table
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(data = HARDWATER, aes(x = hardness, y = mortality)) + 
      geom_point(size = input$size, alpha = input$al, color = "blue") +
      theme_bw() + 
      labs(title = input$title) + 
      theme(plot.title = element_text(hjust=0.5))
  })
  output$table <- renderTable({
    HARDWATER
  })
}

# Run the application
shinyApp(ui = ui, server = server)