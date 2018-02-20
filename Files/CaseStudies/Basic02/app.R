# Load the shiny package
library(shiny)
library(PASWR2)

# Define UI for the application
ui <- fluidPage(
  tags$header("Describe what this app does here."),
  tags$hr(""),
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h4("Plot parameters"),
      textInput("title", "Plot title", "Water hardness versus mortality"),
      numericInput("num", "Number of locations to show", 30, 1, nrow(HARDWATER)),
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
    plot(HARDWATER[1:input$num, "mortality"], HARDWATER[1:input$num, "hardness"],  
         main = input$title, cex = input$size, xlab = "mortality", ylab = "hardness",
         col = "lightblue", pch = 19)
  })
  output$table <- renderTable({
    HARDWATER[1:input$num, ]
  })
}

# Run the application
shinyApp(ui = ui, server = server)


