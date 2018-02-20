
library(shiny)

ui <- fluidPage(
  
  # Application title
  headerPanel("Binomial demo"),
  
  sidebarPanel(
    sliderInput("N", "Number of Trials", 
                min=1, max=100, value=1),
    
    selectInput(inputId = "conflevel", label =  "Confidence Level", choices = c(".80", ".90", ".95", ".99"), selected = ".95")
    
  ),
  
  mainPanel(
    plotOutput("plot"),
    tableOutput("table")
  )
)

server <- function(input, output){
  output$plot <- renderPlot({
    binom.plot(input$N, np = 500, input$conflevel)
  })
}

shinyApp(ui = ui, server = server)