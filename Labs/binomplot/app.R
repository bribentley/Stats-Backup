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
ui <- fluidPage(
   
   # Application title
   titlePanel("Binom Plot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         h4("Plot parameters"),
         sliderInput("p", "Probability:", min=0, max=1, value=0.5, step = 0.05),
         sliderInput("N", "Number of Trials", min=1, max=100, value=1),
         sliderInput("x", "Number of observations:", value = 500, min = 1, max = 1000),
         checkboxInput("normal", "Overlay Normal distribution", FALSE)
   ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")), 
          tabPanel("Summary", verbatimTextOutput("summary")), 
          tabPanel("Table", tableOutput("table"))
        )
   ))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

