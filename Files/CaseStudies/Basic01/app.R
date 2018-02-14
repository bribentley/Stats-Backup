# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  tags$header("This is a header!"),
  # The hr tag defines a thematic break in an HTML page
  tags$hr(""),
  # "My First Shiny App" as a primary header
  h1("My First Shiny App"),
  # "Shiny Template" as a secondary header
  h2("Shiny Template"),
  # "Let your light shine!" in italics
  em("Let your light shine"),
  # "makes me happy!" as bold text
  strong("makes me happy!")
)

# Define the server logic
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)