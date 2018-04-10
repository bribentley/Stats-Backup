
library(shiny)
library(binom)

ui <- fluidPage(
  
  headerPanel("Binomial demo"),
  
  sidebarPanel(
    sliderInput("N", "Number of Trials:", 
                min=1, max=200, value=1),
  

    sliderInput(inputId = "conflevel", label =  "Confidence Level:", 
                min = .8, max = .999, value = .95),
    
    selectInput(inputId = "meth", label = "Method:", 
                choices = c("Wilson" = "Wilson", "Asymp" = "Asymp", "Agresti-Coull" = "AC", "Exact" = "Exact"), 
                selected = "Wilson")
  ),
  
  mainPanel(
    plotOutput("plot")
    )
  )

server <- function(input, output){
    x<-reactive({input$meth})
    output$plot <- renderPlot({
    if (x()=="Wilson"){
      binom.plot(n = input$N, method = binom.wilson, np = 1000, 
                 conf.level = input$conflevel)}
      else{
      if (x()=="Asymp"){
        binom.plot(n = input$N, method = binom.asymp, np = 1000, 
                  conf.level = input$conflevel)}
      else{
      if (x()=="AC"){
        binom.plot(n = input$N, method = binom.agresti.coull, np = 1000, 
                   conf.level = input$conflevel)}
    
      else{
      if (x()=="Exact"){
        binom.plot(n = input$N, method = binom.exact, np = 1000, 
                    conf.level = input$conflevel)}
      else{}}}}
  })
}

shinyApp(ui = ui, server = server)
