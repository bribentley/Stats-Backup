
library(shiny)
library(ggplot2)

ui <- fluidPage(
   
   # Application title
   titlePanel("Power Graph by Bri Stephen and Coleman"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        sliderInput("N", "Number of Trials:", min=1, max=200, value=1),
        numericInput("sd", "Standard Deviation:", value = 1, min = .5, max = 30, step = .5),
        numericInput("Ho", "Null Hypothesis:", value = 10, min = 1, max = 50, step = 1),
        numericInput("Ha", "Alternative Hypothesis:", value = 13, min = 1, max = 50, step = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

server <- function (input, output){
  output$plot <- renderPlot({
    p <- ggplot(data = data.frame(x = c(input$Ho-3.6*input$sd, input$Ha+3.6*input$sd)), aes(x = x))
    dnorm_func <- function(x){
    y <- dnorm(x, input$Ha, input$sd)
    y[x < qnorm(0.95, input$Ho, input$sd)] <- NA
    return(y)
  }
  dnorm_func1 <- function(x){
    y <- dnorm(x, input$Ha, input$sd)
    y[x >= qnorm(0.95, input$Ho, input$sd)] <- NA
    return(y)
  }
  p + stat_function(fun = dnorm_func, geom = "area", fill = "blue", 
                    alpha = 0.2, n = 500) + 
    stat_function(fun = dnorm_func1, geom = "area", fill = "blue") + 
    geom_hline(yintercept = 0) + 
    stat_function(fun = dnorm, args = list(input$Ho, input$sd), n = 10000) + 
    stat_function(fun = dnorm, args = list(input$Ha, input$sd), n = 10000) + 
    theme_bw() + 
    labs(x = "", y = "")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

