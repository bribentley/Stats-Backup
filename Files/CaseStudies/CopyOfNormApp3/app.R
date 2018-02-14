library(colourpicker)

ui <- fluidPage(
  # Superhero css from http://bootswatch.com/
  theme = "bootstrap.css",
  tags$h2("Normal Distribution Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "MU", label = "Mean:", value = 0, min = -Inf, max = Inf),
      numericInput(inputId = "SIGMA", label = "Standard Deviation:", value = 1, min = 0.0000001, max = Inf),
      numericInput(inputId = "MIN", label = "Lower:", value = -2, min = -Inf, max = Inf),
      numericInput(inputId = "MAX", label = "Upper:", value = 2, min = -Inf, max = Inf),
      colourInput(inputId = "COLOR", label = "Color:", value = "hotpink"),
      numericInput(inputId = "ALPHA", label = "Intensity:", value = 0.4, min = 0.05, max = 1),
      downloadButton("report", "Generate report")
    ),
    mainPanel(
      plotOutput("NG", height = "510px", width = "510px")
    )
  ))


server <- function(input, output){
  library(ggplot2)
  output$NG <- renderPlot({
    DF <- data.frame(x=c(input$MU - 3.5*input$SIGMA, input$MU + 3.5*input$SIGMA))
    abN <- round(pnorm(input$MAX, input$MU, input$SIGMA) - pnorm(input$MIN, input$MU, input$SIGMA),4) 
    p <- ggplot(data = DF, aes(x = x)) 
    limitRange <- function(min = input$MIN, max = input$MAX) {
      function(x) {
        y <- dnorm(x, input$MU, input$SIGMA)
        y[x < input$MIN | x > input$MAX] <- NA
        return(y)
      }
    }
    SDB <- -3:3*input$SIGMA + input$MU
    df <- data.frame(x1 = SDB, x2 = SDB, y1 = rep(0, length(SDB)), y2 = dnorm(SDB, input$MU, input$SIGMA))
    p + stat_function(fun = dnorm, args = list(input$MU, input$SIGMA)) +
      stat_function(fun = limitRange(min = input$MIN, max = input$MAX),
                    geom = "area", fill = input$COLOR, alpha = input$ALPHA, n = 500) + 
      stat_function(fun = dnorm, args = list(input$MU, input$SIGMA), size = 1) +
      theme_bw(base_size = 18) + 
      labs(x = paste("X ~ N(", input$MU,",",input$SIGMA,")"), y = "", title = paste("The area between", input$MIN, "and", input$MAX, "is", abN)) +
      scale_x_continuous(breaks=SDB) + 
      geom_segment(data = df, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = "dashed", color = "gray50") 
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    # For WORD output, change this to "report.docx"
    # For HTML output, change this to "report.html"
    # Make sure the YAML in the report.Rmd file is output: word_document,
    # output: pdf_document, html_document, for word, pdf, and html, respectively
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(mu = input$MU, sigma = input$SIGMA, min = input$MIN, max = input$MAX, color = input$COLOR, alpha = input$ALPHA)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}
shinyApp(ui = ui, server = server, options = list(height = 600, width = 700))