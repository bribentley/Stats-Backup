# Shiny Test TAC 2/09/18
library(shiny)
normarea <- function (lower = -Inf, upper = Inf, m = 0, sig = 1) 
{
  Altblue <- "#CDCDED"
  Fontcol <- "#3333B3"
  opar <- par(no.readonly = TRUE)
  par(mar = c(4, 1, 4, 1))
  area <- pnorm(upper, m, sig) - pnorm(lower, m, sig)
  ra <- round(area, 4)
  x <- seq(m - 4 * sig, m + 4 * sig, length = 1000)
  y <- dnorm(x, m, sig)
  par(pty = "m")
  plot(x, y, type = "n", xaxt = "n", yaxt = "n", xlab = "", 
       ylab = "", main = "")
  mtext(substitute(paste("The area between ", lower, " and ", 
                         upper, " is ", ra), list(lower = lower, upper = upper, ra = ra)), side = 3, line = 1, font = 2, cex = 1.15)
  mtext(substitute(paste("RV ~ N (", mu == m, ", ", sigma == 
                           sig, ")"), list(m = m, sig = sig)), side = 1, line = 3, 
        col = Fontcol)
  if (lower == -Inf || lower < m - 4 * sig) {
    lower <- m - 4 * sig
  }
  if (upper == Inf || upper > m + 4 * sig) {
    upper <- m + 4 * sig
  }
  axis(1, at = c(m, lower, upper), labels = c(m, lower, upper))
  xaxis1 <- seq(lower, upper, length = 200)
  yaxis1 <- dnorm(xaxis1, m, sig)
  xaxis1 <- c(lower, xaxis1, upper)
  yaxis1 <- c(0, yaxis1, 0)
  polygon(xaxis1, yaxis1, density = -1, col = Altblue)
  lines(x, y, lwd = 2)
  lines(c(m - 4 * sig, m + 4 * sig), c(0, 0), lwd = 2)
  lines(c(lower, lower), c(0, dnorm(lower, m, sig)), lwd = 2)
  lines(c(upper, upper), c(0, dnorm(upper, m, sig)), lwd = 2)
  on.exit(par(opar))
}

ui <- fluidPage(
  p("This app finds the area between two points/values of a normal distribution.  
    The user needs to specify the two points/values and the mean and standard deviation
    of a normal distribution."),
  tags$hr(""),
  sidebarLayout(
    sidebarPanel(
      h1("Inputs"),
      numericInput(inputId = "lower", em("Lower value:"), value = -2, min = -Inf, max = 1000, step = 1),
      numericInput(inputId = "upper", strong("Upper value:"), value = 2, min = Inf, max = 1000, step = 1),
      numericInput(inputId = "m", code("Population Mean:"), value = 0, min = -1000, max = 1000, step = 1),
      numericInput(inputId = "sig", code("Population Standard Deviation:"), value = 1, min = 0.0000001, max = 100000, step = 1)
    ),
    mainPanel(
      plotOutput(outputId = "NormA")
    )
  ),
  tags$hr(""),
  tags$a(href = "https://alanarnholt.github.io", "My web page"),
  tags$hr("")
  )

server <- function(input, output){
  output$NormA <- renderPlot({
    normarea(input$lower, input$upper, input$m, input$sig)
  }) 
}

shinyApp(ui = ui, server = server)