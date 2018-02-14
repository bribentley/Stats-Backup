# Load the shiny package
library(shiny)
library(ggplot2)
cisimg <- function(samples = 100,
                   n = 30,
                   mu = 0,
                   sigma = 1,
                   Pi = 0.5,
                   conf.level = 0.95,
                   type = c("Mean", "Var", "Pi")
) 
{
  if (!require(ggplot2)) {
    stop("Can't continue can't load ggplot2")
  }
  alpha <- 1 - conf.level
  CL <- conf.level * 100
  # round n and N in case user entered a non integer
  n <- round(n)
  N <- round(samples)
  type <- match.arg(type)
  if (!missing(type) && length(type) != 1 || is.na(type))
    stop("type must be one \"Mean\", \"Var\", \"Pi\"")
  # check for nonsense input
  if (N <= 0 || n <= 1)
    stop("Number of random samples and sample size must both be at least 2")
  if (!missing(conf.level) &&
      (length(conf.level) != 1 || !is.finite(conf.level) ||
       conf.level <= 0 || conf.level >= 1))
    stop("'conf.level' must be a single number between 0 and 1")
  if (sigma <= 0)
    stop("Variance must be a positive value")
  #
  if (type == "Pi" && (n * Pi < 10 || n * (1 - Pi) < 10))
    warning("Coverage probability likely less than confidence level")
  #
  if (type == "Mean") {
    xbar <- numeric(samples)
    #
    for (i in 1:samples) {
      xbar[i] <- mean(rnorm(n, mu, sigma))
    }
    ll <- xbar - qnorm(1 - alpha / 2) * sigma / sqrt(n)
    ul <- xbar + qnorm(1 - alpha / 2) * sigma / sqrt(n)
    notin <- sum((ll > mu) + (ul < mu))
    percentage <- round((1 - notin / N) * 100, 2)
    data <- data.frame(xbar = xbar, ll = ll, ul = ul)
    data$samplenumb <- factor(as.integer(rownames(data)))
    data$correct <- "Includes"
    data$correct[data$ul < mu] <- "Too Low"
    data$correct[data$ll > mu] <- "Too High"
    #
    bestfitM <- function(NN = N) {
      list(scale_y_continuous(limits = c(min(min(ll), (mu - 2 * sigma)), 
                                         max(max(ul), (mu + 2 * sigma)))),
           if (NN >= 21)
             scale_x_discrete(breaks = seq(0, NN, by = round(NN / 10))))
    }
    
    p <- ggplot(data, aes(y = xbar, x = samplenumb)) +
      geom_point(size = 0.75) +
      geom_hline(yintercept = mu) +
      geom_errorbar(aes(ymin = ll, ymax = ul, color = correct), width = 0.5) +
      labs(title = bquote(atop(paste(.(CL), "% confidence intervals for ",
                                     mu," for ", .(N), " random samples of size "),
                               paste(.(n), " generated from a normal distribution where ",
                                     mu, " = ", .(mu), " and ", sigma , " = ", .(sigma))
      )),
      subtitle = bquote(paste("Note: ", .(percentage), 
                              "% of the confidence intervals contain ", 
                              mu, " = ", .(mu))
      ),
      y = expression("Sample mean" ~ (bar(X))),
      x = bquote("Random samples of size" ~ .(n))
      ) +
      bestfitM() +
      scale_color_manual(values = c("green", "red", "blue")) +
      guides(color = guide_legend(title = "Confidence\nIntervals")) +
      theme_bw() + 
      theme(plot.title = element_text(hjust=0.5)) + 
      theme(plot.subtitle = element_text(hjust=0.5))
    
    print(p)
  }
  else if (type == "Var") {
    S2 <- numeric(samples)
    #
    for (i in 1:samples) {
      S2[i] <- var(rnorm(n, mu, sigma))
    }
    ll <- (n - 1) * S2 / qchisq(1 - alpha / 2, n - 1)
    ul <- (n - 1) * S2 / qchisq(alpha / 2, n - 1)
    
    notin <- sum((ll > sigma ^ 2) + (ul < sigma ^ 2))
    percentage <- round((1 - notin / N) * 100, 2)
    data <- data.frame(ll = ll, ul = ul)
    data$samplenumb <- factor(as.integer(rownames(data)))
    data$correct <- "Includes"
    data$correct[data$ul < sigma ^ 2] <- "Too Low"
    data$correct[data$ll > sigma ^ 2] <- "Too High"
    #
    bestfitV <- function(NN = N) {
      list(scale_y_continuous(),
           if (NN >= 21)
             scale_x_discrete(breaks = seq(0, NN, by = round(NN / 10))))
    }
    
    p <- ggplot(data, aes(y = S2, x = samplenumb)) +
      geom_point(size = 0.75) +
      geom_hline(yintercept = sigma ^ 2) +
      geom_errorbar(aes(ymin = ll, ymax = ul, color = correct), width = 0.5) +
      labs(title = bquote(atop(paste(.(CL), "% confidence intervals for ",
                                     sigma ^ 2, " for ", .(N), " random samples of size "),
                               paste(.(n), " generated from a normal distribution where ",
                                     mu, " = ", .(mu), " and ", sigma , " = ", .(sigma))
      )),
      subtitle = bquote(paste("Note: ", .(percentage), "% of the confidence intervals contain ",
                              sigma ^ 2, " = ", .(sigma ^ 2))),
      y = expression("Sample variance" ~ (S ^ 2)),
      x = bquote("Random samples of size" ~ .(n))) +
      bestfitV() +
      scale_color_manual(values = c("green", "red", "blue")) +
      guides(color = guide_legend(title = "Confidence\nIntervals")) +
      theme_bw() + 
      theme(plot.title = element_text(hjust=0.5)) + 
      theme(plot.subtitle = element_text(hjust=0.5))
    
    print(p)
  }
  else if (type == "Pi") {
    X <- rbinom(samples, n, Pi)
    p <- X / n
    ll <- p - qnorm(1 - alpha / 2) * sqrt((p * (1 - p)) / n)
    ul <- p + qnorm(1 - alpha / 2) * sqrt((p * (1 - p)) / n)
    
    notin <- sum((ll > Pi) + (ul < Pi))
    percentage <- round((1 - notin / N) * 100, 2)
    data <- data.frame(p = p, ll = ll, ul = ul)
    data$samplenumb <- factor(as.integer(rownames(data)))
    data$correct <- "Includes"
    data$correct[data$ul < Pi] <- "Too Low"
    data$correct[data$ll > Pi] <- "Too High"
    #
    bestfitP <- function(NN = N) {
      list(scale_y_continuous(),
           if (NN >= 21)
             scale_x_discrete(breaks = seq(0, NN, by = round(NN / 10))))
    }
    
    p <- ggplot(data, aes(y = p, x = samplenumb)) +
      geom_point(size = 0.75) +
      geom_hline(yintercept = Pi) +
      geom_errorbar(aes(ymin = ll, ymax = ul, color = correct), width = 0.5) +
      bestfitP() +
      labs(
        title = bquote(atop(paste(.(CL), "% asymptotic confidence intervals for ",
                                  pi, " for ", .(N), " random samples "),
                            paste("generated from a binomial distribution where ",
                                  n , " = ", .(n) , " and ", pi , " = ", .(Pi))
        )),
        subtitle = bquote(paste("Note: ", .(percentage),
                                "% of the confidence intervals contain ",
                                pi, " = ", .(Pi))),
        y = expression("Sample proportion" ~ (p)),
        x = bquote("Random samples of size" ~ .(n))) +
      scale_color_manual(values = c("green", "red", "blue")) +
      guides(color = guide_legend(title = "Confidence\nIntervals")) +
      theme_bw() + 
      theme(plot.title = element_text(hjust=0.5)) + 
      theme(plot.subtitle = element_text(hjust=0.5))
    
    print(p)
  }
}  





# Define UI for the application
ui <- fluidPage(
  withMathJax(),
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h3(em("Select Parameters")),
      radioButtons(inputId = "type", label = "Select parameter for Confidence Interval", choices = c("\\(\\mu \\)" = "Mean",
                                                                                                     "\\(\\sigma^2 \\)" = "Var",
                                                                                                     "\\(\\pi \\)" = "Pi"),
                   selected = "Mean"),
      tags$hr(""),
      numericInput(inputId = "samples", label = "Number of Samples", value = 100, min = 2, max = 500, step = 1),
      numericInput(inputId = "n", label = "Sample size", value = 25, min = 2, max = 10000, step = 1),
      numericInput(inputId = "mu", label = "Population mean", value = 0, min = -100000, max = 100000, step = 1),
      numericInput(inputId = "sigma", label = "Population standard deviation", value = 1, min = 0.001, max = 100000, step = 1),
      sliderInput("Pi", "Population proportion of success", 0.001, 0.999, 0.5, 0.1),
      numericInput(inputId = "conf.level", label = "Confidence level", value = 0.95, min = 0.500, max = 0.999, step = 0.001)
    ),
    # Add a main panel around the plot and table
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
          cisimg(samples = input$samples,
                 n = input$n,
                 mu = input$mu,
                 sigma = input$sigma,
                 Pi = input$Pi,
                 conf.level = input$conf.level,
                 type = input$type)    
      })
    }

# Run the application
shinyApp(ui = ui, server = server)