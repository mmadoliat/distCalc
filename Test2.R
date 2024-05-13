library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Distribution Calculator"),
  tabsetPanel(
    tabPanel("Predefined Distributions",
             sidebarLayout(
               sidebarPanel(
                 selectInput("type","Type: ", choices = c("Continuous" = "cont", "Discrete" = "disc", "Custom" = "custom")),
                 uiOutput("dist"),
                 uiOutput("p1"),
                 uiOutput("p2"),
                 uiOutput("p3"),
                 selectInput("fun", "Function: ", choices = c("Probability function" = "prob", "Quantile function" = "quantile")),
                 uiOutput("prob"),
                 uiOutput("a"),
                 uiOutput("q"),
                 actionButton("plot_button", "Plot Distribution")
               ),
               mainPanel(
                 plotOutput("plot"),
                 verbatimTextOutput("area")
               )
             )
    ),
    tabPanel("Custom Distribution Viewer",
             sidebarLayout(
               sidebarPanel(
                 textInput("density_function", "Density Function (f(x)):"),
                 textInput("cdf_function", "Cumulative Distribution Function (F(x)):"),
                 textInput("hazard_function", "Hazard Function (h(x)):"),
                 numericInput("lower_bound", "Lower Bound:", value = 0),
                 numericInput("upper_bound", "Upper Bound:", value = 10),
                 actionButton("plot_button_custom", "Plot Distribution")
               ),
               mainPanel(
                 plotOutput("distribution_plot")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Persistence for user-defined distribution
  user_dist <- reactiveValues(density = NULL, cdf = NULL, hazard = NULL)
  
  # Render UI for user-defined distribution
  observeEvent(input$plot_button_custom, {
    output$distribution_plot <- renderPlot({
      # Check if all necessary inputs are provided
      if (is.null(input$density_function) ||
          is.null(input$cdf_function) ||
          is.null(input$hazard_function)) {
        return(NULL)
      }
      
      # Convert user input strings to functions
      f <- function(x) eval(parse(text = input$density_function))
      F <- function(x) eval(parse(text = input$cdf_function))
      h <- function(x) eval(parse(text = input$hazard_function))
      
      # Generate data for plotting
      x <- seq(input$lower_bound, input$upper_bound, length.out = 1000)
      density <- f(x)
      cdf <- F(x)
      hazard <- h(x)
      
      # Plotting
      p <- ggplot() +
        geom_line(aes(x, density, color = "Density")) +
        geom_line(aes(x, cdf, color = "CDF")) +
        geom_line(aes(x, hazard, color = "Hazard")) +
        labs(x = "x", y = "Value", title = "Custom Distribution") +
        scale_color_manual(values = c("Density" = "red", "CDF" = "blue", "Hazard" = "green")) +
        theme_minimal()
      
      print(p)
    })
    
    # Save user-defined distribution for persistence
    user_dist$density <- input$density_function
    user_dist$cdf <- input$cdf_function
    user_dist$hazard <- input$hazard_function
  })
  
  # Function to render UI elements based on distribution type
  output$dist <- renderUI({
    if (is.null(input$type)) return()
    switch(input$type,
           "cont" = selectInput("dist", "Distribution: ", choices = c(`Normal` = "norm", `t` = "t", `Chi-Squared` = "chisq", `F` = "f", `Uniform` = "unif", `Beta` = "beta", `Exponential` = "exp", `Gamma` = "gamma", `Weibull` = "weibull", `Skew Normal` = "sn"), selected = "norm"),
           "disc" = selectInput("dist", "Distribution: ", choices = c(`Binomial` = 'binom', `Poisson` = 'pois', `Geometric` = 'geom', `Negative Binomial` = 'nbinom', `Hypergeometric` = 'hyper'), selected = "binom"),
           "custom" = "Custom Distribution"
    )
  })
  
  # Function to handle plotting of predefined distributions
  observeEvent(input$plot_button, {
    # Your existing code for plotting predefined distributions
    x <- input$a
    if (input$type == "cont") {CplotGraph(x, lower = q.dist(0.00125), upper = q.dist(0.99875), header = input$dist)}
    if (input$type == "disc") {DplotGraph(x, lower = q.dist(0.00125), upper = q.dist(0.99875), header = input$dist)}
  })
  
  # Render UI for parameters based on selected distribution
  output$p1 = renderUI({
    if(input$fun == "prob"){
      if (is.null(input$type)) return()
      switch(input$type,
             "cont" = numericInput("p1","Mean: ", value = 0),
             "disc" = numericInput("p1", "Parameter 1: ", value = 1)
      )
    }
  })
  
  output$p2 = renderUI({
    if(input$fun == "prob"){
      if (is.null(input$type)) return()
      switch(input$type,
             "cont" = numericInput("p2","Standard Deviation: ", min = 0.1, value = 1, step=0.1),
             "disc" = numericInput("p2", "Parameter 2: ", value = 1)
      )
    }
  })
  
  output$p3 = renderUI({
    if(input$fun == "prob"){
      if (is.null(input$type)) return()
      switch(input$type,
             "cont" = NULL,
             "disc" = numericInput("p3", "Parameter 3: ", value = 1)
      )
    }
  })
  
  output$prob <- renderUI({
    if(input$fun == "prob"){
      if (is.null(input$type)) return()
      switch(input$type,
             "cont" = selectInput("prob", "Probability: ", choices = c(`< (less)` = "less", `> (greater)` = "greater"), selected = "less"),
             "disc" = selectInput("prob", "Probability: ", choices = c(`= (equal)` = "equal", `<= (at most)` = "less", `> (greater)` = "greater"), selected = "equal")
      )
    }
  })
  
  output$a = renderUI({
    if(input$fun == "prob"){
      numericInput("a","a:", value = round(q.dist(0.5),3), min = q.dist(0), max = q.dist(1), step = ifelse(input$type=="cont",0.01,1))
    }
  })
  
  output$q = renderUI({
    if(input$fun == "quantile"){
      numericInput("q","Quantile:", value = round(p.dist(0.5),3), min = p.dist(0), max = p.dist(1), step = ifelse(input$type=="cont",0.01,1))
    }
  })
  
  # Output for text area displaying calculated probability or quantile
  output$area <- renderText({
    # Probability function
    if(input$fun == "prob"){
      if (input$type == "cont") {
        if (input$prob == "less") {
          paste("P(", input$a, "< X) = ", round(p.dist(input$a), 4), sep = "")
        } else {
          paste("P(", input$a, "> X) = ", round(1 - p.dist(input$a), 4), sep = "")
        }
      }
      if (input$type == "disc") {
        if (input$prob == "equal") {
          paste("P(X = ", input$a, ") = ", round(p.dist(input$a), 4), sep = "")
        } else if (input$prob == "less") {
          paste("P(X <= ", input$a, ") = ", round(p.dist(input$a), 4), sep = "")
        } else {
          paste("P(X > ", input$a, ") = ", round(1 - p.dist(input$a - 1), 4), sep = "")
        }
      }
    }
    
    # Quantile function
    if(input$fun == "quantile"){
      paste("X(", input$q, ") = ", round(q.dist(input$q), 4), sep = "")
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
