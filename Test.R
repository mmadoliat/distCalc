library(shiny)
library(ggplot2)

# Define functions for distributions
norm_density <- function(x, p1, p2) {
  dnorm(x, p1, p2)
}
norm_cdf <- function(x, p1, p2) {
  pnorm(x, p1, p2)
}
norm_hazard <- function(x, p1, p2) {
  norm_density(x, p1, p2) / (1 - norm_cdf(x, p1, p2))
}

# Define UI
ui <- fluidPage(
  titlePanel("Custom Distribution Viewer"),
  sidebarLayout(
    sidebarPanel(
      # Input fields for distribution parameters
      selectInput("distribution", "Select Distribution:",
                  choices = c("Normal", "t", "Chi-squared", "F", "Uniform", "Beta", "Exponential", "Gamma", "Weibull", "Binomial", "Poisson", "Geometric", "Negative Binomial", "Hypergeometric")),
      numericInput("p1", "Parameter 1:", value = 0),
      numericInput("p2", "Parameter 2:", value = 1),
      actionButton("plot_button", "Plot Distribution")
    ),
    mainPanel(
      plotOutput("distribution_plot")
    )
  )
)

# Define Server logic
server <- function(input, output) {
  output$distribution_plot <- renderPlot({
    # Switch statement for selected distribution
    dist <- switch(input$distribution,
                   "Normal" = list(density_func = norm_density, cdf_func = norm_cdf, hazard_func = norm_hazard),
                   "t" = list(density_func = t_density, cdf_func = t_cdf, hazard_func = t_hazard)
                   # Add cases for other distributions here
    )
    # Generate x values
    x <- seq(-5, 5, length.out = 1000)
    
    # Plot the selected distribution
    if (!is.null(dist)) {
      density_curve <- dist$density_func(x, input$p1, input$p2)
      cdf_curve <- dist$cdf_func(x, input$p1, input$p2)
      hazard_curve <- dist$hazard_func(x, input$p1, input$p2)
      
      df <- data.frame(x = x, Density = density_curve, CDF = cdf_curve, Hazard = hazard_curve)
      
      p <- ggplot(df, aes(x)) +
        geom_line(aes(y = Density, color = "Density")) +
        geom_line(aes(y = CDF, color = "CDF")) +
        geom_line(aes(y = Hazard, color = "Hazard")) +
        labs(x = "x", y = "Value", title = "Custom Distribution") +
        scale_color_manual(values = c("red", "blue", "green")) +
        theme_minimal()
      
      print(p)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
