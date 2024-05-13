library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Custom Distribution Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("density_function", "Density Function (f(x)):"),
      textInput("cdf_function", "Cumulative Distribution Function (F(x)):"),
      textInput("hazard_function", "Hazard Function (h(x)):"),
      numericInput("lower_bound", "Lower Bound:", value = 0),
      numericInput("upper_bound", "Upper Bound:", value = 10),
      actionButton("plot_button", "Plot Distribution")
    ),
    
    mainPanel(
      plotOutput("distribution_plot")
    )
  )
)

# Server
server <- function(input, output) {
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
}

# Run the application
shinyApp(ui = ui, server = server)
