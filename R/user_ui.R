# R/app_ui.R
user_ui <- function() {
  fluidPage(
    titlePanel("Custom Distribution App"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("dist_type", "Distribution Type",
                     choices = list("Continuous" = "continuous", "Discrete" = "discrete")),
        textInput("dist_name", "Distribution Name:", ""),
        textAreaInput("pdf_input", "PDF Function:", ""),
        textAreaInput("cdf_input", "CDF Function:", ""),
        numericInput("lower_bound", "Lower Bound:", 0),
        numericInput("upper_bound", "Upper Bound:", 1),
        numericInput("a_value", "a:", value = 0),
        conditionalPanel(
          condition = "input.dist_type == 'continuous'",
          selectInput("comparison", "Probability:",
                      choices = c("Less than" = "less", "Greater than" = "greater"))
        ),
        conditionalPanel(
          condition = "input.dist_type == 'discrete'",
          selectInput("comparison", "Probability:",
                      choices = c("At most" = "at most(<=)", "Greater than" = "greater (>)", "Equal to" = "equals (=)"))
        ),
        actionButton("save_button", "Save Distribution")
      ),
      mainPanel(
        plotOutput("pdf_plot"),
        plotOutput("cdf_plot"),
        uiOutput("hazard_ui")
      )
    )
  )
}




