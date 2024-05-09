library(ggplot2)
user_server <- function(input, output) {

  observeEvent(input$save_button, {
    # Extract inputs
    dist_name <- isolate(input$dist_name)
    pdf_function <- isolate(input$pdf_input)
    cdf_function <- isolate(input$cdf_input)
    lower_bound <- isolate(input$lower_bound)
    upper_bound <- isolate(input$upper_bound)


    new_distribution <- data.frame(name = dist_name, pdf = pdf_function, cdf = cdf_function, lower = lower_bound, upper = upper_bound)
    if (file.exists("custom_distributions.csv")) {
      custom_distributions <- read.csv("custom_distributions.csv")
      custom_distributions <- bind_rows(custom_distributions, new_distribution)
    } else {
      custom_distributions <- new_distribution
    }
    write.csv(custom_distributions, "custom_distributions.csv", row.names = FALSE)
    # if (file.exists("custom_distributions.rds")) {
    #   # Read existing data from RDS file
    #   custom_distributions <- readRDS("custom_distributions.rds")
    #   # Bind new data with existing data
    #   custom_distributions <- rbind(custom_distributions, new_distribution)
    # } else {
    #   # If no file exists, start with the new data
    #   custom_distributions <- new_distribution
    # }
    #
    # # Save the updated data frame to an RDS file
    # saveRDS(custom_distributions, "custom_distributions.rds")
    # output$status <- renderText("Data saved successfully.")
  })

  compute_hazard <- function(x, pdf_func, cdf_func) {
    pdf_val <- eval(parse(text = paste0("(", pdf_func, ")")))
    cdf_val <- eval(parse(text = paste0("(", cdf_func, ")")))
    hazard_val <- pdf_val / (1 - cdf_val)
    return(hazard_val)
  }

  plot_data <- reactive({
    input$save_button  # This ensures the code reacts to the button press
    if (input$pdf_input != "" && input$cdf_input != "" && !is.null(input$lower_bound) && !is.null(input$upper_bound)) {
      x <- seq(input$lower_bound, input$upper_bound, length.out = 100)
      pdf_vals <- eval(parse(text = input$pdf_input))
      cdf_vals <- eval(parse(text = input$cdf_input))
      hazard_vals <- pdf_vals / (1 - cdf_vals)
      data.frame(x, pdf_vals, cdf_vals, hazard_vals)
    } else {
      data.frame(x = numeric(0), pdf_vals = numeric(0), cdf_vals = numeric(0), hazard_vals = numeric(0))
    }
  })

  output$hazard_ui <- renderUI({
    if (input$dist_type == "continuous") {
      plotOutput("hazard_plot")
    }
  })

  probability <- reactive({
    # Define 'x' values over the appropriate range based on the comparison type
    if (input$dist_type == "continuous") {
      x_values <- if (input$comparison == "less") {
        seq(input$lower_bound, input$a_value, by = 0.01)  # Fine granularity for continuous
      } else if (input$comparison == "greater (>)") {
        seq(input$a_value, input$upper_bound, by = 0.01)
      } else if (input$comparison == "equals (=)") {
        input$a_value
      }

      # Evaluate the provided continuous function
      env <- new.env()
      env$x <- x_values
      function_call <- paste0("pdf_vals <- ", input$pdf_input)
      eval(parse(text = function_call), envir = env)

      # Calculate the total probability based on the comparison type
      if (input$comparison == "equals (=)") {
        # Directly return the probability of the single value
        env$pdf_vals
      } else {
        # Sum the probabilities for 'less than' or 'greater than'
        sum(env$pdf_vals)
      }
    } else {  # Discrete probability calculation
      x_values <- if (input$comparison == "at most(<=)") {
        seq(input$lower_bound, input$a_value, by = 1)
      } else if (input$comparison == "greater (>)") {
        seq(input$a_value + 1, input$upper_bound, by = 1)
      } else if (input$comparison == "equals (=)") {
        input$a_value
      }

      # Evaluate the provided discrete function
      env <- new.env()
      env$x <- x_values
      function_call <- paste0("pdf_vals <- ", input$pdf_input)
      eval(parse(text = function_call), envir = env)

      # Calculate the total probability based on the comparison type
      if (input$comparison == "equals (=)") {
        # Directly return the probability of the single value
        env$pdf_vals
      } else {
        # Sum the probabilities for 'less than' or 'greater than'
        sum(env$pdf_vals)
      }
    }
  })


  observeEvent(input$a_value, {  # Listen only to changes in a_value
    output$pdf_plot <- renderPlot({
      req(input$pdf_input, input$lower_bound, input$upper_bound)  # Ensure necessary inputs are provided
      # Generate data from the user-defined PDF function
      x_values <- seq(input$lower_bound, input$upper_bound, by = 0.01)
      env <- new.env()
      env$x <- x_values
      eval(parse(text = paste0("pdf_vals <- ", input$pdf_input)), envir = env)
      data <- data.frame(x = x_values, pdf_vals = env$pdf_vals)

      plot <- ggplot(data, aes(x = x, y = pdf_vals)) +
        geom_line(color = "blue") +
        theme_minimal()

      # Different handling for discrete and continuous distributions
      if (input$dist_type == "discrete") {
        discrete_data <- subset(data, x == floor(input$a_value))  # Consider discrete points

        # Calculate the probability for discrete distributions
        prob_value <- discrete_data$pdf_vals
        comparison_type <- input$comparison
        prob_label <- switch(
          comparison_type,
          "at most(<=)" = sum(env$pdf_vals[x_values <= input$a_value]),
          "greater (>)" = sum(env$pdf_vals[x_values > input$a_value]),
          "equals ( =)" = prob_value
        )

        prob_label <- format(round(prob_label, 4), nsmall = 4)
        prob_text <- paste("P(X", input$comparison, input$a_value, ") =", prob_label)

        plot <- plot +
          geom_point(data = discrete_data, aes(x = x, y = pdf_vals), color = "red", size = 3) +
          annotate("text", x = mean(range(data$x)), y = max(data$pdf_vals),
                   label = prob_text, vjust = 1.5, color = "red")
      } else {
        # Continuous handling
        area_data <- if (input$comparison == "less") {
          subset(data, x <= input$a_value)
        } else {
          subset(data, x >= input$a_value)
        }

        prob_value <- if (input$comparison == "less") {
          sum(env$pdf_vals[x_values <= input$a_value]) * 0.01  # Assuming small steps, this approximation is reasonable
        } else {
          sum(env$pdf_vals[x_values > input$a_value]) * 0.01
        }

        prob_label <- paste0("P(X ", ifelse(input$comparison == "less", "<=", ">"), " ", input$a_value, ") = ", format(round(prob_value, 4), nsmall = 4))

        plot <- plot +
          geom_area(data = area_data, aes(ymin = 0, ymax = pdf_vals), fill = "skyblue", alpha = 0.5) +
          geom_vline(xintercept = input$a_value, color = "red", linetype = "dashed") +
          annotate("text", x = mean(range(data$x)), y = max(data$pdf_vals),
                   label = prob_label, vjust = -0.5, hjust = 0.5, size = 5, color = "blue")
      }

      plot
    })
  })



  output$cdf_plot <- renderPlot({
    req(input$cdf_input, input$lower_bound, input$upper_bound)  # Ensure necessary inputs are provided
    # Generate data from the user-defined CDF function
    x_values <- seq(input$lower_bound, input$upper_bound, by = 0.01)  # Define a sequence of x values
    env <- new.env()
    env$x <- x_values
    eval(parse(text = paste0("cdf_vals <- ", input$cdf_input)), envir = env)  # Evaluate the CDF function
    data <- data.frame(x = x_values, cdf_vals = env$cdf_vals)

    plot <- ggplot(data, aes(x = x, y = cdf_vals)) +
      theme_minimal()

    if (input$dist_type == "continuous") {
      # Define area data based on the comparison
      area_data <- if (input$comparison == "less") {
        subset(data, x <= input$a_value)
      } else {
        subset(data, x >= input$a_value)
      }

      plot <- plot +
        geom_ribbon(data = area_data, aes(ymin = 0, ymax = cdf_vals), fill = "skyblue", alpha = 0.5) +
        geom_vline(xintercept = input$a_value, color = "red", linetype = "dashed") +
        annotate("text", x = mean(range(data$x)), y = max(data$cdf_vals) * 0.95, label = paste("CDF at X =", input$a_value, ":", round(env$cdf_vals[which(x_values == input$a_value)], 4)), hjust = 0.5, vjust = -1, size = 5, color = "blue")
    }

    plot + geom_line(color = "blue") +
      labs(title = "CDF", x = "x", y = "cdf")
  })


  output$hazard_plot <- renderPlot({
    data <- plot_data()
    if (nrow(data) > 0 && input$dist_type == "continuous") {
      plot <- ggplot(data, aes(x = x, y = hazard_vals)) +
        geom_line(color = "blue") +
        labs(title = "Hazard Function", x = "x", y = "hazard") +
        theme_minimal()

      if (input$dist_type == "continuous") {
        area_data <- if (input$comparison == "less") {
          subset(data, x <= input$a_value)
        } else {
          subset(data, x >= input$a_value)
        }
        plot <- plot +
          geom_ribbon(data = area_data, aes(ymin = 0, ymax = hazard_vals), fill = "skyblue", alpha = 0.5) +
          geom_vline(xintercept = input$a_value, color = "red", linetype = "dashed")
      }
      plot
    }
  })

}
