library(dplyr)
createDistribution <- function(name, density_func, cdf_func, hazard_func, type) {
  list(
    name = name,
    density = density_func,
    cdf = cdf_func,
    hazard = hazard_func,
    type = type,
    evaluateDensity = function(x) eval(parse(text = density_func), envir = list(x = x)),
    evaluateCDF = function(x) eval(parse(text = cdf_func), envir = list(x = x)),
    evaluateHazard = function(x) ifelse(!is.na(hazard_func), eval(parse(text = hazard_func), envir = list(x = x)), NA),
    probabilityQuery = function(a, direction = "less") {
      cdf_value = eval(parse(text = cdf_func), envir = list(x = a))
      if (direction == "less") {
        return(cdf_value)
      } else if (direction == "greater") {
        return(1 - cdf_value)
      } else {
        stop("Invalid direction: choose 'less' or 'greater'")
      }
    },
    getInfo = function() {
      cat("Distribution Name:", name, "\n",
          "Type:", type, "\n",
          "Density Function:", density_func, "\n",
          "CDF Function:", cdf_func, "\n",
          "Hazard Function:", ifelse(is.na(hazard_func), "NA", hazard_func))
    }
  )
}

# Function to save a distribution object to a CSV file
# saveDistribution <- function(distribution, filepath) {
#   df <- data.frame(
#     name = distribution$name,
#     pdf = as.character(distribution$density),
#     cdf = as.character(distribution$cdf),
#     hazard = ifelse(is.na(distribution$hazard), "NA", as.character(distribution$hazard)),
#     lower = ifelse(is.na(distribution$lower), "NA", as.character(distribution$lower)),
#     upper = ifelse(is.na(distribution$upper), "NA", as.character(distribution$upper)),
#     type = distribution$type,
#     stringsAsFactors = FALSE
#   )
#   write.csv(df, filepath, row.names = FALSE)
# }
saveDistribution <- function(distribution, filepath) {
  dist_name <- distribution$name
  pdf_function <- distribution$pdf
  cdf_function <- distribution$cdf
  hazard_function <- ifelse(is.na(distribution$hazard), "NA", distribution$hazard)
  lower_bound <- ifelse(is.na(distribution$lower), "NA", as.numeric(distribution$lower))
  upper_bound <- ifelse(is.na(distribution$upper), "NA", as.numeric(distribution$upper))
  dist_type <- distribution$type  # Assuming 'type' is part of your distribution object

  new_distribution <- data.frame(
    name = dist_name,
    pdf = pdf_function,
    cdf = cdf_function,
    hazard = hazard_function,
    lower = lower_bound,
    upper = upper_bound,
    # type = dist_type,
    stringsAsFactors = FALSE
  )

  if (file.exists(filepath)) {
    custom_distributions <- read.csv(filepath)
    custom_distributions <- bind_rows(custom_distributions, new_distribution)
  } else {
    custom_distributions <- new_distribution
  }
  write.csv(custom_distributions, filepath, row.names = FALSE)
}
