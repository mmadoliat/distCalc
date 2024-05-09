library(dplyr)

get_custom_dist <- function(dist_name) {
  custom_distributions <- read.csv("~/R/distCalc/R/custom_distributions.csv")


  distribution <- custom_distributions %>%
    filter(name == dist_name)

  # Check if distribution is empty
  if(nrow(distribution) == 0) {
    return(list(pdf = NULL, cdf = NULL, hazard = NULL))
  }

  # Assuming pdf, cdf, and hazard are executable R code stored as strings
  return(list(
    pdf = as.character(distribution$pdf),
    cdf = as.character(distribution$cdf),
    hazard = as.character(distribution$hazard)
  ))
}

# Testing the function with case-insensitive name
test <- get_custom_dist('Healthcare')
print(test$pdf)
