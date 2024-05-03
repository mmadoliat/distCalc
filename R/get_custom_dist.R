library(dplyr)

get_custom_dist <- function(dist_name) {
  custom_distributions <- read.csv("custom_distributions.csv")

  distribution <- custom_distributions %>%
    filter(name == dist_name)

  return(list(
    pdf = as.character(distribution$pdf),
    cdf = as.character(distribution$cdf),
    hazard = as.character(distribution$hazard)
  ))
}


test <- get_custom_dist('healthcare')
print(test$pdf)


