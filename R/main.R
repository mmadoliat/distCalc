library(distcalc)
distCalc()
customCalc()

setwd("~/R/distcalc/R")
# Get the custom distribution named "Exponential Distribution"
custom_dist <- get_custom_dist("Donny")

# Access the PDF function
pdf_function <- custom_dist$pdf
print(pdf_function)

# Access the CDF function
cdf_function <- custom_dist$cdf
print(cdf_function)


# example_usage.R

# Source the distribution definitions
source("createDist.R")

# Create a Normal distribution object
normalDist <- createDistribution(
  name = "Normal Distribution",
  density_func = "dnorm(x, mean=0, sd=1)",
  cdf_func = "pnorm(x, mean=0, sd=1)",
  hazard_func = NA,
  type = "Continuous"
)
print(normalDist$getInfo())

# Evaluate the probability that X <= 1
print(normalDist$probabilityQuery(1, "less"))

# Evaluate the probability that X > 1
print(normalDist$probabilityQuery(1, "greater"))


# Save the distribution to a CSV file
saveDistribution(normalDist, "custom_distributions.csv")

x<- 3
p1<- 0.5
p2 <- 2
test<- weibull_cdf(x, p1, p2)
print(test)
