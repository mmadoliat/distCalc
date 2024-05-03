chisq_density <- function(x, p1) {
  return(dchisq(x, p1))
}

chisq_cdf <- function(x, p1) {
  return(pchisq(x, p1))
}

chisq_hazard <- function(x, p1) {
  return(chisq_density(x, p1) / (1 - chisq_cdf(x, p1)))
}

