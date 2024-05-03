f_density <- function(x, p1, p2) {
  return(df(x, p1, p2))
}

f_cdf <- function(x, p1, p2) {
  return(pf(x, p1, p2))
}

f_hazard <- function(x, p1, p2) {
  return(f_density(x, p1, p2) / (1 - f_cdf(x, p1, p2)))
}

